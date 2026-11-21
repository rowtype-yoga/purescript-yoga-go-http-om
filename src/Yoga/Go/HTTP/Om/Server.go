package ffi

import (
	"context"
	"io"
	"net"
	"net/http"
	"net/url"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/i-am-the-slime/iris-go/eventloop"
	"github.com/i-am-the-slime/iris-go/exception"
)

// Server is the route table and, once listening, the Go http.Server.
type Server = *server

// RequestHandle is one request a route matched, with its response writer.
type RequestHandle = *handle

type BoundAddress struct {
	Address string
	Port    int32
}

type server struct {
	routes         []route
	maxBodyBytes   int64
	headersTimeout time.Duration
	requestTimeout time.Duration
	keepAlive      time.Duration
	http           *http.Server
	retire         func()
	stopped        sync.Once
}

type route struct {
	method    string
	pattern   string
	bodyKind  string
	segments  []segment
	literals  int
	signature string
	handler   func(*handle) func(func())
}

type segment struct {
	literal string
	capture string
}

type handle struct {
	writer       http.ResponseWriter
	pathNames    []string
	pathValues   []string
	queryNames   []string
	queryValues  [][]string
	headerNames  []string
	headerValues []string
	cookieNames  []string
	cookieValues []string
	formNames    []string
	formValues   [][]string
	kind         string
	body         string
	bodyPresent  bool
	sent         bool
}

func CreateServerImpl(maxBodyBytes int32, headersMilliseconds, requestMilliseconds, keepAliveMilliseconds float64) *server {
	return &server{
		maxBodyBytes:   int64(maxBodyBytes),
		headersTimeout: milliseconds(headersMilliseconds),
		requestTimeout: milliseconds(requestMilliseconds),
		keepAlive:      milliseconds(keepAliveMilliseconds),
	}
}

func AddRouteImpl(method string, pattern string, bodyKind string, handler func(*handle) func(func()), s *server) {
	segments := compile(pattern)
	added := route{method: strings.ToUpper(method), pattern: pattern, bodyKind: bodyKind, segments: segments, handler: handler}
	for _, part := range segments {
		if part.capture == "" {
			added.literals++
		}
	}
	added.signature = signature(segments)
	for _, existing := range s.routes {
		if existing.method == added.method && existing.signature == added.signature {
			panic(exception.NewError("Duplicate route shape: " + added.method + " " + pattern))
		}
	}
	s.routes = append(s.routes, added)
}

// ListenImpl binds the address and serves in the background. The server keeps the program alive
// until CloseImpl.
func ListenImpl[R any](failed func(string) R, bound func(BoundAddress) R, host string, port int32, s *server) R {
	listener, err := net.Listen("tcp", net.JoinHostPort(host, strconv.Itoa(int(port))))
	if err != nil {
		return failed(err.Error())
	}
	s.http = &http.Server{
		Handler:           s,
		ReadHeaderTimeout: s.headersTimeout,
		ReadTimeout:       s.requestTimeout,
		IdleTimeout:       s.keepAlive,
	}
	s.retire = eventloop.Hold()
	go s.http.Serve(listener)
	address := listener.Addr().(*net.TCPAddr)
	return bound(BoundAddress{Address: address.IP.String(), Port: int32(address.Port)})
}

// CloseImpl stops accepting connections, waits for the requests in flight, and then resumes the
// fiber.
func CloseImpl(s *server, finished func()) {
	retire := eventloop.Hold()
	go func() {
		if s.http != nil {
			s.http.Shutdown(context.Background())
		}
		finished()
		s.stopped.Do(func() {
			if s.retire != nil {
				s.retire()
			}
		})
		retire()
	}()
}

func PathNamesImpl(h *handle) []string      { return h.pathNames }
func PathValuesImpl(h *handle) []string     { return h.pathValues }
func QueryNamesImpl(h *handle) []string     { return h.queryNames }
func QueryValuesImpl(h *handle) [][]string  { return h.queryValues }
func HeaderNamesImpl(h *handle) []string    { return h.headerNames }
func HeaderValuesImpl(h *handle) []string   { return h.headerValues }
func CookieNamesImpl(h *handle) []string    { return h.cookieNames }
func CookieValuesImpl(h *handle) []string   { return h.cookieValues }
func FormNamesImpl(h *handle) []string      { return h.formNames }
func FormValuesImpl(h *handle) [][]string   { return h.formValues }
func BodyKindImpl(h *handle) string         { return h.kind }
func BodyImpl(h *handle) string             { return h.body }
func BodyPresentImpl(h *handle) bool        { return h.bodyPresent }

// SendResponseImpl writes the status, headers and body. A body is written only when hasBody and
// the status allows one; defaultType is the content type when the headers name none.
func SendResponseImpl(status int32, names []string, values []string, hasBody bool, body string, defaultType string, h *handle) {
	if h.sent {
		return
	}
	h.sent = true
	header := h.writer.Header()
	typed := false
	for index, name := range names {
		header.Set(name, values[index])
		if strings.EqualFold(name, "content-type") {
			typed = true
		}
	}
	forbidsBody := status < 200 || status == 204 || status == 304
	if forbidsBody || !hasBody {
		h.writer.WriteHeader(int(status))
		return
	}
	if !typed {
		header.Set("Content-Type", defaultType)
	}
	h.writer.WriteHeader(int(status))
	io.WriteString(h.writer, body)
}

// SendFailureImpl answers 500 unless a response was already sent.
func SendFailureImpl(h *handle) {
	if !h.sent {
		h.sent = true
		plain(h.writer, http.StatusInternalServerError, "Internal Server Error")
	}
}

func (s *server) ServeHTTP(writer http.ResponseWriter, request *http.Request) {
	path, err := decodePath(request.URL.EscapedPath())
	if err != nil {
		plain(writer, http.StatusBadRequest, "Malformed percent-encoding in request path")
		return
	}
	var matches []match
	for _, candidate := range s.routes {
		if values, ok := candidate.match(path); ok {
			matches = append(matches, match{route: candidate, values: values})
		}
	}
	if len(matches) == 0 {
		plain(writer, http.StatusNotFound, "Not Found")
		return
	}
	selected, ok := selectMethod(matches, strings.ToUpper(request.Method))
	if !ok {
		writer.Header().Set("Allow", allowed(matches))
		plain(writer, http.StatusMethodNotAllowed, "Method Not Allowed")
		return
	}
	h := &handle{writer: writer, kind: selected.route.bodyKind}
	for index, part := range selected.route.segments {
		if part.capture != "" {
			h.pathNames = append(h.pathNames, part.capture)
			h.pathValues = append(h.pathValues, selected.values[index])
		}
	}
	h.queryNames, h.queryValues = sortedValues(request.URL.Query())
	h.headerNames, h.headerValues = headers(request)
	h.cookieNames, h.cookieValues = cookies(request.Header.Get("Cookie"))
	if status, message := s.readBody(selected.route.bodyKind, request, h); status != 0 {
		plain(writer, status, message)
		return
	}
	done := make(chan struct{})
	selected.route.handler(h)(func() { close(done) })
	<-done
	if !h.sent {
		plain(writer, http.StatusInternalServerError, "Internal Server Error")
	}
}

// readBody fills the handle's body for the route's body kind, or returns the status and message
// to refuse the request with.
func (s *server) readBody(kind string, request *http.Request, h *handle) (int, string) {
	if kind == "none" {
		return 0, ""
	}
	if request.ContentLength > s.maxBodyBytes {
		return http.StatusRequestEntityTooLarge, "Payload Too Large"
	}
	data, err := io.ReadAll(io.LimitReader(request.Body, s.maxBodyBytes+1))
	if err != nil {
		return http.StatusBadRequest, err.Error()
	}
	if int64(len(data)) > s.maxBodyBytes {
		return http.StatusRequestEntityTooLarge, "Payload Too Large"
	}
	h.bodyPresent = len(data) > 0
	if !h.bodyPresent {
		return 0, ""
	}
	mediaType := strings.ToLower(strings.TrimSpace(strings.SplitN(request.Header.Get("Content-Type"), ";", 2)[0]))
	switch kind {
	case "text":
		if mediaType != "text/plain" {
			return http.StatusUnsupportedMediaType, "Expected text/plain request body"
		}
	case "json":
		if mediaType != "application/json" && !strings.HasSuffix(mediaType, "+json") {
			return http.StatusUnsupportedMediaType, "Expected application/json request body"
		}
	case "form":
		if mediaType != "application/x-www-form-urlencoded" {
			return http.StatusUnsupportedMediaType, "Expected application/x-www-form-urlencoded request body"
		}
		form, err := url.ParseQuery(string(data))
		if err != nil {
			return http.StatusBadRequest, err.Error()
		}
		h.formNames, h.formValues = sortedValues(form)
	}
	h.body = string(data)
	return 0, ""
}

type match struct {
	route  route
	values []string
}

func (r route) match(path []string) ([]string, bool) {
	if len(r.segments) != len(path) {
		return nil, false
	}
	for index, part := range r.segments {
		if part.capture == "" && part.literal != path[index] {
			return nil, false
		}
	}
	return path, true
}

func selectMethod(matches []match, method string) (match, bool) {
	var best match
	found := false
	for _, candidate := range matches {
		if candidate.route.method == method && (!found || candidate.route.literals > best.route.literals) {
			best, found = candidate, true
		}
	}
	return best, found
}

func allowed(matches []match) string {
	seen := map[string]bool{}
	var methods []string
	for _, candidate := range matches {
		if !seen[candidate.route.method] {
			seen[candidate.route.method] = true
			methods = append(methods, candidate.route.method)
		}
	}
	sort.Strings(methods)
	return strings.Join(methods, ", ")
}

func compile(pattern string) []segment {
	var segments []segment
	for _, part := range split(pattern) {
		if strings.HasPrefix(part, ":") {
			segments = append(segments, segment{capture: part[1:]})
		} else {
			segments = append(segments, segment{literal: part})
		}
	}
	return segments
}

func signature(segments []segment) string {
	parts := make([]string, len(segments))
	for index, part := range segments {
		if part.capture != "" {
			parts[index] = ":"
		} else {
			parts[index] = part.literal
		}
	}
	return strings.Join(parts, "/")
}

func split(path string) []string {
	if path == "/" || path == "" {
		return nil
	}
	return strings.Split(strings.TrimPrefix(path, "/"), "/")
}

func decodePath(escaped string) ([]string, error) {
	parts := split(escaped)
	for index, part := range parts {
		decoded, err := url.PathUnescape(part)
		if err != nil {
			return nil, err
		}
		parts[index] = decoded
	}
	return parts, nil
}

func sortedValues(values url.Values) ([]string, [][]string) {
	names := make([]string, 0, len(values))
	for name := range values {
		names = append(names, name)
	}
	sort.Strings(names)
	lists := make([][]string, len(names))
	for index, name := range names {
		lists[index] = values[name]
	}
	return names, lists
}

func headers(request *http.Request) ([]string, []string) {
	names := []string{"host"}
	values := []string{request.Host}
	for name, list := range request.Header {
		names = append(names, strings.ToLower(name))
		values = append(values, strings.Join(list, ", "))
	}
	return names, values
}

func cookies(header string) ([]string, []string) {
	var names, values []string
	if header == "" {
		return names, values
	}
	for _, cookie := range strings.Split(header, ";") {
		separator := strings.Index(cookie, "=")
		if separator < 0 {
			continue
		}
		name := strings.TrimSpace(cookie[:separator])
		if name == "" {
			continue
		}
		encoded := strings.TrimSpace(cookie[separator+1:])
		value, err := url.PathUnescape(encoded)
		if err != nil {
			value = encoded
		}
		names = append(names, name)
		values = append(values, value)
	}
	return names, values
}

func plain(writer http.ResponseWriter, status int, message string) {
	writer.Header().Set("Content-Type", "text/plain; charset=utf-8")
	writer.WriteHeader(status)
	io.WriteString(writer, message)
}

func milliseconds(value float64) time.Duration {
	return time.Duration(value * float64(time.Millisecond))
}
