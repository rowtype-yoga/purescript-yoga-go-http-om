package main

import (
	"log"
	"net/http"
	"strconv"
)

var aliceBody = []byte(`{"name":"Alice","id":42}`)

func getUser(w http.ResponseWriter, r *http.Request) {
	id, err := strconv.Atoi(r.PathValue("id"))
	h := w.Header()
	h["Content-Type"] = []string{"application/json; charset=utf-8"}
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte(`{"error":"invalid id"}`))
		return
	}
	if id == 42 {
		w.Write(aliceBody)
		return
	}
	buf := make([]byte, 0, 32)
	buf = append(buf, `{"error":"no user `...)
	buf = strconv.AppendInt(buf, int64(id), 10)
	buf = append(buf, `"}`...)
	w.WriteHeader(http.StatusNotFound)
	w.Write(buf)
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /users/{id}", getUser)
	log.Fatal(http.ListenAndServe("127.0.0.1:18082", mux))
}
