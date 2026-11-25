// The inventory API written by hand with net/http and encoding/json, for comparison: the same
// routes, the same checks, the same answers.
package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"slices"
	"strconv"
	"strings"
	"sync"
)

type Product struct {
	Sku   string `json:"sku"`
	Name  string `json:"name"`
	Stock int    `json:"stock"`
}

type problem struct {
	Error string `json:"error"`
}

type inventory struct {
	mutex    sync.Mutex
	products map[string]Product
}

func main() {
	shelf := &inventory{products: map[string]Product{}}
	mux := http.NewServeMux()
	mux.HandleFunc("POST /products", shelf.addProduct)
	mux.HandleFunc("GET /products/{sku}", shelf.getProduct)
	mux.HandleFunc("GET /products", shelf.lowStock)
	mux.HandleFunc("POST /products/{sku}/sell", shelf.sell)
	fmt.Println("inventory on http://127.0.0.1:8071")
	log.Fatal(http.ListenAndServe("127.0.0.1:8071", mux))
}

func (shelf *inventory) addProduct(w http.ResponseWriter, r *http.Request) {
	if !isJSON(r) {
		http.Error(w, "Expected application/json request body", http.StatusUnsupportedMediaType)
		return
	}
	var body struct {
		Sku   *string `json:"sku"`
		Name  *string `json:"name"`
		Stock *int    `json:"stock"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil || body.Sku == nil || body.Name == nil || body.Stock == nil {
		writeJSON(w, http.StatusBadRequest, problem{"Invalid request"})
		return
	}
	product, err := validProduct(*body.Sku, *body.Name, *body.Stock)
	if err != nil {
		writeJSON(w, http.StatusBadRequest, problem{err.Error()})
		return
	}
	shelf.mutex.Lock()
	_, taken := shelf.products[product.Sku]
	if !taken {
		shelf.products[product.Sku] = product
	}
	shelf.mutex.Unlock()
	if taken {
		writeJSON(w, http.StatusConflict, problem{product.Sku + " is already on the shelf"})
		return
	}
	writeJSON(w, http.StatusCreated, product)
}

func (shelf *inventory) getProduct(w http.ResponseWriter, r *http.Request) {
	sku, err := parseSku(r.PathValue("sku"))
	if err != nil {
		writeJSON(w, http.StatusBadRequest, problem{err.Error()})
		return
	}
	shelf.mutex.Lock()
	product, found := shelf.products[sku]
	shelf.mutex.Unlock()
	if !found {
		writeJSON(w, http.StatusNotFound, problem{"no product " + sku})
		return
	}
	writeJSON(w, http.StatusOK, product)
}

func (shelf *inventory) lowStock(w http.ResponseWriter, r *http.Request) {
	below, err := strconv.Atoi(r.URL.Query().Get("below"))
	hasLimit := err == nil
	shelf.mutex.Lock()
	products := make([]Product, 0, len(shelf.products))
	for _, product := range shelf.products {
		if !hasLimit || product.Stock < below {
			products = append(products, product)
		}
	}
	shelf.mutex.Unlock()
	slices.SortFunc(products, func(a, b Product) int { return strings.Compare(a.Sku, b.Sku) })
	writeJSON(w, http.StatusOK, products)
}

func (shelf *inventory) sell(w http.ResponseWriter, r *http.Request) {
	sku, err := parseSku(r.PathValue("sku"))
	if err != nil {
		writeJSON(w, http.StatusBadRequest, problem{err.Error()})
		return
	}
	if !isJSON(r) {
		http.Error(w, "Expected application/json request body", http.StatusUnsupportedMediaType)
		return
	}
	var body struct {
		Quantity *int `json:"quantity"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil || body.Quantity == nil {
		writeJSON(w, http.StatusBadRequest, problem{"Invalid request"})
		return
	}
	quantity, err := parseStock(*body.Quantity)
	if err != nil {
		writeJSON(w, http.StatusBadRequest, problem{err.Error()})
		return
	}
	shelf.mutex.Lock()
	product, found := shelf.products[sku]
	enough := found && product.Stock >= quantity
	if enough {
		product.Stock -= quantity
		shelf.products[sku] = product
	}
	shelf.mutex.Unlock()
	switch {
	case !found:
		writeJSON(w, http.StatusNotFound, problem{"no product " + sku})
	case !enough:
		writeJSON(w, http.StatusConflict, problem{fmt.Sprintf("only %d left", product.Stock)})
	default:
		writeJSON(w, http.StatusOK, product)
	}
}

func validProduct(sku string, name string, stock int) (Product, error) {
	validSku, err := parseSku(sku)
	if err != nil {
		return Product{}, err
	}
	validName, err := parseName(name)
	if err != nil {
		return Product{}, err
	}
	validStock, err := parseStock(stock)
	if err != nil {
		return Product{}, err
	}
	return Product{Sku: validSku, Name: validName, Stock: validStock}, nil
}

func parseSku(text string) (string, error) {
	looksLikeASku := len(text) >= 3 && len(text) <= 16
	for _, character := range text {
		skuCharacter := (character >= 'A' && character <= 'Z') || (character >= '0' && character <= '9') || character == '-'
		looksLikeASku = looksLikeASku && skuCharacter
	}
	if !looksLikeASku {
		return "", fmt.Errorf("a SKU is 3 to 16 of A-Z, 0-9 and -, not %q", text)
	}
	return text, nil
}

func parseName(text string) (string, error) {
	trimmed := strings.TrimSpace(text)
	if trimmed == "" {
		return "", fmt.Errorf("a product needs a name")
	}
	return trimmed, nil
}

func parseStock(count int) (int, error) {
	if count < 0 {
		return 0, fmt.Errorf("a quantity can't be negative, got %d", count)
	}
	return count, nil
}

func isJSON(r *http.Request) bool {
	return strings.HasPrefix(r.Header.Get("Content-Type"), "application/json")
}

func writeJSON(w http.ResponseWriter, status int, value any) {
	body, err := json.Marshal(value)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(status)
	w.Write(body)
}
