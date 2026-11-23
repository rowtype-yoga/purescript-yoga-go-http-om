package main

import (
	"encoding/json"
	"log"
	"net/http"
	"strconv"
)

type user struct {
	Name string `json:"name"`
	ID   int    `json:"id"`
}

type apiError struct {
	Error string `json:"error"`
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	body, err := json.Marshal(v)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(status)
	w.Write(body)
}

func getUser(w http.ResponseWriter, r *http.Request) {
	id, err := strconv.Atoi(r.PathValue("id"))
	if err != nil {
		writeJSON(w, http.StatusBadRequest, apiError{Error: "invalid id"})
		return
	}
	if id == 42 {
		writeJSON(w, http.StatusOK, user{ID: 42, Name: "Alice"})
		return
	}
	writeJSON(w, http.StatusNotFound, apiError{Error: "no user " + strconv.Itoa(id)})
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /users/{id}", getUser)
	log.Fatal(http.ListenAndServe("127.0.0.1:18081", mux))
}
