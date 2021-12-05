(ns advent-of-code-2021.util)

(defn transpose
  "Transposes a list of lists."
  [l]
  (apply map list l))

(defn to-int
  "Converts a string representing an integer into the int itself."
  [s]
  (Integer/parseInt s))
