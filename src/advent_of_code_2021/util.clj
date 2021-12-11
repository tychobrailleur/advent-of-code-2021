(ns advent-of-code-2021.util
  (:require [clojure.string :as str]))

(defn transpose
  "Transposes a list of lists."
  [l]
  (apply map list l))

(defn to-int
  "Converts a string representing an integer into the int itself."
  [s]
  (Integer/parseInt s))

(defn text->int
  "Converts a line of comma-separated numbers into a list of integers."
  ([line]
   (map to-int (str/split (str/trim line) #",")))
  ([line sep]
   (map to-int (str/split (str/trim line) sep))))

(defn abs [i]
  (if (neg? i) (- i) i))

(defn vec-contains?
  "Returns true if vector V contains element ELT."
  [v elt]
  (some #(= elt %) v))

(defn tee
  "Prints N along with a specific messsage, and returns N, for using in threading."
  [n msg]
  (println msg n)
  n)
