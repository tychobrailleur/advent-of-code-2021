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

(defn abs
  "Returns the absolute value of I."
  [i]
  (if (neg? i) (- i) i))

(defn move-towards-zero
  "Makes an integer I converge towards 0.  When it reaches 0, it remains constant."
  [i]
  (cond
    (= i 0) 0
    (neg? i) (inc i)
    :else (dec i)))

(defn vec-contains?
  "Returns true if vector V contains element ELT."
  [v elt]
  (some #(= elt %) v))

(defn tee
  "Prints N along with a specific messsage, and returns N, for using in threading."
  [n msg]
  (println msg n)
  n)

(defn remove-every-nth-char
  "Removes every Nth character in a string S."
  [s n]
  (str/join (keep-indexed (fn [idx elt] (when (or (= idx 0) (not (= 0 (mod idx n)))) elt)) s)))

(defn manhattan-distance
  "Manhattan distance between START and FINISH."
  [start finish]
  (let [[x1 y1] start
        [x2 y2] finish]
    (+ (abs (- x1 x2))
       (abs (- y1 y2)))))
