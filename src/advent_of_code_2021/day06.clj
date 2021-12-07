(ns advent-of-code-2021.day06
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn process-input [input]
  (loop [i input
         nexts []]
    (if (= 0 (first i))
      (if (empty? (rest i))
        (conj (conj nexts 6) 8)
        (recur (rest i) (conj (conj nexts 6) 8)))
      (if (empty? (rest i))
        (conj nexts (dec (first i)))
        (recur (rest i) (conj nexts (dec (first i))))))))

(defn process-n-generations [input n]
  (loop [stage 1
         output input]
    (if (= stage n)
      (process-input output)
      (recur (inc stage) (process-input output)))))

(defn process-large-input [input]
  (let [{zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8} input]
    {8 zero
     0 one
     1 two
     2 three
     3 four
     4 five
     5 six
     6 (+ zero seven)
     7 eight}))

(defn total-values [input]
  (reduce + 0 (vals input)))

(defn process-large-generations [input n]
  (loop [stage 1
         output (merge {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0} (frequencies input))]
    (if (= stage n)
      (process-large-input output)
      (recur (inc stage) (process-large-input output)))))


(defn -main [& args]
  (let [content (slurp "resources/day06.txt")]
    (-> content
        str/trim
        util/text->int
        (process-n-generations 80)
        count
        println)
    (-> content
        str/trim
        util/text->int
        (process-large-generations 256)
        total-values
        println)))
