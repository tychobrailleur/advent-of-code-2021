(ns advent-of-code-2021.day03
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(defn string->bits [line]
  (->> (str/split line #"")
       (map #(Integer/parseInt %))))


(defn most-common-bit [bits]
  (let [{zero 0 one 1} (frequencies bits)]
    (if (> zero one) 0 1)))

(defn least-common-bit [bits]
  (let [{zero 0 one 1} (frequencies bits)]
    (if (<= zero one) 0 1)))

(defn complement-bits [l]
  (map #(if (= % 1) 0 1) l))

(defn get-gamma [content]
  (->> content
       (map string->bits)
       util/transpose
       (map most-common-bit)))

(defn compute-result [content]
  (let [lines (str/split content #"\n")
        gamma (get-gamma lines)
        epsilon (complement-bits gamma)]
    (println (* (Integer/parseInt (str/join gamma) 2)
                (Integer/parseInt (str/join epsilon) 2)))))

(defn filter-discriminant [bits pos discrimant]
  (let [tr (util/transpose bits)
        reference (nth tr pos) ;; find the row at pos in the transposed list
        ref-bit (discrimant reference)] ;; in that row find bit with discrimant function
    (filter #(= ref-bit (nth % pos)) bits))) ;; filter entries that have that bit in position pos

(defn filter-most-common [bits pos]
  (let [entries (filter-discriminant bits pos most-common-bit)]
    (if (= 1 (count entries))
      (first entries)
      (filter-most-common entries (inc pos)))))

(defn filter-least-common [bits pos]
  (let [entries (filter-discriminant bits pos least-common-bit)]
    (if (= 1 (count entries))
      (first entries)
      (filter-least-common entries (inc pos)))))

(defn compute-result-2 [content]
  (let [lines (map string->bits (str/split content #"\n"))
        oxygen (filter-most-common lines 0)
        co2 (filter-least-common lines 0)]
    (println (* (Integer/parseInt (str/join oxygen) 2)
                (Integer/parseInt (str/join co2) 2)))))

(defn -main [& args]
  (let [content (slurp "resources/day03.txt")]
    (compute-result content)
    (compute-result-2 content)))
