(ns advent-of-code-2021.day08
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn parse-line [line]
  (let [[digits code] (str/split line #"\|")
        code-digits (str/split (str/trim code) #" ")]
    code-digits))

(defn count-uniques [code-digits]
  (count (filter #(util/vec-contains? [2 3 4 7] (count %)) code-digits)))

(defn find-all-strings-of-length [digits length]
  (filter #(= (count %) length) digits))

(defn find-string-of-length [digits length]
  (some #(when (= (count %) length) %) digits))

(defn find-all-letters-in-but-not-in [a b]
  (vec (clojure.set/difference (set (char-array a)) (set (char-array b)))))

(defn find-letter-in-but-not-in [a b]
  (first (find-all-letters-in-but-not-in a b)))

(defn sort-chars-in-string [s]
  (-> s
      sort
      str/join))

(defn find-other-digits [digits mapping]
  (let [one (get mapping 1)
        four (get mapping 4)
        seven (get mapping 7)
        top-letter (find-letter-in-but-not-in seven one)
        top-left-and-middle (find-all-letters-in-but-not-in four one)
        potential-zeroes (find-all-strings-of-length digits 6)
        potential-threes (find-all-strings-of-length digits 5)
        ;; zero is missing the middle bar, which means it does not have top left or middle from four.
        zero (first (filter #(and (str/index-of % top-letter)
                                          (or (not (str/index-of % (first top-left-and-middle)))
                                              (not (str/index-of % (second top-left-and-middle)))))
                            potential-zeroes))
        ;; three is the only five-character number with all of one's characters
        three (first (filter #(and (str/index-of % (first (char-array one)))
                                   (str/index-of % (second (char-array one)))) potential-threes))
        ;; two is not three, and is missing the top left bar
        two (first (filter #(and (not= three %)
                                 (or (not (str/index-of % (first top-left-and-middle)))
                                     (not (str/index-of % (second top-left-and-middle)))))
                           potential-threes))
        ;; five is the last five-character number
        five (some #(when (not (or (= three %) (= two %))) %) potential-threes)
        ;; nine is not zero, and had all the characters from one.
        nine (first (filter #(and (not= % zero)
                                  (and (str/index-of % (first (char-array one)))
                                       (str/index-of % (second (char-array one))))) potential-zeroes))
        ;; six is the last six-character number
        six (some #(when (and (not= % zero) (not= % nine)) %) potential-zeroes)]
    (-> mapping
        ;; letters can be listed in any order, so sort them.
        (assoc 0 (sort-chars-in-string zero))
        (assoc 2 (sort-chars-in-string two))
        (assoc 3 (sort-chars-in-string three))
        (assoc 5 (sort-chars-in-string five))
        (assoc 6 (sort-chars-in-string six))
        (assoc 9 (sort-chars-in-string nine))
        clojure.set/map-invert)))

(defn deduce-mapping [digits]
  (let [mapping {1 (sort-chars-in-string (find-string-of-length digits 2))
                 4 (sort-chars-in-string (find-string-of-length digits 4))
                 7 (sort-chars-in-string (find-string-of-length digits 3))
                 8 (sort-chars-in-string (find-string-of-length digits 7))}]
    (find-other-digits (vec (clojure.set/difference (set digits) (vals mapping))) mapping)))

(defn decode-number [mapping code]
  (let [[_ th hu te un] (re-matches #"(\w+) (\w+) (\w+) (\w+)" code)]
    (+ (* (get mapping (sort-chars-in-string th)) 1000)
       (* (get mapping (sort-chars-in-string hu)) 100)
       (* (get mapping (sort-chars-in-string te)) 10)
       (get mapping (sort-chars-in-string un)))))

(defn compute-code-for-line [line]
  (let [[digits code] (map str/trim (str/split line #"\|"))]
    (-> digits
        (str/split #" ")
        vec
        deduce-mapping
        (decode-number code))))

(defn -main [& args]
  (let [lines (str/split (slurp "resources/day08.txt") #"\n")]
    (->> lines
         (map #(parse-line %))
         (map #(count-uniques %))
         (reduce + 0)
         println)
    (->> lines
         (map compute-code-for-line)
         (reduce + 0)
         println)))
