(ns advent-of-code-2021.day10
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util])
  (:import [java.util ArrayDeque]))

(defn stack-empty? [^ArrayDeque stack]
  (= 0 (.size stack)))

(def pairs (hash-map \( \) \{ \} \< \> \[ \]))

(defn pair-matches? [c1 c2]
  (or (= (get pairs c1) c2)
      (= (get pairs c2) c1)))

(defn parse-line [line]
  (loop [chars (.toCharArray line)
        stack (ArrayDeque.)]
    (if (empty? chars)
      (if (not (stack-empty? stack))
        {:state :incomplete :unmatched stack}
        {:state :correct})
      (let [next-char (first chars)]
        (if (util/vec-contains? [\( \{ \< \[] next-char)
          (do (.push stack next-char)
              (recur (rest chars) stack))
          (if (stack-empty? stack)
            {:state :corrupted :last-char next-char}
            (if (pair-matches? (.pop stack) next-char)
              (recur (rest chars) stack)
              {:state :corrupted :last-char next-char})))))))

(def scores (hash-map \) 3 \] 57 \} 1197 \> 25137))
(def scores-incomplete (hash-map \) 1 \] 2 \} 3 \> 4))

(defn score-code [c]
  (get scores (:last-char c) 0))

(defn score-incomplete-code [c]
  (reduce #(+ (* %1 5) (get scores-incomplete %2)) 0 c))

(defn overall-incomplete-code-score [c]
  (nth (sort c) (/ (count c) 2)))

(defn match-unmatched [stack]
  (loop [s stack
         matching []]
    (if (stack-empty? s)
      matching
      (let [ch (.pop s)]
        (recur s (conj matching (get pairs ch)))))))

(defn -main[& args]
  (let [lines (str/split (slurp "resources/day10.txt") #"\n")]
    (->> lines
         (map parse-line)
         (filter #(= (:state %) :corrupted))
         (map score-code)
         (reduce + 0)
         println)
    (->> lines
         (map parse-line)
         (filter #(= (:state %) :incomplete))
         (map #(match-unmatched (:unmatched %)))
         (map score-incomplete-code)
         overall-incomplete-code-score
         println)))
