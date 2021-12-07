(ns advent-of-code-2021.day07
  (:require [advent-of-code-2021.util :as util]))


(defn build-crab-fleet [list]
  (let [freq (frequencies list)
        positions (sort (keys freq))]
    (vec (map #(vector % (get freq %)) positions))))

(defn move-crabs-to-line
  "Calculates the total number of steps for the current crab group to get to position POS.
  The cost per step is given by the function COST-FN

  A crab group is of the form [current-position number-of-crabs].
  "
  [crabs pos cost-fn]
  (let [[p n] crabs]
    (* n (cost-fn (util/abs (- pos p))))))

(defn simple-step-cost
  "Cost of the move is the number of steps itself."
  [n]
  n)

(defn step-increasing-cost
  "Cost of the move is sum of integers from 1 to N (arithmetic progression)."
  [n]
  (/ (* n (+ 1 n)) 2))

(defn move-fleet-to-line [fleet pos cost-fn]
  (reduce #(+ %1 (move-crabs-to-line %2 pos cost-fn)) 0 fleet))

(defn find-cheapest-move [list cost-fn]
  (let [max-pos (apply max list)
        fleet (build-crab-fleet list)]
    (loop [min-fuel Integer/MAX_VALUE
           pos 0]
      (if (= pos (inc max-pos))
        min-fuel
        (recur (min min-fuel (move-fleet-to-line fleet pos cost-fn))
               (inc pos))))))

(defn -main [& args]
  (let [content (util/text->int (slurp "resources/day07.txt"))]
    (println (find-cheapest-move content simple-step-cost))
    (println (find-cheapest-move content step-increasing-cost))))
