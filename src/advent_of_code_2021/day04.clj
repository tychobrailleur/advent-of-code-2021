(ns advent-of-code-2021.day04
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]))



(defn create-card [card]
  (map (fn [row] (map #(hash-map :val % :state false) row)) card))

(defn text->card [text]
  (let [rows (str/split text #"\n")
        rows-num (->> rows
                      (map #(str/split (str/trim %) #" +"))
                      (map (fn [row] (map #(Integer/parseInt %) row))))]
    (create-card rows-num)))

(defn winning-row? [row]
  (reduce #(and %1 %2) true (map #(:state %) row)))

(defn winning? [card]
  (or (reduce #(or %1 %2) false (map winning-row? card))
      (reduce #(or %1 %2) false (map winning-row? (util/transpose card)))))

(defn parse-cards [file]
  (let [content (slurp file)
        parts (str/split content #"\n\n")
        draw (map #(Integer/parseInt %) (str/split (first parts) #","))
        cards (map text->card (rest parts))]
    {:draw draw :cards cards}))


(defn update-card-with-draw [card draw]
  (map (fn [row] (map #(if (= (:val %) draw) (assoc % :state true) %) row)) card))

(defn do-draw [draw cards]
  (loop [d draw
         c cards]
    (let [updated-cards (map #(update-card-with-draw % (first d)) c)]
      (if (some winning? updated-cards)
        {:last (first d) :cards updated-cards}
        (if (not (empty? (rest d)))
          (recur (rest d) updated-cards)
          {:last (first d) :cards updated-cards})))))

(defn do-draw-last-to-win [draw cards]
  (loop [d draw
         c cards
         last-to-win nil
         last-to-win-at -1]
    (let [updated-cards (map #(update-card-with-draw % (first d)) c)]
      (if (some winning? updated-cards)
        (recur (rest d)
               (filter #(not (winning? %)) updated-cards)
               (some #(if (winning? %) % nil) updated-cards)
               (first d))
        (if (not (empty? (rest d)))
          (recur (rest d)
                 updated-cards
                 last-to-win
                 last-to-win-at)
          {:last last-to-win-at :cards updated-cards :winning last-to-win})))))

(defn get-unmarked-values [card]
  (->> card
       flatten
       (filter #(not (:state %)))
       (map #(:val %))))

(defn -main [& args]
  (let [{draw :draw cards :cards} (parse-cards "resources/day04.txt")
        {last :last updated-cards :cards} (do-draw draw cards)
        winning (some #(if (winning? %) % nil) updated-cards)
        unmarked-values (get-unmarked-values winning)]
    (println (* last (reduce + 0 unmarked-values))))
  (let [{draw :draw cards :cards} (parse-cards "resources/day04.txt")
        {last :last updated-cards :cards last-to-win :winning} (do-draw-last-to-win draw cards)
        unmarked-values (get-unmarked-values last-to-win)]
    (println (* last (reduce + 0 unmarked-values)))))
