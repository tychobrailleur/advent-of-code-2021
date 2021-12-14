(ns advent-of-code-2021.day14
  (:require [clojure.string :as str]))



(defn parse-rule [line]
  (let [[_ base insertion] (re-matches #"([A-Z]{2}) -> ([A-Z])" line)]
    (hash-map [(first base) (last base)]
              [[(first base) (first insertion)] [(first insertion) (last base)]])))

(defn build-rules [text]
  (->> (str/split text #"\n")
       (mapv parse-rule)
       (reduce #(merge %1 %2) {})))


(defn apply-rules [rules input]
  (->> input
       (mapv #(get rules %))
       (apply concat)))

(defn divide-values [mm]
  (reduce-kv (fn [m k v] (assoc m k (/ v 2))) {} mm))

(defn count-letters [polymer]
  (let [last-letter (second (last polymer))]
    (loop [i polymer
           counts {}]
      (if (empty? i)
        (update counts last-letter inc)
        (let [[one two] (first i)]
          (recur (rest i)
                 (update-in counts [one] (fnil inc 1))))))))


;; For second part, this was very hard, as brute force was no longer working:
;;   the polymer was getting far too huge to be able to count letters.
;;   Instead the solution here counts pairs after each iteration, and updates a map.
;;   At the end, we could the letters by getting them from the pair frequencies, and divide by two,
;;   as a pair [[AB][BC]] becomes [ABC]
;;   One of first letter and last letter needs to be added before dividing, as those two are always stable.

(defn apply-rules-2 [rules state]
  (loop [pairs (keys state)
         new-state {}]
    (if (empty? pairs)
      new-state
      (let [pair (first pairs)
            freq (get state pair)
            [pair-one pair-two] (get rules pair)]
        (recur (rest pairs)
               (-> new-state
                   (update pair-one (fnil (partial + freq) 0))
                   (update pair-two (fnil (partial + freq) 0))))))))

(defn count-letters-2 [pairs freqs first-letter last-letter]
  (loop [p pairs
         counts {}]
    (if (empty? p)
      (-> counts
          (update first-letter inc)
          (update last-letter inc)
          divide-values)
      (let [[one two] (first p)
            cc (get freqs (first p))]
        (recur (rest p)
               (-> counts
                   (update one (fnil #(+ cc %) 0))
                   (update two (fnil #(+ cc %) 0))))))))

(defn -main [& args]

  ;; First “naïve” approach, re-building the polymers.
  (let [content (slurp "resources/day14.txt")
        [input-text rules-def] (str/split content #"\n\n")
        input (->> input-text
                   (partition 2 1)
                   (mapv vec))
        rules (build-rules rules-def)
        polymer (last (take 11 (iterate (partial apply-rules rules) input)))
        state (frequencies polymer)
        freqs (count-letters polymer)]
    (println (- (apply max (vals freqs))
                (apply min (vals freqs)))))

  ;; Second approach, just counting pairs after each round.
  (let [content (slurp "resources/day14.txt")
        [input rules-def] (str/split content #"\n\n")
        rules (build-rules rules-def)
        state (->> input
                   (partition 2 1)
                   (map vec)
                   frequencies
                   (iterate (partial apply-rules-2 rules))
                   (take 41)
                   last)
        freqs (count-letters-2 (keys state) state (first input) (last input))]
    (println (- (apply max (vals freqs))
                (apply min (vals freqs))))))
