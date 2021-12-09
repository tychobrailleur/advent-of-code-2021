(ns advent-of-code-2021.day09
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(def max-point 9)

(defn add-element-beg-end [v elt]
  (into [elt] (conj v elt)))

(defn build-grid [content]
  (let [lines (str/split content #"\n")
        height (count lines)
        grid (map #(vec (util/text->int % #"")) lines)
        width (count (first grid))
        border (vec (replicate (+ 2 width) max-point))]
    (add-element-beg-end (vec (map #(add-element-beg-end % max-point) grid)) border)))

(defn elt-at [grid entry]
  (let [[i j] entry]
    (nth (nth grid i) j)))

(defn find-neighbours [grid entry]
  (let [[i j] entry
        neighbours-delta [[0 -1][0 1][-1 0][1 0]]]
    (vec (map (fn [delta] (let [[di dj] delta]
                            (vector (+ i di) (+ j dj))))
              neighbours-delta))))

(defn find-neighbours-val [grid entry]
  (map #(elt-at grid %) (find-neighbours grid entry)))

(defn find-low-points [grid]
  (let [height (count grid)
        width (count (first grid))]
    (filter #(and (not= max-point (first (get % :neighbours)))
                  (= (first (get % :neighbours))
                     (apply min (get % :neighbours))))
            (for [i (range 1 (dec height))
                  j (range 1 (dec width))]
              {:point [i j]
               :neighbours (into [(elt-at grid [i j])] (find-neighbours-val grid [i j]))}))))

(defn find-risk-level [grid]
  (reduce + 0 (map inc (map first (map :neighbours (find-low-points grid))))))

(defn find-basin [grid low-point]
  (loop [basin [low-point]
         candidates (find-neighbours grid low-point)]
    (let [basin-points (filter #(and (not= (elt-at grid %) max-point)
                                     (not (some (fn [e] (= e %)) basin))) candidates)]
      (if (empty? candidates)
        (distinct basin)
        (recur (concat basin (vec basin-points))
               (mapcat #(find-neighbours grid %) basin-points))))))


(defn -main [& args]
  (let [content (slurp "resources/day09.txt")]
    (println (find-risk-level (build-grid content))))
  (let [content (slurp "resources/day09.txt")
      grid (build-grid content)
      low-points (map :point (find-low-points grid))]
   (println (reduce * 1 (take-last 3 (sort (map count (map #(find-basin grid %) low-points))))))))
