(ns advent-of-code-2021.day05
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util :refer [to-int]]))

(defn in-line? [[start end]]
  (let [[x1 y1] start
        [x2 y2] end]
    (or (= x1 x2) (= y1 y2))))

(defn parse-line [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    [[(to-int x1) (to-int y1)] [(to-int x2) (to-int y2)]]))


(defn find-limits [lines]
  (let [all-points (flatten lines)]
    [(reduce max 0 (take-nth 2 all-points))
     (reduce max 0 (take-nth 2 (rest all-points)))]))

(defn parse-lines-of-vents [content]
  (let [lines (str/split content #"\n")]
    (map parse-line lines)))

(defn create-grid [[x y]]
  (vec (repeat (inc y) (vec (replicate (inc x) 0)))))

(defn mark-point [grid [x y]]
  (let [row (nth grid y)]
    (assoc grid y
           (update-in row [x] inc))))

(defn ints-in-range [start end]
  (if (< start end)
    (range start (inc end))
    (range end (inc start))))

(defn points-in-range [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (map #(vector x1 %) (ints-in-range y1 y2))
    (map #(vector % y1) (ints-in-range x1 x2))))

(defn extended-points-in-range [[x1 y1] [x2 y2]]
  (let [delta-x (- x2 x1)
        delta-y (- y2 y1)]
    (loop [list-points (list)
           dx delta-x
           dy delta-y]
      (if (and (= 0 dx) (= 0 dy)) ;; Always true because diagonals are 45 degrees
        (conj list-points (vector x1 y1))
        (recur (conj list-points (vector (+ dx x1) (+ dy y1)))
               (util/move-towards-zero dx)
               (util/move-towards-zero dy))))))

(defn add-segment-to-grid [grid segment f]
  (let [[start end] segment
        points (f start end)]
    (loop [p points
           g grid]
      (if (empty? p)
        g
        (recur (rest p)
               (mark-point g (first p)))))))

(defn add-segments-to-grid [grid segments f]
  (if (empty? segments)
    grid
    (add-segments-to-grid (add-segment-to-grid grid (first segments) f)
                          (rest segments) f)))

(defn count-overlaps-greater-than [grid n]
  (->> grid
       flatten
       (filter #(>= % n))
       count))

(defn -main [& args]
  (let [content (slurp "resources/day05.txt")
        segments (parse-lines-of-vents content)
        horizontal-or-vertical-segments (filter in-line? segments)
        grid (create-grid (find-limits segments))
        updated-grid (add-segments-to-grid grid horizontal-or-vertical-segments points-in-range)
        updated-second-grid (add-segments-to-grid grid segments extended-points-in-range)]
    (println (count-overlaps-greater-than updated-grid 2))
    (println (count-overlaps-greater-than updated-second-grid 2))))
