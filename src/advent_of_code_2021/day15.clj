(ns advent-of-code-2021.day15
  (:require [clojure.data.priority-map :refer [priority-map]]
            [advent-of-code-2021.util :as util]
            [clojure.string :as str]))

(def infinity (Double/POSITIVE_INFINITY))

(def border-value -1)
(def max-point 9)

(defn add-element-beg-end [v elt]
  (into [elt] (conj v elt)))

(defn build-grid-without-border [content]
  (let [lines (str/split content #"\n")]
    (mapv vec (mapv #(util/text->int % #"") lines))))

(defn build-grid [content]
  (let [grid (build-grid-without-border content)
        width (count (first grid))
        height (count grid)
        border (vec (replicate (+ 2 width) border-value))]
    (-> (->> grid
             (mapv #(add-element-beg-end % border-value)))
        (add-element-beg-end border))))

(defn display-grid [grid]
  (->> grid
       (map #(str/join "" %))
       (str/join "\n")))


(defn grid-width [grid]
  (count (first grid)))

(defn grid-height [grid]
  (count grid))

(defn elt-at [grid entry]
  (let [[i j] entry]
    (nth (nth grid i) j)))

(defn neg-coords? [point]
  (let [[i j] point]
    (or (neg? i) (neg? j))))

(defn find-neighbours [grid entry lookup-fn]
  (let [[i j] entry
        neighbours-delta [[0 -1][0 1][-1 0][1 0]]]
    (filterv #(and (not= (lookup-fn grid %) border-value)
                   (not (neg-coords? %)))
             (mapv (fn [delta] (let [[di dj] delta]
                                 (vector (+ i di) (+ j dj))))
                   neighbours-delta))))


(defn heuristic
  "Estimated distance to goal from current node."
  [goal current]
  (util/manhattan-distance current goal))

(defn get-risk [grid location]
  (elt-at grid location))

(defn visit-neighbours [graph current goal neighbours frontier came-from risk-so-far lookup-fn]
  (loop [n neighbours
         f frontier
         c came-from
         risks risk-so-far]
    (if (empty? n)
      {:frontier f :from c :risks risks}
      (let [v (first n)
            new-risk (+ (risks current) (lookup-fn graph v))]
        ;; if v is not the parent
        (if (or (not (contains? c v)) (< new-risk (or (risks v) infinity)))
          (recur (rest n)
                 (assoc f v (+ new-risk (heuristic goal v)))
                 (assoc c v current)
                 (assoc risks v new-risk))
          (recur (rest n) f c risks))))))

;; A* star implementation previously done here:
;;   https://github.com/tychobrailleur/graph-sandbox/blob/master/src/graph_sandbox/a_star.clj
(defn a-star-search
  ([grid start end lookup-fn]
   (loop [frontier (priority-map start 0)
          risk-so-far {start 0}  ;; map associating current risk with entry
          came-from {start nil}] ;; start hard no predecessor
     (if (empty? frontier)
       came-from
       (let [current (first (peek frontier))
             neighbours (find-neighbours grid current lookup-fn)
             next (visit-neighbours grid current end neighbours
                                    (pop frontier)
                                    came-from
                                    risk-so-far
                                    lookup-fn)]
         (if (= current end)
           came-from
           (recur (next :frontier) (next :risks) (next :from)))))))
  ([grid start end]
   (a-star-search grid start end elt-at)))


(defn build-path [result start end]
  (loop [path []
         current end]
    (if (or (= current start) (nil? current))
      (conj path start)
      (recur (conj path current)
             (get result current)))))

(defn calculate-risk-path [path grid start lookup-fn]
  (- (reduce (fn [s p] (+ s (lookup-fn grid p))) 0 path)
     (lookup-fn grid start)))


(defn extended-grid-lookup-fn [scale grid entry]
  (let [[i j] entry
        width (count (first grid))
        height (count grid)
        mod_i (mod i width)  ;; mod tells you in what row of the table you are
        rem_i (quot i width) ;; rem tells you in what table you are
        mod_j (mod j height)
        rem_j (quot j height)
        val (elt-at grid [mod_i mod_j])]
    (if (or (>= rem_i scale)
            (>= rem_j scale))
      -1
      (inc (mod (+ (dec val) rem_i rem_j) max-point)))))


;; i cols, j rows

(defn -main [& args]
  (let [content (slurp "resources/day15.txt")
        grid (build-grid content)
        width (grid-width grid)
        height (grid-height grid)
        start [1 1]
        end [(- width 2) (- height 2)]]
    (-> grid
        (a-star-search start end)
        (build-path start end)
        (calculate-risk-path grid start elt-at)
        println))
  (let [content (slurp "resources/day15.txt")
        grid (build-grid-without-border content)
        width (* 5 (grid-width grid))
        height (* 5 (grid-height grid))
        start [0 0]
        end [(dec width) (dec height)]]
    (-> grid
        (a-star-search start end (partial extended-grid-lookup-fn 5))
        (build-path start end)
        (calculate-risk-path grid start (partial extended-grid-lookup-fn 5))
        println)))
