(ns advent-of-code-2021.day11
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(def border-value -1)
(def max-point 9)

(defn add-element-beg-end [v elt]
  (into [elt] (conj v elt)))

(defn height-grid [grid]
  (count grid))

(defn width-grid [grid]
  (count (first grid)))

(defn build-grid [content border-val f]
  (let [lines (str/split content #"\n")
        height (count lines)
        grid (map #(mapv f (util/text->int % #"")) lines)
        width (count (first grid))
        border (vec (replicate (+ 2 width) (f border-val)))]
    (add-element-beg-end (mapv #(add-element-beg-end % (f border-val)) grid) border)))

(defn display-grid [grid]
  (str/join "\n" (map #(str/join (filter (comp not neg?) (map :val %))) grid)))

(defn elt-at [grid entry]
  (let [[i j] entry]
    (nth (nth grid i) j)))

(defn find-neighbours [grid entry border-val]
  (let [[i j] entry
        i-deltas [-1 0 1]
        j-deltas [-1 0 1]]
    (filterv #(not= (:val (elt-at grid %)) border-val)
             (for [di i-deltas
                   dj j-deltas
                   :when (and (not (and (= di 0) (= dj 0))))]
               (vector (+ i di) (+ j dj))))))

(defn all-coords [grid]
  (for [i (range 1 (dec (height-grid grid)))
        j (range 1 (dec (width-grid grid)))]
    [i j]))

(defn reset-entry [grid point]
  (let [[i j] point]
    (if (= (get-in grid [i j :state]) :fired)
      (-> grid
          (assoc-in [i j :val] 0)
          (assoc-in [i j :state] :rest))
      grid)))

(defn increment-energy [grid coords]
  (loop [g grid
         c coords]
    (if (empty? c)
      g
      (let [[i j] (first c)]
        (recur (update-in g [i j :val] inc)
               (rest c))))))

(defn fire-octopus [grid point]
  (let [[i j] point
        val (get-in grid [i j :val])]
    (if (not= (get-in grid [i j :state]) :fired)
      (if (> val max-point)
        (let [g (assoc-in grid [i j :state] :fired)
              neighbours (find-neighbours g point border-value)]
          (if (empty? neighbours)
            g
            (reduce (fn [acc nn]
                      (let [[ii jj] nn]
                        (if (and (not= (get-in acc [ii jj :state]) :fired)
                                 (> (get-in acc [ii jj :val]) max-point))
                          (fire-octopus acc nn)
                          acc)))
                    (increment-energy g neighbours)
                    neighbours)))
        grid)
      grid)))

(defn make-octopi-fire [grid]
  (loop [g grid
         coords (all-coords grid)]
    (if (empty? coords)
      g
      (recur (fire-octopus g (first coords))
             (rest coords)))))

(defn all-fired? [grid]
  (= (* (- (height-grid grid) 2) (- (width-grid grid) 2)) ;; minus 2, for the borders.
     (count (filter #(> (:val %) max-point) (apply concat grid)))))

(defn count-fired-and-reset [grid sum state]
  (let [count-fired (count (filter #(= (:state %) :fired)
                                   (apply concat grid)))]
    (loop [g grid
           c (all-coords grid)]
      (if (empty? c)
        [g (+ sum count-fired) (if (and (all-fired? grid)
                                        (neg? (state :all-flashed)))
                                 (assoc state :all-flashed (inc (state :step)))
                                 (update state :step inc))]
        (recur (reset-entry g (first c))
               (rest c))))))

(defn process-step [[grid count state]]
  (let [coords (all-coords grid)]
    (-> grid
        (increment-energy coords)
        make-octopi-fire
        (count-fired-and-reset count state))))

(defn build-entry [val]
  (hash-map :val val :state :rest))

(defn process [content n]
  (let [grid (build-grid content border-value build-entry)]
    (last (take (inc n) (iterate process-step [grid 0 {:step 0 :all-flashed -1}])))))

(defn process-till-all-fired [content]
  (let [grid (build-grid content border-value build-entry)]
    (loop [output (process-step [grid 0 {:step 0 :all-flashed -1}])]
      (let [s (:all-flashed (last output))]
        (if (not (neg? s))
          s
          (recur (process-step output)))))))

(defn -main [& args]
  (let [content (slurp "resources/day11.txt")]
    (println (second (process content 100))))
  (let [content (slurp "resources/day11.txt")]
    (println (process-till-all-fired content))))
