(ns advent-of-code-2021.day17
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]))



(defn make-step [p]
  (let [[x y vx vy] p]
    [(+ x vx) (+ y vy) (util/move-towards-zero vx) (dec vy)]))

(defn create-target [[range_x range_y]]
  (let [[x_min x_max] range_x
        [y_min y_max] range_y]
    [x_min y_min x_max y_max]))

(defn overshot? [target p]
  (let [[x y vx vy] p
        [x_a y_a x_b y_b] target
        min_x (min x_a x_b)
        min_y (min y_a y_b)
        max_x (max x_a x_b)
        max_y (max y_a y_b)]
    (or (> x max_x)
        (and (< y min_y) (neg? vy))
        (and (> x min_x ) (> y max_y) (> vx (- max_x x))))))

(defn in-plot? [plot p]
  (some #(and (= (first %) (first p))
              (= (second %) (second p))) plot))

(defn in-target? [target p]
  (let [[x_a y_a x_b y_b] target
        [x y] p]
    (and (<= x_a x) (>= x_b x) (<= y_a y) (>= y_b y))))

(defn build-plane [plot target]
  (let [[xes yes _ _] (util/transpose plot)
        [target_low_x target_low_y target_high_x target_high_y] target
        max_x (max (apply max xes) target_high_x)
        max_y (max (apply max yes) target_high_y)
        min_x (min (apply min xes) target_low_x)
        min_y (min (apply min yes) target_low_y)
        width (inc (- max_x min_x))
        height (inc (- max_y min_y))
        offset_y (- max_y height)]

    (for [p_y (range 0 height)]
      (for [p_x (range 0 width)]
        (if (in-plot? plot [p_x (+ (- height p_y) offset_y)])
          \#
          (if (in-target? target [p_x (+ (- height p_y) offset_y)])
            \T
            \.))))))

(defn find-min-vx0 [min-x-target]
  ;; if x is below a certain threshold, it will never reach target.
  ;;  v_x decrements at each step, so it is a arithmetic series,
  ;; so v_x must be at least v_x(v_x+1)/2 = min_x_target
  (defn delta [a b c]
    (- (* b b) (* 4 a c)))
  (let [root-1 (/ (- (- 1) (Math/sqrt (delta 1 1 (- (* 2 min-x-target))))) 2)
        root-2 (/ (+ (- 1) (Math/sqrt (delta 1 1 (- (* 2 min-x-target))))) 2)]
    (int (Math/ceil (max root-1 root-2)))))

(defn find-velocity-for-max-y [target]
  (let [[x_a y_a x_b y_b] target
        velocities (for [vy (range 1 100) ;; arbitrary max vy, there should be a way
                                           ;;to compute an optimal with Pythagorus.
                         vx (range (find-min-vx0 x_a) x_b)]
                     [vx vy])]
    (loop [velo velocities
           on-target []
           max-y 0
           coord-max-y nil]
      (if (empty? velo)
        coord-max-y
        (let [[vx0 vy0] (first velo)]
          (recur (rest velo)
                 ;; Loop for one initial velocity
                 (let [outcome (loop [location (make-step [0 0 vx0 vy0])
                                      highest-y 0]
                                 (if (in-target? target location)
                                   [vx0 vy0 highest-y] ;; if we are on target, we have a candidate
                                   (if (and (= (last location) 0) ;; if vy == 0
                                            (< (second location) max-y)) ;; and y below max-x, stop here.
                                     nil
                                     (if (not (overshot? target location)) ;; continue if we have not overshot
                                       (recur (make-step location)
                                              (if (> (second location) highest-y) (second location) highest-y))
                                       nil))))]
                   (if outcome
                     (conj on-target outcome)
                     on-target))
                 (reduce max 0 (map #(last %) on-target))
                 (first (filter #(= (last %) max-y) on-target))))))))

(defn display-plane [plane]
  (str/join "\n" (map (fn [row] (str/join row)) plane)))

(defn -main [& args]
  (println (last (find-velocity-for-max-y (create-target [[253 280] [-73 -46]])))))

(comment

  (println (take 5 (iterate make-step [0 0 7 2])))

  (def target (create-target [[20 30] [-10 -5]]))
  (println (-> (->> [0 0 17 -4]
                    (iterate make-step)
                    (take 2))
               (build-plane target)
               display-plane))

  (println (-> (->> [0 0 7 2]
                    (iterate make-step)
                    (take 8))
               (build-plane target)
               display-plane))

  (let [steps (take 8 (iterate make-step [0 0 17 -4]))]
    (map (partial overshot? [20 -5 30 -10]) steps))

  (let [steps (take 10 (iterate make-step [0 0 7 2]))]
    (map (partial overshot? [20 -5 30 -10]) steps))



  (println (-> (->> [0 0 6 3]
                    (iterate make-step)
                    (take 11))
               (build-plane target)
               display-plane))

  (let [steps (take 11 (iterate make-step [0 0 6 3]))]
    (map (partial overshot? [20 -5 30 -10]) steps))

    (println (-> (->> [0 0 5 3]
                    (iterate make-step)
                    (take 11))
               (build-plane target)
               display-plane))

    (find-velocity-for-max-y (create-target [[20 30] [-10 -5]]))
    (println (find-velocity-for-max-y (create-target [[253 280] [-73 -46]])))


  (make-step (make-step [0 0 7 2])))
