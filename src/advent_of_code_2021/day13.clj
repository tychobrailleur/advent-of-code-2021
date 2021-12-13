(ns advent-of-code-2021.day13
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(defn grid-height [grid]
  (count grid))

(defn grid-width [grid]
  (count (first grid)))

(defn set-elt [grid x y val]
  (assoc-in grid [y x] val))

(defn get-elt [grid x y]
  (get-in grid [y x]))

(defn fold-vertically [grid fold]
  (subvec (let [width (grid-width grid)
                height (grid-height grid)]
            (loop [g grid
                   x 0
                   dy 1]
              (if (>= (+ fold dy) height)
                g
                (let [elt (get-elt g x (- fold dy))
                      new-y (if (= x (dec width)) (inc dy) dy)
                      new-x (mod (inc x) width)]

                  (recur (set-elt g x
                                  (- fold dy)
                                  (or elt (get-elt g x (+ fold dy))))
                         new-x new-y))))) 0 fold))

(defn fold-horizontally [grid fold]
  (mapv #(subvec % 0 fold)
        (let [width (grid-width grid)
              height (grid-height grid)]
          (loop [g grid
                 dx 1 ;; distance to fold
                 y 0]
            (if (>= y height)
              g
              (let [elt (get-elt g (- fold dx) y)
                    new-y (if (= dx (- (dec width) fold)) (inc y) y)
                    new-dx (if (= dx (- (dec width) fold)) 1 (inc dx))]
                (recur (set-elt g (- fold dx)
                                y
                                (or elt (get-elt g (+ fold dx) y)))
                       new-dx new-y)))))))

(defn display-grid [grid]
  (str/join "\n" (map str/join (map #(map (fn [row] (get {true "#" false "."} row)) %) grid))))

(defn build-grid [points]
  (let [t (util/transpose points)
        width (apply max (first t))
        height (apply max (second t))
        grid (vec (repeat (inc height)
                          (vec (replicate (inc width) false))))]
    (loop [g grid
           p points]
      (if (empty? p)
        g
        (let [[x y] (first p)]
          (recur (set-elt g x y true) (rest p)))))))

(defn text->points [text]
  (mapv #(mapv util/to-int (str/split % #","))
        (str/split text #"\n")))


(defn execute-fold-instructions [content one-fold?]
  (let [[points instruction-section] (str/split content #"\n\n")
        instructions (str/split instruction-section #"\n")
        grid (build-grid (text->points (str/trim points)))]
    (loop [i instructions
           g grid]
      (if (empty? i)
        g
        (let [[_ direction fold] (re-matches #"fold along ([xy])=([0-9]+)" (first i))]
          (if (= direction "x")
            (if one-fold?
              (fold-horizontally g (Integer/parseInt fold))
              (recur (rest i) (fold-horizontally g (Integer/parseInt fold))))
            (if one-fold?
              (fold-vertically g (Integer/parseInt fold))
              (recur (rest i) (fold-vertically g (Integer/parseInt fold))))))))))

(defn count-dots [grid]
  (count (filter identity (apply concat grid))))

(defn -main [& args]
  (println (count-dots (execute-fold-instructions (slurp "resources/day13.txt") true)))
  (println (display-grid (execute-fold-instructions (slurp "resources/day13.txt") false))))
