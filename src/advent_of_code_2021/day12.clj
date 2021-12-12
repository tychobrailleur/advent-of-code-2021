(ns advent-of-code-2021.day12
  (:require [clojure.string :as str]))

(defn build-graph [content]
  (let [v (mapv #(str/split % #"\-")
                (str/split content #"\n"))]
    (loop [graph {}
           pairs v]
      (if (empty? pairs)
        graph
        (let [[s e] (first pairs)]
          (recur
           (-> graph
               (update-in [s] (fnil conj #{}) e)
               (update-in [e] (fnil conj #{}) s))
           (rest pairs)))))))

(defn neighbours [graph node]
  (get graph node))


(defn small-cave? [loc]
  (and (or (and (not= loc "end")
                (not= loc "start")))
       (Character/isLowerCase (first loc))))

(defn has-small-cave? [path cave]
  (some #(= cave %) path))

(defn check-small-cave-once-rule [path loc]
  (or (and (small-cave? loc)
           (has-small-cave? path loc))
      (= loc "start")))

(defn already-two-visit-small-cave [path]
  (filter (fn [[k v]] (> v 1))
          (frequencies (filter small-cave? path))))

(defn check-single-small-cave-twice-rule [path loc]
  (let [double (already-two-visit-small-cave path)]
    (or (and (small-cave? loc)
             (and double
                  (has-small-cave? path loc)))
        (= loc "start"))))

(defn visit-neighbours [graph current-path small-cave-rule]
  (let [current-location (last current-path)
        neighbours (neighbours graph current-location)]
    (loop [n neighbours
           paths [current-path]]
      (if (empty? n)
        paths
        (let [new-loc (first n)]
          (if (= "end" new-loc)
            (recur (rest n) (conj paths (conj current-path new-loc)))
            (if (small-cave-rule current-path new-loc)
              (recur (rest n) paths)
              (recur (rest n) (concat paths (visit-neighbours graph
                                                              (conj current-path new-loc)
                                                              small-cave-rule))))))))))

(defn find-paths [graph start small-cave-rule]
  (let [current-paths []]
    (conj current-paths (visit-neighbours graph [start] small-cave-rule))))


(defn -main [& args]
  (let [content (slurp "resources/day12.txt")
        graph (build-graph content)]
    (println (count (filterv #(= "end" (last %))
                             (distinct (first (find-paths graph "start" check-small-cave-once-rule)))))))
  (let [content (slurp "resources/day12.txt")
        graph (build-graph content)]
    (println (count (filterv #(= "end" (last %))
                             (distinct (first (find-paths graph "start" check-single-small-cave-twice-rule))))))))
