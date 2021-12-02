(ns advent-of-code-2021.day01
  (:require [clojure.string :as str]))


(defn file-to-int-list [content]
  (->> (str/split content #"\n")
       (map #(Integer/parseInt %))))


(defn find-successive-increments [l]
  (->> l
       (partition 2 1)
       (map (fn [s] (let [[a b] s] (- b a))))
       (filter #(> % 0))
       count))


(defn find-three-measure-increments [l]
  (->> l
       (partition 3 1)
       (map #(reduce + 0 %))
       find-successive-increments))

(defn -main [& args]
  (let [content (slurp "resources/day01.txt")
        nums (file-to-int-list content)]
    (println (find-successive-increments nums))
    (println (find-three-measure-increments nums))))
