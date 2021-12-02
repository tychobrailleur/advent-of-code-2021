(ns advent-of-code-2021.day02
  (:require [clojure.string :as str]))

(defn update-state [state direction distance]
  (update-in state [direction] (partial + distance)))

(defn move-submarine [commands state with-aim]
  (if (empty? commands)
    state
    (let [command (first commands)
          [dir distance] (str/split command #" ")]
      (cond
        (= dir "forward")
        (move-submarine (rest commands)
                        (if with-aim
                          (-> state
                              (update-state :position (Integer/parseInt distance))
                              (update-state :depth (* (state :aim) (Integer/parseInt distance))))
                          (update-state state :position (Integer/parseInt distance)))
                        with-aim)
        (= dir "down")
        (move-submarine (rest commands)
                        (if with-aim
                          (update-state state :aim (Integer/parseInt distance))
                          (update-state state :depth (Integer/parseInt distance)))
                        with-aim)
        (= dir "up")
        (move-submarine (rest commands)
                        (if with-aim
                            (update-state state :aim (- (Integer/parseInt distance)))
                            (update-state state :depth (- (Integer/parseInt distance))))
                        with-aim)))))


(defn -main [& args]
  (let [commands (str/split (slurp "resources/day02.txt") #"\n")
        state (move-submarine commands {:depth 0 :position 0 :aim 0} false)
        state-with-aim (move-submarine commands {:depth 0 :position 0 :aim 0} true)]
    (let [{depth :depth position :position} state
          {depth-with-aim :depth pos-with-aim :position} state-with-aim]
      (println (* depth position))
      (println (* depth-with-aim pos-with-aim)))))
