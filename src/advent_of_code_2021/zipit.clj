(ns advent-of-code-2021.zipit)

(ns advent-of-code-2021.day18
  (:require [clojure.zip :as zip]
            [clojure.string :as str]))



(defn add [left right]
  (zip/vector-zip [left right]))


(defn depth [node]
  (if (not (zip/branch? node))
    0
    (let [left-node (zip/down node)]
      (+ 1 (max (depth left-node)
                (depth (zip/next left-node)))))))

(defn explodable? [node]
  (>= (depth node 4)))

(defn get-left-child [node]
  (zip/down node))

(defn get-right-child [node]
  (let [left (zip/down node)]
    (zip/right node)))

(defn splitable? [node]
  (or (and (not (zip/branch? node))
           (>= (zip/node node) 10))
      (splitable? (get-left-child node))
      (splitable? (get-right-child node))))


(defn reduce [node]
  (if (and (not (explodable? node))
           (not (splitable? node)))
    node
    ;; else explode and split

    ))

;; (defn parse-pair [text state]
;;   (parse-number (rest text)
;;                 (-> state
;;                     (update :stack #(conj % :openpair))
;;                     (update :current (create-node)))
;;                 (update state :stack #(conj % :openpair))))

;; (defn parse-digit [text state]
;;   (loop [input text
;;          digit ""]
;;     (if (empty? input)
;;       state
;;       (if (not (.isDigit (first input)))
;;         (parse-number input
;;                       (update state :stack #(conf % (Integer/parseInt digit))))
;;         (recur (rest input)
;;                (str digit (first input)))))))

;; (defn parse-separator [text state]
;;   (parse-number (rest text)
;;                 (update state :current #(add-left % (peek (:stack state))))
;;                 (update state :stack #(pop %))))

;; (defn parse-close-pair [text state]
;;   (let [new-state (-> state
;;                       (update state :current #(add-right % (peek (:stack state))))
;;                       (update state :stack #(pop %)))])
;;   (if (empty? text)
;;     new-state
;;     (parse-number (rest text) new-state)))

;; (defn parse-number [text state]
;;   (if (empty? text)
;;     state
;;     (let [ch (first text)]
;;       (cond
;;         (= "[") (parse-pair (rest text) state)
;;         (.isDigit ch) (parse-digit text state)
;;         (= ",") (parse-separator text state)
;;         (= "]") (parse-close-pair (rest text) state)
;;         ()))))

(comment

  (def v (read-string (str/replace "[[[1,2],[3,4]],[5, 6]]" "," " ")))
  (zip/vector-zip v)

  (depth (zip/vector-zip v))

  (get-left-child (zip/vector-zip [4 5]))
  (splitable? (zip/vector-zip [12 10]))
  (add left right))
