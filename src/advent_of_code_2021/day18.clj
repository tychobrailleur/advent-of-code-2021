(ns advent-of-code-2021.day18
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent-of-code-2021.util :as util]))


;; Very inefficient implementation for Day 18 – we could have used zippers
;;   and just work on vectors, but I wanted to experiment with trees and recursive
;;   descent parser, as both are challenging to implement with immutable state.
;;
;; This has been a prodigious waste of time, and the solution for part 2 takes
;;   a long time to execute, but this has been very beneficial to learn more about
;;   Clojure.

(defrecord Node [val left right])


(defn create-node
  ([val left right]
   (->Node val left right))
  ([val]
   (->Node val nil nil))
  ([]
   (->Node nil nil nil)))

(defn add-left [node left]
  (->Node (.val node) left (.right node)))

(defn add-right [node right]
  (->Node (.val node) (.left node) right))


(defn branch? [node]
  (or (not (nil? (.left node)))
      (not (nil? (.right node)))))

(defn leaf? [node]
  (not (branch? node)))

(defn tree->str [tree]
  (if (nil? tree)
    ""
    (str "[" (if (leaf? (.left tree))
               (.val (.left tree))
               (tree->str (.left tree)))
         "," (if (leaf? (.right tree))
               (.val (.right tree))
               (tree->str (.right tree)))
         "]")))


(defn depth [node]
  (if (nil? node)
    0
    (+ 1 (max (depth (.left node))
              (depth (.right node))))))

(defn explodable? [node]
  (>= (depth node) 4))

(defn splitable? [node]
  (if (leaf? node)
    (>= (.val node) 10)
    (or (and (not (nil? (.val node)))
             (>= (.val node) 10))
        (splitable? (.left node))
        (splitable? (.right node)))))

(defn add-node-at-path [tree path node]
  (if (empty? path)
    tree
    (if (= (count path) 1)
      (if (= (first path) 0)
        (->Node (.val tree) node (.right tree))
        (->Node (.val tree) (.left tree) node))
      (if (= (first path) 0)
        (->Node (.val tree)
                (add-node-at-path (.left tree)
                                  (rest path)
                                  node)
                (.right tree))
        (->Node (.val tree)
                (.left tree)
                (add-node-at-path (.right tree)
                                  (rest path)
                                  node))))))

(defn get-node [tree path]
  (loop [p path
         node tree]
    (if (empty? p)
      node
      (if (= (first p) 1)
        (recur (rest p)
               (.right node))
        (recur (rest p)
               (.left node))))))


(defn remove-node [tree path]
  (if (empty? path)
    tree
    (if (= (count path) 1)
      (if (= (first path) 0)
        (->Node (.val tree) (create-node 0) (.right tree))
        (->Node (.val tree) (.left tree) (create-node 0)))
      (if (= (first path) 0)
        (->Node nil (remove-node (.left tree)
                                 (rest path))
                (.right tree))
        (->Node nil (.left tree)
                (remove-node (.right tree)
                             (rest path)))))))

(defn left-node-val [node]
  (if (or (nil? node) (nil? (.left node)))
    nil
    (.val (.left node))))

(defn right-node-val [node]
  (if (or (nil? node) (nil? (.right node)))
    nil
    (.val (.right node))))

(defn update-node
  "Updates the node at path PATH in tree TREE by applying the F function.

  The F function is a one-arg function that takes the existing value of the node at PATH."
  [tree path f]
  (if (empty? path)
    tree
    (if (= (count path) 1)
      (if (= (first path) 0)
        (->Node nil (->Node (f (left-node-val tree)) nil nil) (.right tree))
        (->Node nil (.left tree) (->Node (f (right-node-val tree)) nil nil)))
      (if (= (first path) 0)
        (->Node nil (update-node (.left tree)
                                 (rest path) f)
                (.right tree))
        (->Node nil (.left tree)
                (update-node (.right tree)
                             (rest path) f))))))

(defn find-left-node
  "Finds the node immediately to the left of node at path PATH in the textual representation of TREE."
  [tree path]
  (let [path-to-left-node (doall (drop-while #(not= % 1) (reverse path)))]
    (if (empty? path-to-left-node)
      nil
      (let [first-to-right-node (conj (vec (reverse (rest path-to-left-node))) 0)
            right-node (get-node tree first-to-right-node)]
        ;; find first ancestor which has a left child.
        (loop [current-node right-node  ;; then find its rightmost child.
               p first-to-right-node]
          (if (nil? (.right current-node))
            p
            (recur (.right current-node)
                   (conj p 1))))))))

(defn find-right-node
  "Finds the node immediately to the right of node at path PATH in the textual representation of TREE."
  [tree path]
  (let [path-to-right-node (doall (drop-while #(not= % 0) (reverse path)))]
    (if (empty? path-to-right-node)
      nil
      (let [first-to-left-node (conj (vec (reverse (rest path-to-right-node))) 1)
            left-node (get-node tree first-to-left-node)] ;; find first ancestor which has a right child.
        (loop [current-node left-node  ;; then find its leftmost child.
               p first-to-left-node]
          (if (nil? (.left current-node))
            p
            (recur (.left current-node)
                   (conj p 0))))))))


(defn explode-left [tree current-path path-left val]
  (if (nil? path-left)
    (update-node tree current-path (fn [n] 0))
    (update-node tree path-left #(+ % val))))

(defn explode-right [tree current-path path-right val]
  (if (nil? path-right)
    (update-node tree current-path (fn [n] 0))
    (update-node tree path-right #(+ % val))))

(defn explode [tree path]
  (let [node (get-node tree path)
        val-left (.val (.left node))
        val-right (.val (.right node))
        left (find-left-node tree path)
        right (find-right-node tree path)]
    (-> tree
        (remove-node path)
        (explode-left path left val-left)
        (explode-right path right val-right))))


(defn split [tree path]
  (let [node (get-node tree path)
        val (.val node)
        left-val (int (Math/floor (/ val 2.0)))
        right-val (int (Math/ceil (/ val 2.0)))]
    (-> tree
        (update-node path (fn [n] nil))
        (add-node-at-path (conj (vec path) 0) (create-node left-val))
        (add-node-at-path (conj (vec path) 1) (create-node right-val)))))


(defn find-explode
  ([tree]
   (find-explode tree []))
  ([tree path]
   (if (leaf? tree)
     nil ;; if we got to a leaf, we cannot be at an exploding level
     (if (and (leaf? (.left tree))
              (leaf? (.right tree)))
       (if (>= (count path) 4)
         path
         nil)
       (if (branch? tree)
         (let [explode-on-left (find-explode (.left tree) (conj path 0))]
           (if explode-on-left
             explode-on-left
             (find-explode (.right tree) (conj path 1)))))))))


(defn find-split
  ([tree]
   (find-split tree []))
  ([tree path]
   (if (leaf? tree)
     (if (>= (.val tree) 10)
       path
       nil)
     (let [split-on-left (find-split (.left tree) (conj path 0))]
       (if split-on-left
         split-on-left
         (find-split (.right tree) (conj path 1)))))))

(defn reduce-tree [tree]
  ;; FIXME this is really inefficient, there is surely a better way.
  (if (and (not (explodable? tree))
           (not (splitable? tree)))
    tree
    ;; else explode and split
    (let [path (find-explode tree)]
      (if path
        (reduce-tree (explode tree path))
        (let [split-path (find-split tree)]
          (if split-path
            (reduce-tree (split tree split-path))
            tree))))))

(defn add [left right]
  (reduce-tree (create-node nil left right)))

;; Parsing of input string
;;   I am aware that read-string would have done the job, but I wanted to experiment
;;   with recursive descent parser in pure Clojure.

(declare parse-number)
(defn parse-digit [state]
  (let [input (state :text)]
    (loop [i (state :index)
           digit ""]
      (if (not (Character/isDigit (.charAt input i)))
        (parse-number (-> state
                          (update :stack #(conj % (->Node (Integer/parseInt digit) nil nil)))
                          (assoc :index i)))
        (recur (inc i)
               (str digit (.charAt input i)))))))

(defn parse-close-pair [state]
  (let [stack (state :stack)
        top (peek stack)
        parent (peek (pop stack))
        new-stack (conj (pop (pop stack)) (add-right parent top))]
    (parse-number (-> state
                      (assoc :stack new-stack)))))

(defn parse-separator [state]
  (let [stack (state :stack)
        top (peek stack)
        parent (peek (pop stack))
        new-stack (conj (pop (pop stack)) (add-left parent top))]
    (parse-number (-> state
                      (assoc :stack new-stack)))))

(defn parse-pair [state]
  (let [text (state :text)]
    (parse-number (-> state
                      (update :stack #(conj % (create-node)))))))


(defmulti parse-number (fn [a] (type a)))

(defmethod parse-number java.util.Map [state]
  (let [index (state :index)
        code (state :text)]
    (if (>= index (count code))
      (peek (state :stack))
      (let [ch (.charAt code index)]
        (cond
          (= \[ ch) (parse-pair (update state :index inc))
          (Character/isDigit ch) (parse-digit state)
          (= \, ch) (parse-separator (update state :index inc))
          (= \] ch) (parse-close-pair (update state :index inc)))))))

(defmethod parse-number String [s]
  (parse-number {:index 0 :text s :stack ()}))

(defn str->number [s]
  (parse-number {:index 0 :text s :stack ()}))

(defn magnitude [tree]
  (if (leaf? tree)
    (.val tree)
    (if (and (leaf? (.left tree))
             (leaf? (.right tree)))
      (+ (* 3 (.val (.left tree)))
         (* 2 (.val (.right tree))))
      (+ (* 3 (magnitude (.left tree)))
         (* 2 (magnitude (.right tree)))))))

(defn calculate-sum [content]
  (let [lines (str/split content #"\n")]
    (->> lines
         (map parse-number)
         (reduce add)
         reduce-tree
         magnitude)))

(defn pairs-of-sums [content]
  (let [lines (str/split content #"\n")]
    (-> (->> lines
             (map parse-number))
        (combo/permuted-combinations 2))))

(defn find-max-sum [content]
  (let [pairs (pairs-of-sums content)]
    (loop [m -1
           p pairs]
      (if (empty? p)
        m
        (recur (max m (magnitude (reduce add (first p))))
               (rest p))))))


(defn -main [& args]
  (println (calculate-sum (slurp "resources/day18.txt")))
  (println (find-max-sum (str/trim (slurp "resources/day18.txt")))))

(comment

  (def left (create-node nil
                         (create-node 1 nil nil)
                         (create-node 2 nil nil)))

  (def right (create-node nil
                          (create-node nil
                                       (create-node 3 nil nil)
                                       (create-node 4 nil nil))
                          (create-node 5 nil nil)))

  (read-string "[[[3,4],[5,6]],[7,8]]")

  (println (parse-number {:index 0 :text "[[3,4],5]" :stack ()}))
  (println (tree->str (parse-number {:index 0 :text "[[[3,4],[5,6]],[7,8]]" :stack ()})))

  (println (add (str->number "[1,2]")
                (str->number "[[3,4],5]")))

  (println (tree->str (remove-node (parse-number {:index 0 :text "[[[3,4],[5,6]],[7,8]]" :stack ()})
                                   (list 0 1))))
  (println (tree->str (update-node (parse-number {:index 0 :text "[[[3,4],[5,6]],[7,8]]" :stack ()})
                                   (list 0 1 0)
                                   (fn [n] (+ 10 5)))))



  (let [current-tree (parse-number {:index 0 :text "[[[3,4],[5,6]],[7,8]]" :stack ()})]
    (println (get-node current-tree (find-left-node current-tree (list 0 1 1)))) ;; find left node to 5
    )

  (let [current-tree (parse-number {:index 0 :text "[[[3,4],[5,6]],[7,8]]" :stack ()})]
    (println (add-node-at-path current-tree (list 0 0 1) (->Node 3 nil nil))))


  (let [current-tree (create-node nil
                                  (create-node 1 nil nil)
                                  (create-node 2 nil nil))]
    (println (remove-node (add-node-at-path current-tree (list 0 0) (->Node 3 nil nil))
                          (list 0 0))))

  (let [current-tree (parse-number "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")]
    (println (find-explode current-tree)))

  (let [current-tree (parse-number "[[[[0,7],4],[15,[0,13]]],[1,1]]")]
    (println (find-split current-tree)))


  (let [current-tree (parse-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")]
    (println (tree->str (reduce-tree current-tree))))


  (let [current-tree (parse-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")]
    (println (magnitude current-tree)))

  (let [tree1 (parse-number "[6,2]")
        tree2 (parse-number "[2,6]")]
    (println (magnitude (add tree1 tree2))))

  (add left right))
