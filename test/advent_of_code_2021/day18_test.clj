(ns advent-of-code-2021.day18-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2021.day18 :refer :all]))


(deftest find-left-node-test
  (testing "Find left node 1"
    (is (= (find-left-node (parse-number "[[[3,4],[5,6]],[7,8]]")
                           (list 0 1 1))
           [0 1 0])))
  (testing "Find left node 2"
    (is (= (find-left-node (parse-number "[[[3,4],[5,6]],[7,8]]")
                           (list 1 1 0))
           [1 0]))))


(deftest explode-test
  (testing "No left node"
    (let [current-tree (parse-number "[[[[[9,8],1],2],3],4]")]
      (is (= "[[[[0,9],2],3],4]" (tree->str (explode current-tree (list 0 0 0 0)))))))
  (testing "No right node"
    (let [current-tree (parse-number "[7,[6,[5,[4,[3,2]]]]]")]
      (is (= "[7,[6,[5,[7,0]]]]" (tree->str (explode current-tree (list 1 1 1 1)))))))
  (testing "Explode mid-tree"
    (let [current-tree (parse-number "[[6,[5,[4,[3,2]]]],1]")]
      (is (= "[[6,[5,[7,0]]],3]" (tree->str (explode current-tree (list 0 1 1 1)))))))
  (testing "Explode mid-tree again"
    (let [current-tree (parse-number "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")]
      (is (= "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" (tree->str (explode current-tree (list 0 1 1 1)))))))
  (testing "Explode medium example"
    (let [current-tree (parse-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")]
      (is (= "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" (tree->str (explode current-tree (list 0 0 0 0))))))))


(deftest split-test
  (testing "Simple split"
    (let [current-tree (parse-number "[[[[0,7],4],[15,[0,13]]],[1,1]]")]
      (is (= "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" (tree->str (split current-tree (list 0 1 0))))))))


(deftest find-explode-test
  (testing "Find exploded 1"
    (let [current-tree (parse-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")]
      (is (= [0 0 0 0] (find-explode current-tree))))
    (let [current-tree (parse-number "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")]
      (is (= [0 1 1 0] (find-explode current-tree))))))

(deftest find-split-test
  (testing "Find split 1"
    (let [current-tree (parse-number "[[[[0,7],4],[15,[0,13]]],[1,1]]")]
      (is (= [0 1 0] (find-split current-tree))))))

(deftest reduce-tree-test
  (testing "Reduce tree 1"
    (let [current-tree (parse-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")]
      (is (= "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (tree->str (reduce-tree current-tree)))))))


(deftest magnitude-test
  (testing "Magniture calculation"
    (let [current-tree (parse-number "[[9,1],[1,9]]")]
      (is (= 129 (magnitude current-tree)))))
  (testing "Magnitude calculation medium 1"
    (let [current-tree (parse-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")]
      (is (= 1384 (magnitude current-tree)))))
  (testing "Magnitude calculation medium 2"
    (let [current-tree (parse-number "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")]
      (is (= 3488 (magnitude current-tree))))))

(deftest add-test
  (testing "add simple pairs"
    (let [tree1 (parse-number "[6,2]")
          tree2 (parse-number "[2,6]")]
      (is (= 102 (magnitude (add tree1 tree2)))))))
