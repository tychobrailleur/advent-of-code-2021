(ns advent-of-code-2021.day16
  (:require [clojure.string :as str]))

(def hex-bit-mapping {\0 [0 0 0 0]
                      \1 [0 0 0 1]
                      \2 [0 0 1 0]
                      \3 [0 0 1 1]
                      \4 [0 1 0 0]
                      \5 [0 1 0 1]
                      \6 [0 1 1 0]
                      \7 [0 1 1 1]
                      \8 [1 0 0 0]
                      \9 [1 0 0 1]
                      \A [1 0 1 0]
                      \B [1 0 1 1]
                      \C [1 1 0 0]
                      \D [1 1 0 1]
                      \E [1 1 1 0]
                      \F [1 1 1 1]})


(defn hex->bits [hex]
  (apply concat (map #(get hex-bit-mapping %) hex)))


(defn parse-content [content]
  (.toCharArray content))

(defn bits->decimal
  "Converts an array of bits into its decimal representation."
  [bits]
  (let [length (count bits)]
    (loop [i length
           s 0]
      (if (= i 0)
        s
        (recur (dec i)
               (+ s (bit-shift-left (nth bits (- length i)) (dec i))))))))

(defn parse-literal-packet [bits state]
  (loop [b bits
         literal-bits []]
    (let [packet (vec (take 5 b))]
      (if (= (first packet) 1) ;; 1, not last packet
        (recur (drop 5 b)
               (concat literal-bits (subvec packet 1)))
        (let [literal (bits->decimal (concat literal-bits (subvec packet 1)))
              version (get-in state [:current :version])
              type-id (get-in state [:current :type-id])
              new-packet {:version  version :type-id type-id :value literal}]
          (-> state
              (update :packets #(conj % new-packet))
              (assoc :rest (vec (drop 5 b)))
              (dissoc :current)))))))

(declare parse-packets)
(declare parse-packet)

(defn parse-operator-15bit [bits state]
  (let [length (bits->decimal (take 15 bits))
        rest-bits (vec (drop 15 bits))]
    (-> state
        (update :packets #(conj % {:version (get-in state [:current :version])
                                   :type-id (get-in state [:current :type-id])
                                   :packets ((parse-packets (subvec rest-bits 0 length)) :packets)}))
        (assoc :rest (subvec rest-bits length))
        (dissoc :current))))

(defn parse-operator-11bit [bits state]
  (let [total-packet-num (bits->decimal (take 11 bits))]
    (loop [b (vec (drop 11 bits))
           packets []
           packet-num 1]
      (if (> packet-num total-packet-num)
        (let [new-packet {:version (get-in state [:current :version])
                          :type-id (get-in state [:current :type-id])
                          :packets packets}]
          (-> state
              (update :packets #(conj % new-packet))
              (assoc :rest b)
              (dissoc :current)))
        (let [packet-state (parse-packet b {:packets [] :rest b})]
          (recur (packet-state :rest)
                 (vec (concat packets (packet-state :packets)))
                 (inc packet-num)))))))

(defn parse-operator-packet [bits state]
  (let [length-type-id (bits->decimal (take 1 bits))]
    (if (= 0 length-type-id)
      (parse-operator-15bit (rest bits) state)
      (parse-operator-11bit (rest bits) state))))


(defn parse-type-id [bits state]
  (let [type-id (bits->decimal (take 3 bits))]
    (if (= type-id 4)
      (parse-literal-packet (vec (drop 3 bits))
                            (assoc-in state [:current :type-id] type-id))
      (parse-operator-packet (vec (drop 3 bits))
                             (assoc-in state [:current :type-id] type-id)))))

(defn parse-version [bits state]
  (let [version (bits->decimal (take 3 bits))]
    (parse-type-id (vec (drop 3 bits)) (-> state
                                           (assoc :rest (drop 3 bits))
                                           (assoc :current (hash-map :version version))))))

(defn parse-packet [bits state]
  (parse-version bits state))

(defn parse-packets [bits]
  (loop [state {:packets [] :rest bits}
         b bits]
    (if (empty? b)
      state
      (let [st (parse-packet b state)]
        (recur st
               (:rest st))))))

(defn process-code [code]
  (let [bits (hex->bits (parse-content code))]
    (parse-packet bits {:packets []})))

(defn sum-versions [packets]
  (if (packets :packets)
    (reduce (fn [acc packt] (+ acc (sum-versions packt)))
            (get packets :version 0) (packets :packets))
    (get packets :version 0)))

(defn walk-tree [p]
  (if (vector? p)
    (map #(walk-tree %) p)
    (if (contains? p :rest) ;; highest level packet needs to be unwrapped
      (walk-tree (get p :packets))
      (case (p :type-id)
        0 (apply + (walk-tree (vec (get p :packets))))
        1 (apply * (walk-tree (get p :packets)))
        2 (apply min (walk-tree (get p :packets)))
        3 (apply max (walk-tree (get p :packets)))
        4 (p :value)
        5 (if (> (first (walk-tree (get p :packets)))
                 (walk-tree (second (get p :packets)))) 1 0)
        6 (if (< (first (walk-tree (get p :packets)))
                 (walk-tree (second (get p :packets)))) 1 0)
        7 (if (= (first (walk-tree (get p :packets)))
                 (walk-tree (second (get p :packets)))) 1 0)))))


(comment

  (println (process-code "EE00D40C823060")) ;; length type id 11
  (println (process-code "38006F45291200")) ;; length type id 15
  (println (process-code "8A004A801A8002F478"))
  (println (process-code "620080001611562C8802118E34"))
  (println (process-code "C0015000016115A2E0802F182340"))
  (println (process-code "A0016C880162017C3686B18A3D4780"))

  (println (sum-versions (process-code "A0016C880162017C3686B18A3D4780")))
  (println (sum-versions (process-code (slurp "resources/day16.txt"))))


  (println (walk-tree (process-code "C200B40A82")))
  (println (walk-tree (process-code "04005AC33890")))
  (println (walk-tree (process-code "880086C3E88112")))
  (println (walk-tree (process-code "CE00C43D881120")))
  (println (walk-tree (process-code "D8005AC2A8F0")))
  (println (walk-tree (process-code "F600BC2D8F")))
  (println (walk-tree (process-code "9C005AC2F8F0")))
  (println (walk-tree (process-code "9C0141080250320F1802104A08")))

  (println (walk-tree (process-code (slurp "resources/day16.txt"))))
  )

(defn -main [& args]
  (println (sum-versions (process-code (slurp "resources/day16.txt"))))
  (println (first (walk-tree (process-code (slurp "resources/day16.txt"))))))
