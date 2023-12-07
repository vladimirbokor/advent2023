(ns day07.task1
  (:require [clojure.string :as str])
  )

(defn get-input-pairs [rdr]
  (->>
   (line-seq rdr)
   (map #(str/split % #" ")) 
   ))

(defn count-cards [hand]
  (->>
   (sort hand)
   (partition-by identity)
   (map count)
   (sort-by -)))

(defn get-rank-of-hand [hand]
  (let [card-count (count-cards hand)
        first-num (first card-count)
        second-num (second card-count)]
    (cond
      (== first-num 5) 6
      (== first-num 4) 5
      (and (== first-num 3) (== second-num 2)) 4
      (and (== first-num 3) (== second-num 1)) 3
      (and (== first-num 2) (== second-num 2)) 2
      (and (== first-num 2) (== second-num 1)) 1
      :else 0)))
(assert (= 6 (get-rank-of-hand "AAAAA"))) ;; Five of a kind
(assert (= 5 (get-rank-of-hand "AA8AA"))) ;; Four of a kind
(assert (= 4 (get-rank-of-hand "23332"))) ;; Full house
(assert (= 3 (get-rank-of-hand "TTT98"))) ;; Three of a kind
(assert (= 2 (get-rank-of-hand "23432"))) ;; Two pair
(assert (= 1 (get-rank-of-hand "A23A4"))) ;; One pair
(assert (= 0 (get-rank-of-hand "23456"))) ;; High card

(defn get-rank-of-pair [pair]
  (get-rank-of-hand (first pair))
  )

(def card-order "23456789TJQKA")
(def rank-map (zipmap card-order (iterate inc 0)))

(defn score-hand [hand]
  (reduce
   (fn [acc card]
     (+ (* 13 acc) (rank-map card)))
   0
   hand))

(defn compare-hands [pair1 pair2]
  (let [hand1 (first pair1)
        hand2 (first pair2)]
    (compare (score-hand hand1) (score-hand hand2)))) 


(defn sort-hand-pairs [hands-pair]
  (sort compare-hands hands-pair))


(defn calculate-by-idx [res-seq]
  (map-indexed (fn [idx value] (* (parse-long value) (inc idx))) res-seq))


(with-open [rdr (clojure.java.io/reader "assets/day07/test_input.txt")]
  (->>
   (get-input-pairs rdr)
   (group-by get-rank-of-pair)
   (sort)
   (map second)
   (map sort-hand-pairs)
   (apply concat)
   (map second)
   (calculate-by-idx)
   (reduce + )
   )
  
  )
