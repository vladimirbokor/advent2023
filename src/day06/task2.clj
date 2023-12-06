(ns day06.task2
  (:require [clojure.string :as str]))

(defn get-distance [wait-time, total-time]
  (* wait-time (- total-time wait-time)))
(assert (= 0 (get-distance 0 7)))
(assert (= 10 (get-distance 5 7)))
(assert (= 0 (get-distance 7 7)))


(defn get-winning-counts [total-time, current-max]
  (->>
   (map #(get-distance % total-time) (range (+ total-time 1)))
   (filter #(> % current-max))
   count))
  

(defn parse-input [input]
  (parse-long (str/replace (second (str/split input #":")) #" " ""))) 

(with-open [rdr (clojure.java.io/reader "assets/day06/real_data.txt")]
  (let [lines (line-seq rdr)
        time (parse-input (first lines))
        distance (parse-input (second lines))]
    (get-winning-counts time distance)
    )
  )
