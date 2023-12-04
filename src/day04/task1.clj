(ns day04.task1
  (:require [clojure.string :as str]))

(defn get-set [numbers]
  (set (str/split (str/triml numbers) #"\s+")))

(defn get-point[power]
  (cond (== power 0) 0
        :else (int (Math/pow 2 (- power 1))))
  )
(assert (= 0 (get-point 0)))
(assert (= 8 (get-point 4)))


(defn get-points [numbers]
  (let [splited_numbers (str/split numbers #"\|")
        winning_numbers (first splited_numbers)
        current_numbers (second splited_numbers)]
    (->>
     (clojure.set/intersection (get-set winning_numbers) (get-set current_numbers))
     (count)
     (get-point)))
  )

(defn handle-card[card_str]
  (->>
   (str/split card_str #":")
   (last)
   (get-points)
   )
)

(with-open [rdr (clojure.java.io/reader "assets/day04/real_data.txt")]
  (->>
   (line-seq rdr)
   (map handle-card)
   (reduce +))
  )