(ns day04.task2
  (:require [clojure.string :as str]))

(defn get-set [numbers]
  (set (str/split (str/triml numbers) #"\s+")))


(defn get-winning-counts [numbers]
  (let [splited_numbers (str/split numbers #"\|")
        winning_numbers (first splited_numbers)
        current_numbers (second splited_numbers)]
    (->>
     (clojure.set/intersection (get-set winning_numbers) (get-set current_numbers))
     (count))))

(defn handle-card [card_str]
  (->>
   (str/split card_str #":")
   (last)
   (get-winning-counts)))

(defn gen-initial-vec [winning_vec_count]
  (vec (repeat (count winning_vec_count) 1)) 
  )

(defn generate-vec-to-sum [current_position length current_value count]
  (let [before (repeat (+ current_position 1) 0)
        values (repeat count current_value)
        after  (repeat (- (- length (+ current_position count)) 1) 0)]
    (into [] (concat before values after))))

(defn sum-vecs [current_vec, current_position, win_counts]
  (if (>= current_position (count current_vec))
    current_vec
    (let [vec_to_sum (generate-vec-to-sum current_position (count current_vec) (get current_vec current_position) (get win_counts current_position))]
      (sum-vecs (vec (map + current_vec vec_to_sum)) (+ current_position 1) win_counts))))

(with-open [rdr (clojure.java.io/reader "assets/day04/real_data.txt")]
  (let [winning_vec_count (vec (map handle-card (line-seq rdr)))
        initial_vec (gen-initial-vec winning_vec_count)]
    (reduce + (sum-vecs initial_vec 0 winning_vec_count))))