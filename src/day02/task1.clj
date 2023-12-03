(ns day02.task1
  (:require [clojure.string :as str])
  )

(def cubes_map {"red" 12, "green" 13, "blue" 14})

(defn is_possible_pair [pair_str]
  (let [splited_pair (str/split (str/triml pair_str) #"\s")]
    (<= (parse-long (first splited_pair)) (cubes_map (last splited_pair)))
  )
)

(defn is_possible_game_iter [current_iter]
  (->>
   (str/split current_iter #",")
   (every? is_possible_pair))
  )

(defn is_possible_game [current_game]
  (->>
   (str/split current_game #";")
   (every? is_possible_game_iter)))


(defn handle_game [game_str]
  (let [splited_str (str/split game_str #":")]
    (cond
      (is_possible_game (last splited_str)) (parse-long (last (str/split (first splited_str) #"\s")))
      :else 0
    )
  )
)

(with-open [rdr (clojure.java.io/reader "assets/day02/real_data.txt")]
  (->>
   (line-seq rdr)
   (map handle_game)
   (reduce +))
)