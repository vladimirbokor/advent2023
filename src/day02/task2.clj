(ns day02.task2
  (:require [clojure.string :as str])
  )


(defn make_pair [pair_str]
  (let [splited_pair (str/split (str/triml pair_str) #"\s")]
    (hash-map (last splited_pair) (parse-long (first splited_pair)))
  )
)

(defn make_current_map [current_iter]
  (->>
   (str/split current_iter #",")
   (map make_pair)
   (apply merge)
   )
)

(defn get_game_power [current_game]
  (->>
   (str/split current_game #";")
   (map make_current_map)
   (apply (partial merge-with max))
   vals
   (reduce *)
   )
)


(defn handle_game [game_str]
  (->>
   (str/split game_str #":")
   last
   (get_game_power)
   )
)

(with-open [rdr (clojure.java.io/reader "assets/day02/real_data.txt")]
  (->>
   (line-seq rdr)
   (map handle_game)
   (reduce +))
)