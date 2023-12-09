(ns day08.task2
  (:require [clojure.string :as str]))

(defn parse-pair [s]
  (let [[key value] (str/split s #" = ")
        value-clean (-> value
                        (str/replace "(" "")
                        (str/replace ")" ""))
        value-pair (str/split value-clean #", ")]
    {key (vec value-pair)}))

(defn create-generator [s]
  (let [state (atom (cycle s))]
    (fn []
      (let [next-val (first @state)]
        (swap! state next)
        next-val))))

(defn get-next-map [move, maps, map]
  (let [[left, right] (maps map)]
    (if (= move \R) right left))
)

(defn go-through-map [instr-gen maps next-maps count]
  (loop [curr-maps next-maps
         curr-count count]
    (let [next-move (instr-gen)
          current-map (map #(get-next-map next-move maps %) curr-maps)]
      (if (every? #(str/ends-with? % "Z") current-map)
        curr-count
        (recur current-map (inc curr-count))))))


(with-open [rdr (clojure.java.io/reader "assets/day08/real_data.txt")]
  (let [lines (line-seq rdr)
        instructions (first lines)
        maps-lines (nthrest lines 2)
        maps (into {} (map parse-pair maps-lines))
        instr-gen (create-generator instructions)
        initial-map (filterv #(str/ends-with? % "A") (keys maps))]
    ;; Not working ;(
    (go-through-map instr-gen maps initial-map 1)
    ) 
)