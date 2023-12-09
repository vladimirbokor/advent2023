(ns day08.task1
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

(defn go-through-map [instr-gen maps next-map count]
  (loop [current-map next-map
         current-count count]
    (let [[left, right] (maps current-map)
          res (if (= (instr-gen) \R) right left)]
      (if (= res "ZZZ")
        current-count
        (recur res (inc current-count))))))


(with-open [rdr (clojure.java.io/reader "assets/day08/real_data.txt")]
  (let [lines (line-seq rdr)
        instructions (first lines)
        maps-lines (nthrest lines 2)
        maps (into {} (map parse-pair maps-lines))
        instr-gen (create-generator instructions)]
    (go-through-map instr-gen, maps, "AAA", 1)))
