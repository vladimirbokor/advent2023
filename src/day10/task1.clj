(ns day10.task1
  (:require [clojure.string :as str])
  )

(defn find-start-position [lines]
  (->>
   (map-indexed (fn [idx, line] [idx, (str/index-of line \S)]) lines)
   (filter #(not (nil? (second %))))
  )
  )

(defn find-next-move [lines, current-idx, prev-move]
  (let [[cur-str, cur-pos] current-idx]
    ;; Here will be solution...or not
    )
  )


(with-open [rdr (clojure.java.io/reader "assets/day10/test_input.txt")]
  (let [lines (vec (line-seq rdr))
        start-position (find-start-position lines)]
    (println (find-start-position lines))
    
    (println (get lines 3))
    )
  
  )