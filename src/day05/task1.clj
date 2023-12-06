(ns day05.task1
  (:require [clojure.string :as str])
  )

(defn get-rest-lines [lines]
  (drop-while #(not (re-find #" map:" %)) (rest lines))
  )

(defn get-map [lines]
  (->> (rest lines)
       (take-while #(not (re-find #" map:" %)))
       (drop-last)
       (mapv #(mapv parse-long (str/split % #"\s+")))))
   
(defn get-value [value, current_map]
  (let [[dest source length] current_map
        diff (- value source)]
    (if (and (>= value source) (>= length diff))
      (+ dest diff))))

(defn get-next-value[value, maps]
  (let [result (remove nil? (map (partial get-value value) maps))]
    (if (empty? result) value (first result))
    )
)

(defn go-through-maps[value, maps]
  (let [next-value (get-next-value value (first maps))
        ]
    (if (= (count maps) 1)
      next-value
      (go-through-maps next-value (rest maps)) 
    )
  )
)

(with-open [rdr (clojure.java.io/reader "assets/day05/real_data.txt")]
  (let [lines (line-seq rdr)
        seed-line (first lines)
        seed-to-soil-lines (get-rest-lines lines)
        soil-to-fertilizer-lines (get-rest-lines seed-to-soil-lines)
        fertilizer-to-water-lines (get-rest-lines soil-to-fertilizer-lines)
        water-to-light-lines (get-rest-lines fertilizer-to-water-lines)
        light-to-temperature-lines (get-rest-lines water-to-light-lines)
        temperature-to-humidity-lines (get-rest-lines light-to-temperature-lines)
        humidity-to-location-lines (get-rest-lines temperature-to-humidity-lines)

        seeds (mapv parse-long (rest (str/split seed-line #"\s+")))
        seed-to-soils (get-map seed-to-soil-lines)
        soil-to-fertilizers (get-map soil-to-fertilizer-lines)
        fertilizer-to-waters (get-map fertilizer-to-water-lines)
        water-to-lights (get-map water-to-light-lines)
        light-to-temperatures (get-map light-to-temperature-lines)
        temperature-to-humidities (get-map temperature-to-humidity-lines)
        humidity-to-locations (->>
                               (rest humidity-to-location-lines)
                               (mapv #(mapv parse-long (str/split % #"\s+"))))

        maps [seed-to-soils soil-to-fertilizers fertilizer-to-waters water-to-lights light-to-temperatures temperature-to-humidities humidity-to-locations]]

    (apply min (map #(go-through-maps % maps) seeds))
    ))