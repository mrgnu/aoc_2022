(ns aoc-2022.day-4
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-4-1
  [
   "2-4,6-8"
   "2-3,4-5"
   "5-7,7-9"
   "2-8,3-7"
   "6-6,4-6"
   "2-6,4-8"
   ])

(defn input-4-1 []
  (->> "resources/day_4_1.txt"
       utils/per-line-input))

(defn- range-to-seq [range-spec]
  (let [parts (clojure.string/split range-spec #"-")
        [l u] (map #(Integer. %) parts)]
    (range l (inc u))))

(defn- parse-section-pair [line]
  (let [section-pair (clojure.string/split line #",")]
    (map range-to-seq section-pair)))

(defn- total-overlap? [section-pair]
  (let [[s1 s2] (map set section-pair)]
    (or (empty? (clojure.set/difference s1 s2))
        (empty? (clojure.set/difference s2 s1)))))

(defn part-1 [input]
  (->> input
       (map parse-section-pair)
       (filter total-overlap?)
       count))

(defn day-4-1 []
  (part-1 (input-4-1))
  )

(defn day-4-2 []
  )
