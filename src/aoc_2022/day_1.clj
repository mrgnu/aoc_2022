(ns aoc-2022.day-1
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-1-1
  [
   "1000"
   "2000"
   "3000"
   ""
   "4000"
   ""
   "5000"
   "6000"
   ""
   "7000"
   "8000"
   "9000"
   ""
   "10000"
   ])

(defn input-1-1 []
  (->> "resources/day_1_1.txt"
       utils/per-line-input))

(defn- sum-lines [lines]
  (apply + (map #(Integer. %) lines)))

(defn- parse-input [lines]
  (->> lines
       (partition-by empty?)
       (filter (comp not empty? first))
       (map sum-lines)))

(defn day-1-1 []
  (->> (input-1-1)
       parse-input
       (apply max))
  )

(defn day-1-2 []
  (->> (input-1-1)
       parse-input
       sort
       reverse
       (take 3)
       (apply +))
  )
