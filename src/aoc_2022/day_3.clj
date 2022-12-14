(ns aoc-2022.day-3
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-3-1
  [
   "vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   "CrZsJsPPZsGzwwsLwLmpwMDw"
   ])

(defn input-3-1 []
  (->> "resources/day_3_1.txt"
       utils/per-line-input))

(defn- get-priority [c]
  (let [id (int c)]
    (if (Character/isUpperCase id)
      (+ 26 (inc (- id (int \A))))
      (inc (- id (int \a))))))
    

(defn- compartmentalize-line [line]
  (let [n (count line)]
    (->> line
         seq
         (split-at (/ n 2))
         (map set))))

(defn- get-duplicate [bag-sets]
  (first (clojure.set/intersection (first bag-sets) (second bag-sets))))

(defn- group-bags [lines]
  (->> lines
       (partition 3)
       (map (partial map set))))

(defn- find-badge [bag-group]
  (first (apply clojure.set/intersection bag-group)))

(defn- find-badges [bag-groups]
  (map find-badge bag-groups))

(defn part-1 [input]
  (->> input
       (map compartmentalize-line)
       (map get-duplicate)
       (map get-priority)
       (apply +)))

(defn part-2 [input]
  (->> input
       group-bags
       find-badges
       (map get-priority)
       (apply +)))

(defn day-3-1 []
  (part-1 (input-3-1)))

(defn day-3-2 []
  (part-2 (input-3-1)))
