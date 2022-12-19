(ns aoc-2022.day-15
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-15-1
  [
   "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
   "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
   "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
   "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
   "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
   "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
   "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
   "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
   "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
   "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
   "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
   "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
   "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
   "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
   ])

(defn input-15-1 []
  (->> "resources/day_15_1.txt"
       utils/per-line-input))

(defn- make-coord ^ints [^Integer x ^Integer y]
  [x y])

(defn- get-x ^Integer [^ints coord]
  (first coord))

(defn- get-y ^Integer [^ints coord]
  (last coord))

(defn- parse-sensor-line [idx line]
  (let [[_ sensor-x sensor-y beacon-x beacon-y]
        (re-matches #"Sensor at x=([+-]?\d+), y=([+-]?\d+): closest beacon is at x=([+-]?\d+), y=([+-]?\d+)"
                    line)]
    {
     :idx    idx
     :sensor (make-coord (Integer. sensor-x) (Integer. sensor-y))
     :beacon (make-coord (Integer. beacon-x) (Integer. beacon-y))
     }))

(defn parse-sensor-input [lines]
  (map-indexed parse-sensor-line lines))

(defn day-15-1 []
  )

(defn day-15-2 []
  )
