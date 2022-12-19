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

(defn- manhattan-distance
  ([^ints from ^ints to] (manhattan-distance
                          (get-x from) (get-y from)
                          (get-x to)   (get-y to)
                          ))

  ([
    ^Integer from-x ^Integer from-y
    ^Integer to-x   ^Integer to-y
    ]
   (+ (abs (- to-x from-x))
      (abs (- to-y from-y))))
  )

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
  ;; TODO consider using a map from :idx to sensor if needed
  (map-indexed parse-sensor-line lines))

(defn filter-out-beacons [sensors coords]
  (let [beacons (into #{} (map :beacon sensors))]
    (clojure.set/difference coords beacons)))

(defn- row-chunk [row-y center-x d]
  (let [r (range (- center-x d) (inc (+ center-x d)))]
    (into #{} (map #(make-coord % row-y) r))))

(defn build-impossible-location-line [row-y sensors]
  (->> sensors
       (reduce (fn [il-set {:keys [sensor beacon]}]
                 (let [sensor-x (get-x sensor)
                       sensor-y (get-y sensor)
                       d        (manhattan-distance sensor beacon)
                       dy       (abs (- row-y sensor-y))
                       rem      (- d dy)
                       coords   (if (neg? rem) #{} (row-chunk row-y sensor-x rem))]
                   (clojure.set/union il-set coords)))
               #{})
       (filter-out-beacons sensors)
       ))

(defn part-1 [input y]
  (->> input
       parse-sensor-input
       (build-impossible-location-line y)
       count
       ))

(defn day-15-1 []
  (part-1 (input-15-1) 2000000)
  )

(defn day-15-2 []
  )
