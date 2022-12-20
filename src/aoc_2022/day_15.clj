(ns aoc-2022.day-15
  (:require [aoc-2022.utils :as utils]
            [aoc-2022.range :as r]
            )
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

(defn- manhattan-distance ^Integer
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
     :sensor (make-coord (Integer. ^String sensor-x) (Integer. ^String sensor-y))
     :beacon (make-coord (Integer. ^String beacon-x) (Integer. ^String beacon-y))
     }))

(defn parse-sensor-input [lines]
  ;; TODO consider using a map from :idx to sensor if needed
  (map-indexed parse-sensor-line lines))

(defn- get-beacons [sensors]
  (->> sensors
       (map :beacon)
       (into #{})
       ))

(defn- get-x-range
  "returns range of points on row defined by row-y
  reachable from sensor
  within manhattan distance between sensor and beacon"
  [^Integer row-y ^ints sensor ^ints beacon]
  (let [sensor-x (get-x sensor)
        sensor-y (get-y sensor)
        d        (manhattan-distance sensor beacon)
        dy       (abs (- row-y sensor-y))
        rem      (- d dy)]
    (when (>= rem 0)
      (let [from (- sensor-x rem)
            to   (+ sensor-x rem)]
        (r/make-range from to)))))

(defn- get-x-ranges [^Integer row-y sensors]
  (->> sensors
       (reduce (fn [ranges {:keys [sensor beacon]}]
                 (if-let [r (get-x-range row-y sensor beacon)]
                   (conj ranges r)
                   ranges))
               #{})
       r/merge-ranges
       ))

(defn collect-impossible-ranges [^Integer row-y sensors]
  (let [row-beacons (->> sensors
                         get-beacons
                         (filter (comp (partial = row-y) get-y)))
        ranges      (get-x-ranges row-y sensors)]
    (reduce (fn [rs beacon-x]
              (apply concat
                     (map #(r/split-range % beacon-x) rs)))
            ranges
            (map get-x row-beacons))))

(defn part-1 [input y]
  (->> input
       parse-sensor-input
       (collect-impossible-ranges y)
       (map :size)
       (apply +)
       ))

(defn day-15-1 []
  (part-1 (input-15-1) 2000000)
  )

(defn- has-unique-gap? [min-val max-val ranges]
  (let [cropped (r/crop-ranges min-val max-val ranges)
        cropped (sort-by :from cropped)]
    (cond
      ;; can't have a single one size gap with more than two regions
      (> (count cropped) 2) nil
      ;; gap must be internal
      (= (count cropped) 2)
      (let [f (first cropped)
            s (last  cropped)]
        (when (and (= min-val (:from f))
                   (= max-val (:to   s))
                   (= 2 (- (:from s) (:to f))))
          (inc (:to f))))
      :else
      ;; single range
      (let [{:keys [from to]} (first cropped)]
        (cond
          (and (= from (inc min-val))
               (= to   max-val))
          min-val

          (and (= from min-val)
               (= to   (dec max-val)))
          max-val

          :else nil)))))

(defn- compute-tuning-frequency [coord]
  (+ (* (get-x coord) 4000000)
     (get-y coord)))

(defn part-2 [input max-val]
  (let [sensors (parse-sensor-input input)
        y-range (range 0 (inc max-val))
        coord   (reduce (fn [_ y]
                          (let [ranges (get-x-ranges y sensors)]
                            (when-let [x (has-unique-gap? 0 max-val ranges)]
                              (reduced (make-coord x y)))))
                        nil
                        y-range)]
    (compute-tuning-frequency coord)))

(defn day-15-2 []
  (part-2 (input-15-1) 4000000)
  )
