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

(defn- range-size [from to] (inc (- to from)))

(defn- make-range [from to]
  {
   :from from
   :to   to
   :size (range-size from to)
   })

(defn- range-size [from to] (inc (- to from)))

(defn- make-range [from to]
  {
   :from from
   :to   to
   :size (range-size from to)
   })

(defn- merge-ranges [ranges]
  (if (<= (count ranges) 1)
    ranges
    (let [sorted (sort-by :from ranges)]
      (reduce (fn [rs r]
                (let [prev (last rs)]
                  (if (<= (:from r) (inc (:to prev)))
                    (let [to (max (:to prev) (:to r))]
                      (conj (vec (butlast rs)) (assoc prev :to to)))
                    (conj (vec rs) r))))
              [(first sorted)]
              (rest sorted)))))

(defn- range-contains? [{:keys [from to]} v]
  (<= from v to))

(defn- split-range [{:keys [from to size] :as r} v]
  (cond
    (not (range-contains? r v)) [r]
    (= 1 size) []
    (= v from) [(make-range (inc from) to)]
    (= v to)   [(make-range from (dec to))]
    :else
    [
     (make-range from (dec v))
     (make-range (inc v) to)
     ]))

(defn- count-without-beacons [^Integer row-y sensors ranges]
  (let [beacons (get-beacons sensors)
        ranges (map (fn [{:keys [from to] :as r}]
                      (assoc r :size (inc (- to from))))
                    ranges)
        counts (map (fn [{:keys [from to size] :as r}]
                      (let [beacon-count (->> beacons
                                              (filter (comp (partial = row-y) get-y))
                                              (map get-x)
                                              (filter (partial range-contains? r))
                                              count)]
                        (- size beacon-count)))
                    ranges)]
    (apply + counts)))

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
        (make-range from to)))))

(defn- get-x-ranges [^Integer row-y sensors]
  (->> sensors
       (reduce (fn [ranges {:keys [sensor beacon]}]
                 (if-let [r (get-x-range row-y sensor beacon)]
                   (conj ranges r)
                   ranges))
               #{})
       merge-ranges))

(defn collect-impossible-ranges [^Integer row-y sensors]
  (let [row-beacons (->> sensors
                         get-beacons
                         (filter (comp (partial = row-y) get-y)))
        ranges      (get-x-ranges row-y sensors)]
    (reduce (fn [rs beacon-x]
              (apply concat
                     (map #(split-range % beacon-x) rs)))
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

(defn day-15-2 []
  )
