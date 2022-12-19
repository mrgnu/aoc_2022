(ns aoc-2022.day-14
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-14-1
  [
   "498,4 -> 498,6 -> 496,6"
   "503,4 -> 502,4 -> 502,9 -> 494,9"
   ])

(defn input-14-1 []
  (->> "resources/day_14_1.txt"
       utils/per-line-input))

(defn- make-coord ^ints [^Integer x ^Integer y]
  [x y])

(defn- get-x ^Integer [^ints coord]
  (first coord))

(defn- get-y ^Integer [^ints coord]
  (last coord))

(defn- parse-coord [s]
  (let [[_ x y] (re-matches #"(\d+),(\d+)" s)
        x (Integer. x)
        y (Integer. y)]
    (make-coord x y)))

(defn- parse-range [[from-str to-str]]
  (let [^ints from (parse-coord from-str)
        ^ints to   (parse-coord to-str)]
    (cond
      (= (get-x from) (get-x to))
      {:direction :y :from (get-y from) :to (get-y to) :fixed (get-x from)}

      (= (get-y from) (get-y to))
      {:direction :x :from (get-x from) :to (get-x to) :fixed (get-y from)}

      :else (throw (AssertionError.
                    (format "expected horizontal or vertical range. got %s -> %s" from to)))
      )))

(defn- abs-inclusive-range [a b]
  (if (<= a b)
    (range a (inc b))
    (range b (inc a))))

(defn- range-to-coords [{:keys [direction ^Integer from ^Integer to ^Integer fixed] :as coord-range}]
  (let [r (abs-inclusive-range from to)]
    (condp = direction
      :y (map (partial make-coord fixed) r)
      :x (map #(make-coord % fixed) r)
      (throw (AssertionError.
              (format "invalid range: %s" coord-range))))))

(defn- get-line-coords [line]
  (->> (clojure.string/split line #"\s+->\s+")
       (partition 2 1)
       (map parse-range)
       (map range-to-coords)
       (map set)
       (apply clojure.set/union)
       ))

(defn read-cave-map [input]
  (->> input
       (map get-line-coords)
       (reduce (fn [cave-map coords]
                 (reduce (fn [cave-map coord] (assoc cave-map coord :rock))
                         cave-map
                         coords))
               {})
       ))

(defn day-14-1 []
  )

(defn day-14-2 []
  )
