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

(def sand-start-position (make-coord 500 0))

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

(defn part-1-sand-fall [cave-map sand-pos max-y]
  (loop [sand-pos sand-pos]
    (let [x     (get-x sand-pos)
          y     (get-y sand-pos)]
      (if (>= y max-y)
        ;; abyss reached
        nil
        (let [below (make-coord x (inc y))]
          (if-not (contains? cave-map below)
            (recur below)
            (let [bl (make-coord (dec x) (inc y))]
              (if-not (contains? cave-map bl)
                (recur bl)
                (let [br (make-coord (inc x) (inc y))]
                  (if-not (contains? cave-map br)
                    (recur br)
                    (assoc cave-map sand-pos :sand)))))))))))

(defn part-1-sand-simulation [input]
  (let [cave-map (read-cave-map input)
        max-y (->> cave-map keys (map get-y) sort last)]
    (loop [cave-map cave-map]
      (let [new-cave-map (simulate-sand-fall cave-map sand-start-position max-y)]
        (if-not new-cave-map
          (->> cave-map vals (filter (partial = :sand)) count)
          (recur new-cave-map))))))

(defn part-2-sand-fall [cave-map sand-pos floor-y]
  (loop [sand-pos sand-pos]
    (let [x     (get-x sand-pos)
          y     (get-y sand-pos)]
      (if (= (inc y) floor-y)
        ;; floor reached
        (assoc cave-map sand-pos :sand)
        (let [below (make-coord x (inc y))]
          (if-not (contains? cave-map below)
            (recur below)
            (let [bl (make-coord (dec x) (inc y))]
              (if-not (contains? cave-map bl)
                (recur bl)
                (let [br (make-coord (inc x) (inc y))]
                  (if-not (contains? cave-map br)
                    (recur br)
                    (assoc cave-map sand-pos :sand)))))))))))

(defn part-2-sand-simulation [input]
  (let [cave-map (read-cave-map input)
        floor-y (->> cave-map keys (map get-y) sort last (+ 2))]
    (reduce (fn [cave-map _]
              (let [cave-map (part-2-sand-fall cave-map sand-start-position floor-y)]
                (if (= (get cave-map sand-start-position) :sand)
                  (reduced (->> cave-map vals (filter (partial = :sand)) count))
                  cave-map)))
            cave-map
            (range))))

(defn print-cave-line [cave-map min-x max-x y]
  (reduce (fn [s x]
            (str s
                 (condp = (get cave-map (make-coord x y))
                   :rock "#"
                   :sand "o"
                   ".")))
          ""
          (range min-x (inc max-x))))

(defn print-cave-map [cave-map]
  (let [
        min-x (->> cave-map keys (map get-x) sort first)
        max-x (->> cave-map keys (map get-x) sort last)
        min-y (->> cave-map keys (map get-y) sort first)
        max-y (->> cave-map keys (map get-y) sort last)
        ]
    (map (partial print-cave-line cave-map min-x max-x)
         (range min-y (inc max-y)))))

(defn day-14-1 []
  (part-1-sand-simulation (input-14-1))
  )

(defn day-14-2 []
  (part-2-sand-simulation (input-14-1))
  )
