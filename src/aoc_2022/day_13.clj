(ns aoc-2022.day-13
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-13-1
  [
   "[1,1,3,1,1]"
   "[1,1,5,1,1]"
   ""
   "[[1],[2,3,4]]"
   "[[1],4]"
   ""
   "[9]"
   "[[8,7,6]]"
   ""
   "[[4,4],4,4]"
   "[[4,4],4,4,4]"
   ""
   "[7,7,7,7]"
   "[7,7,7]"
   ""
   "[]"
   "[3]"
   ""
   "[[[]]]"
   "[[]]"
   ""
   "[1,[2,[3,[4,[5,6,7]]]],8,9]"
   "[1,[2,[3,[4,[5,6,0]]]],8,9]"
   ])

(defn input-13-1 []
  (->> "resources/day_13_1.txt"
       utils/per-line-input))

(defn parse-input [lines]
  (->> lines
       (filter (comp not empty?))
       (map clojure.edn/read-string)
       )
  )

(defn- valid-numbers? [[a b]]
  (cond
    (= a b) :undetermined
    (< a b) :valid
    :else   :invalid))

(declare valid-packet?)

(defn- valid-lists? [[a b]]
  (let [rs (map (fn [a b] (valid-packet? [a b])) a b)
        r  (or (some #{:valid :invalid} rs) :undetermined)]
    (if (= r :undetermined)
      (cond
        (< (count a) (count b)) :valid
        (> (count a) (count b)) :invalid
        :else                   :undetermined)
        r)))

(defn valid-packet?
  ([a b] (valid-packet? [a b]))

  ([[a b]]
   (if (= (vector? a) (vector? b))
     (if (vector? a)
       (valid-lists? [a b])
       (valid-numbers? [a b]))
     (if (vector? a)
       (valid-packet? [a   [b]])
       (valid-packet? [[a] b])))
   )
  )

(defn part-1 [input]
  (->> input
       parse-input
       (partition 2)
       (map-indexed vector)
       (filter (comp (partial = :valid) valid-packet? second))
       (map first)
       (map inc)
       (apply +)
       )
  )

(defn day-13-1 []
  (part-1 (input-13-1))
  )

(defn day-13-2 []
  )
