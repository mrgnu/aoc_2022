(ns aoc-2022.day-6
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-6-1
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  )

(defn input-6-1 []
  (->> "resources/day_6_1.txt"
       utils/per-line-input
       first))

(defn- get-blocks [msg-size input]
  (->> input
       (partition msg-size 1)
       (map-indexed vector)))

(defn- all-same [block]
  (let [chars (second block)]
    (= (count chars)
       (count (set chars)))))

(defn find-msg [msg-size input]
  (->> input
       (get-blocks msg-size)
       (filter all-same)
       first
       first
       (+ msg-size)))

(defn part-1 [input]
  (let [msg-size 4]
    (->> input
         (find-msg msg-size)
         )))

(defn day-6-1 []
  (part-1 (input-6-1))
  )

(defn day-6-2 []
  )
