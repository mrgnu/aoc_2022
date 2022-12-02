(ns aoc-2022.day-2
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-2-1
  [
   "A Y"
   "B X"
   "C Z"
   ])

(defn input-2-1 []
  (->> "resources/day_2_1.txt"
       utils/per-line-input))

(def sym-map
  {
   \A :rock
   \X :rock
   \B :paper
   \Y :paper
   \C :scissors
   \Z :scissors
   })

(defn- parse-line [line]
  (let [[a _ b] line]
    {
     :opponent (get sym-map a)
     :me       (get sym-map b)
     }))

(defn parse-input [lines]
  (map parse-line lines)
  )

(def shape-score-map
  {
   :rock     1
   :paper    2
   :scissors 3
   })

(def win-map
  {
   :rock     :paper
   :paper    :scissors
   :scissors :rock
   })

(defn- is-win? [me opponent]
  (= me (opponent win-map)))

(defn- result-score [round]
  (let [o (:opponent round)
        m (:me       round)]
    (cond
      ;; tie
      (= o m) 3
      ;; win
      (is-win? m o) 6
      ;; loss
      :else 0)))

(defn compute-score [round]
  (+ ((:me round) shape-score-map)
     (result-score round)))

(defn compute-scores [rounds]
  (->> rounds
       (map compute-score)
       (apply +)))

(defn part-1 [input]
  (->> input
       parse-input
       compute-scores))

(defn day-2-1 []
  ;;; 14827
  (part-1 (input-2-1))
  )

(defn day-2-2 []
  )
