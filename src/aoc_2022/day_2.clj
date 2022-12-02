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

(defn parse-input [line-parser lines]
  (map line-parser lines)
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

(defn- achieve-outcome [opponent outcome]
  (condp = outcome
    :draw opponent
    :win  (win-map opponent)
    :lose (-> (keys win-map)
              set
              (disj opponent)
              (disj (opponent win-map))
              first)))

(def strategy-map
  {
   \X :lose
   \Y :draw
   \Z :win
   })

(defn- strategy-parser [line]
  (let [[a _ b] line
        opponent (get sym-map a)
        strategy (get strategy-map b)
        me       (achieve-outcome opponent strategy)]
    {
     :opponent opponent
     :me       me
     }))

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
       (parse-input parse-line)
       compute-scores))

(defn part-2 [input]
  (->> input
       (parse-input strategy-parser)
       compute-scores))

(defn day-2-1 []
  (part-1 (input-2-1))
  )

(defn day-2-2 []
  (part-2 (input-2-1))
  )
