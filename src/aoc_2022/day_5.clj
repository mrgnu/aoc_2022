(ns aoc-2022.day-5
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-5-1
  [
   "    [D]    "
   "[N] [C]    "
   "[Z] [M] [P]"
   " 1   2   3 "
   ""
   "move 1 from 2 to 1"
   "move 3 from 1 to 3"
   "move 2 from 2 to 1"
   "move 1 from 1 to 2"
   ])

(defn input-5-1 []
  (->> "resources/day_5_1.txt"
       utils/per-line-input))

(defn- get-cargo-state [input]
  (->> input
       (take-while (comp not empty?))))

(defn- get-moves [input]
  (->> input
       (drop-while (comp not empty?))
       rest))

;; build a map from tower (as keyword) to (empty) list of crates
(defn- parse-tower-names [line]
  (->> (str line " ") ;; make sure each column is 4 chars
       (partition 4)  ;; at this point, 2nd char in each item is tower name
       (map #(nth % 1))
       (map str)
       (map keyword)))

(defn- parse-cargo-line [line]
  (->> (str line " ") ;; make sure each column is 4 chars
       (partition 4)  ;; at this point, 2nd char in each item is either \space or name of crate
       (map #(nth % 1))))

(defn- parse-cargo-state [lines]
  (let [[tower-name-line & cargo-lines] (reverse lines)
        tower-names (parse-tower-names tower-name-line)
        cargo-row-specs (map parse-cargo-line cargo-lines)
        ;; cols is a seq of {:tower-name <name>, :boxes <vector of boxes in tower from bottom to top>}
        cols (map-indexed (fn [index tower-name]
                            (let [boxes-in-col
                                  (into []
                                        (take-while (partial not= \space)
                                                    (map #(nth % index) cargo-row-specs)))]
                              {
                               :tower-name tower-name
                               :boxes boxes-in-col
                               }))
                          tower-names)]
    (reduce (fn [tower-map col] (assoc tower-map
                                       (:tower-name col)
                                       (:boxes col)))
            {}
            cols)))

(defn- parse-move [line]
  (let [[_ tower from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]
    {
     :to-move (Integer. tower)
     :from (keyword from)
     :to (keyword to)
     }))

(defn- parse-moves [lines]
  (map parse-move lines))

(defn- one-at-the-time-executor [tower-map move]
  (let [{:keys [to-move from to]} move]
    (reduce (fn [tower-map _]
              (let [crate (last (from tower-map))]
                (-> tower-map
                    (update ,,, from (comp vec butlast))
                    (update ,,, to #(conj % crate))
                    )))
            tower-map
            (range to-move))))

(defn- execute-moves [move-executor tower-map moves]
  (reduce move-executor
          tower-map
          moves))

(defn- execute-drawing [move-executor input]
  (let [tower-map (parse-cargo-state (get-cargo-state input))
        moves     (parse-moves (get-moves input))]
    (execute-moves move-executor tower-map moves)))

(defn- get-top-states [tower-map]
  (let [sorted-keys (->> tower-map keys sort)]
    (reduce (fn [acc tower]
              (str acc (if (empty? tower) "" (last tower))))
            ""
            (map #(get tower-map %) sorted-keys))))

(defn part-1 [input]
  (->> input
       (execute-drawing one-at-the-time-executor)
       get-top-states))

(defn day-5-1 []
  (part-1 (input-5-1))
  )

(defn day-5-2 []
  )
