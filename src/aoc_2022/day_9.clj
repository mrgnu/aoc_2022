(ns aoc-2022.day-9
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-9-1
  [
   "R 4"
   "U 4"
   "L 3"
   "D 1"
   "R 4"
   "D 1"
   "L 5"
   "R 2"
   ])

(defn input-9-1 []
  (->> "resources/day_9_1.txt"
       utils/per-line-input))

(defn parse-move-spec [line]
  (let [[dir steps] (clojure.string/split line #"\s+")]
    {
     :direction (get {"R" :right, "U" :up, "L" :left, "D" :down} dir)
     :steps     (clojure.edn/read-string steps)
     }))

(defn move-spec-to-moves
  "converts {:direction <dir>, :steps <n>} to (<n> * <dir>)"
  [{:keys [direction steps] :as move-spec}]
  (take steps (repeat direction)))

(defn parse-moves [lines]
  (->> lines
       (map parse-move-spec)
       (map move-spec-to-moves)
       (reduce concat [])
       ))

(defn- make-pos [x y]
  [x y])

(defn- move-by [pos delta] (vec (map + pos delta)))

(def move-delta-map { :left [-1, 0], :right [1, 0], :up [0, -1], :down [0, 1] })

(defn- move-head [head-pos move] (move-by head-pos (move move-delta-map)))

(defn- sign [v]
  (cond
    (zero? v) 0
    (neg?  v) -1
    :else     1))

(defn- len [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

;; TODO: this is messy :/
(defn- move-tail [head-pos tail-pos]
  (let [delta (map - head-pos tail-pos)
        l     (len delta)]
    (if (< l 2.0)
      tail-pos
      (do (assert (<= l 3)
                  (format "lazy code only supports len <=3, is %d" len))
          (let [delta (->> delta
                           (map sign)
                           vec)]
            (move-by tail-pos delta))))))

(defn- perform-move [{:keys [head-pos tail-pos tail-positions] :as state}
                     move]
  (let [head-pos (move-head head-pos move)
        tail-pos (move-tail head-pos tail-pos)]
    (-> state
        (assoc ,,, :head-pos head-pos)
        (assoc ,,, :tail-pos tail-pos)
        (update ,,, :tail-positions conj tail-pos))))

(defn perform-moves [moves]
  (let [state {
               :head-pos       (make-pos 0 0)
               :tail-pos       (make-pos 0 0)
               :tail-positions #{}
               }]
    (reduce perform-move state moves)))

(defn part-1 [input]
  (->> input
       parse-moves
       perform-moves
       :tail-positions
       count))

(defn day-9-1 []
  (part-1 (input-9-1))
  )

(defn day-9-2 []
  )
