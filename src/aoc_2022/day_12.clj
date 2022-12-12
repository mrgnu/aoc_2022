(ns aoc-2022.day-12
  (:require [aoc-2022.utils :as utils]
            [aoc-2022.a-star :as a-star]
            )
  )

(def test-input-12-1
  [
   "Sabqponm"
   "abcryxxl"
   "accszExk"
   "acctuvwj"
   "abdefghi"
   ])

(defn input-12-1 []
  (->> "resources/day_12_1.txt"
       utils/per-line-input))

(defn- make-coord ^ints [^Integer x ^Integer y]
  [x y])

(defn- read-map-row [y line]
  (reduce (fn [m [x c]]
            (assoc m (make-coord x y) c))
          {}
          (map-indexed vector line)))

(defn- height-of [c]
  (- (Character/getNumericValue c) (Character/getNumericValue \a)))

(defn read-map
  ([lines] (read-map \a \z lines))

  ([start-height end-height lines]
   (let [char-map   (reduce (fn [m [y line]]
                              (merge m (read-map-row y line)))
                            {}
                            (map-indexed vector lines))
         start-pos  (->> char-map (some #(when (= \S (val %)) %)) key)
         end-pos    (->> char-map (some #(when (= \E (val %)) %)) key)
         char-map   (-> char-map
                        (assoc ,,, start-pos \a)
                        (assoc ,,, end-pos   \z))
         height-map (reduce (fn [m e]
                              (assoc m (key e) (height-of (val e))))
                            {}
                            char-map)
         ]
     {
      :start-pos  start-pos
      :end-pos    end-pos
      :height-map height-map
      }))
  )

(defn- get-adjacent [[x y]]
  #{
    [(dec x) y]
    [(inc x) y]
    [x       (dec y)]
    [x       (inc y)]
    })

(defn- can-climb [height-map h coord]
  (let [th (get height-map coord)]
    (and th
         (<= (- th h) 1))))

(defn- get-neighbors [height-map coord]
  (let [h (get height-map coord)]
    (->> (get-adjacent coord)
         (filter (partial can-climb height-map h))
         set
         )
    ))

(defn- manhattan-heuristic [[fx fy] [tx ty]]
  (+ (abs (- tx fx))
     (abs (- ty fy))))

(defn- get-steps [height-map start-pos end-pos]
  (try
    (->> (a-star/a-star start-pos
                        end-pos
                        (fn [_ _] 1)
                        manhattan-heuristic
                        (partial get-neighbors height-map))
         count
         dec
         )
    (catch Throwable e nil)
    )
  )

(defn part-1 [input]
  (let [{:keys [height-map start-pos end-pos]} (read-map input)]
    (get-steps height-map start-pos end-pos)))

(defn day-12-1 []
  (part-1 (input-12-1))
  )

(defn day-12-2 []
  )
