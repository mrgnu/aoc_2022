(ns aoc-2022.day-8
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-8-1
  [
   "30373"
   "25512"
   "65332"
   "33549"
   "35390"
   ])

(defn input-8-1 []
  (->> "resources/day_8_1.txt"
       utils/per-line-input))

(defn make-coord [x y] {:x x :y y})

(defn get-map-size [rows]
  {
   :w (count (first rows))
   :h (count rows)
   })

(defn get-edge-tree-count [{:keys [w h]}]
  (- (* 2 (+ w h)) 4))

(defn- get-coords [{:keys [w h]}]
  (into #{}
        (let [x-range (range 1 w)
              y-range (range 1 h)]
          (for [x (range 1 (dec w))
                y (range 1 (dec h))]
            (make-coord x y)))))

(defn get-adjacent [{:keys [x y]}]
  #{
    (make-coord (dec x) y)
    (make-coord (inc x) y)
    (make-coord x       (dec y))
    (make-coord x       (inc y))
    })

(defn- add-tree [tree-map coord height] (assoc tree-map coord height))

(defn- parse-map-row [row y]
  (->> row
       (map #(Character/digit % 10))
       (map-indexed vector)
       (reduce (fn [row-map [x h]]
                 (add-tree row-map (make-coord x y) h))
               {})))

(defn parse-map [rows]
  (->> rows
       (map-indexed vector)
       (reduce (fn [tree-map [y row]]
                 (merge tree-map
                        (parse-map-row row y)))
               {})))

(defn tree-visible? [tree-map
                     {:keys [w h] :as map-size}
                     {:keys [x y] :as coord}]
  (let [row      (->> (map #(make-coord % y) (range 0 w)))
        left     (take x row)
        right    (drop (inc x) row)
        col      (->> (map #(make-coord x %) (range 0 h)))
        above    (take y col)
        below    (drop (inc y) col)
        height   (get tree-map coord)
        smaller? (fn [trees] (every? #(< (get tree-map %) height) trees))]
    (or (smaller? left)
        (smaller? right)
        (smaller? above)
        (smaller? below))))

(defn get-interior-visible-tree-count [tree-map map-size]
  (->> (get-coords map-size)
       (map (partial tree-visible? tree-map map-size))
       (filter true?)
       count
       ))

(defn part-1 [input]
  (let [map-size (get-map-size input)
        tree-map (parse-map input)]
    (+ (get-edge-tree-count map-size)
       (get-interior-visible-tree-count tree-map map-size))))

(defn day-8-1 []
  (part-1 (input-8-1))
  )

(defn day-8-2 []
  )
