;; utils for dealing with inclusive ranges

(ns aoc-2022.range
  )

(defn- range-size [^Long from ^Long to] (inc (- to from)))

(defn make-range
  "create an inclusive range. `from' must be <= `to'."
  [^Long from ^Long to]
  (assert (<= from to) "invalid range")
  {
   :from from
   :to   to
   :size (range-size from to)
   })

(defn range-seq
  "returns a lazy seq of all vaules in range."
  [{:keys [^Long from ^Long to]}]
  (range from (inc to)))

(defn range-contains?
  "true if range contains v"
  [{:keys [^Long from ^Long to]} ^Long v]
  (<= from v to))

(defn merge-ranges
  "merge a seq of - presumably overlapping - ranges into a sorted
  seq of ranges with no overlaps"
  [ranges]
  (if (<= (count ranges) 1)
    ranges
    (let [sorted (sort-by :from ranges)]
      (reduce (fn [rs r]
                (let [prev (last rs)]
                  (if (<= (:from r) (inc (:to prev)))
                    (let [from (min (:from prev) (:from r))
                          to   (max (:to prev) (:to r))]
                      (conj (vec (butlast rs))
                            (make-range from to)))
                    (conj (vec rs) r))))
              [(first sorted)]
              (rest sorted)))))

(defn crop-range
  "crop range. if range is completely outside of boundaries, returns nil."
  [^Long min-val ^Long max-val {:keys [^Long from ^Long to ^Long size] :as r}]
  (cond
    (< to   min-val) nil
    (> from max-val) nil
    :else (make-range (max min-val from) (min max-val to))))

(defn crop-ranges
  "crop a seq of ranges."
  [^Long min-val ^Long max-val ranges]
  (->> ranges
       (map (partial crop-range min-val max-val))
       (filter identity)
       ))

(defn split-range
  "splits range at `v'. returns a - possibly empty - vec of ranges.
  if v is outside of range bounds the passed range is returned as a vec.
  if v is on an edge, a single, shrunken range is returned as a vec.
  if v is within range, but not on an edge, two new ranges are returned.
  if range is of size one, and v is in range, an empty vec is returned. "
  [{:keys [^Long from ^Long to ^Long size] :as r} ^Long v]
  (cond
    (not (range-contains? r v)) [r]
    (= 1 size) []
    (= v from) [(make-range (inc from) to)]
    (= v to)   [(make-range from (dec to))]
    :else
    [
     (make-range from (dec v))
     (make-range (inc v) to)
     ]))
