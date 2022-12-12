(ns aoc-2022.a-star
  (:require [clojure.data.priority-map :refer [priority-map]]
            )
  )

(defn- make-a-star-data [from to cost-fun heuristic neighbor-fun]
  {
   :from         from
   :to           to
   :g-score      {from 0}
   ;; priority queue with mappings from open -> cost
   :f-score      (priority-map from (heuristic from to))
   :came-from    {}
   :heuristic    heuristic
   :cost-fun     cost-fun
   :neighbor-fun neighbor-fun
   })

(defn a-star-find-current
  ;; find next node to visit, and remove from open priority queue
  ;; returns [a-star-data current]
  [{:keys [to f-score] :as a-star-data}]
  (let [current (->> f-score first first)
        f-score (dissoc f-score current)]
    [(assoc a-star-data :f-score f-score)
     current]))

(defn a-star-update-g-score [c-score
                             current
                             {:keys [to g-score heuristic cost-fun] :as a-star-data}
                             neighbor]
  (let [cost (cost-fun current neighbor)
        g    (get g-score neighbor Integer/MAX_VALUE)
        tg   (+ c-score cost)
        ]
    (if (< tg g)
      (let [f-score (assoc (:f-score a-star-data) neighbor (+ tg (heuristic neighbor to)))]
        (-> a-star-data
            (assoc-in  ,,, [:came-from neighbor] current)
            (assoc-in  ,,, [:g-score   neighbor] tg)
            (assoc     ,,, :f-score    f-score)
            ))
      a-star-data)))

(defn- a-star-update-g-scores [{:keys [to heuristic neighbor-fun] :as a-star-data}
                               current]
  (assert (contains? (:g-score a-star-data) current))
  (let [c-score   (get (:g-score a-star-data) current)
        neighbors (neighbor-fun current)]
    (reduce (partial a-star-update-g-score c-score current)
            a-star-data
            neighbors)))

(defn- a-star-reconstruct-path [{:keys [from to came-from] :as a-star-data}]
  (loop [path  [to]
         coord to]
    (if (= coord from)
      (reverse path)

      (let [f (get came-from coord)]
        (recur (conj path f) f)))))

(defn a-star

  ([from to cost-fun heuristic neighbor-fun]
   (a-star (make-a-star-data from to cost-fun heuristic neighbor-fun)))

  ([{:keys [to] :as a-star-data}]
   (when (empty? (:f-score a-star-data)) (throw (AssertionError. "no path found")))

   ;; find and remoe coord in open priority queue with least estimated cost
   (let [[a-star-data current] (a-star-find-current a-star-data)]

     ;; to reached - reconstruct path
     (if (= current to)
       (a-star-reconstruct-path a-star-data)

       ;; update g-scores
       (let [a-star-data (a-star-update-g-scores a-star-data current)]
         (recur a-star-data)))))
  )
