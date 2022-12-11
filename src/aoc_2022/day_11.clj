(ns aoc-2022.day-11
  (:require [aoc-2022.utils :as utils])
  )

(def test-input-11-1
  [
   "Monkey 0:"
   "  Starting items: 79, 98"
   "  Operation: new = old * 19"
   "  Test: divisible by 23"
   "    If true: throw to monkey 2"
   "    If false: throw to monkey 3"
   ""
   "Monkey 1:"
   "  Starting items: 54, 65, 75, 74"
   "  Operation: new = old + 6"
   "  Test: divisible by 19"
   "    If true: throw to monkey 2"
   "    If false: throw to monkey 0"
   ""
   "Monkey 2:"
   "  Starting items: 79, 60, 97"
   "  Operation: new = old * old"
   "  Test: divisible by 13"
   "    If true: throw to monkey 1"
   "    If false: throw to monkey 3"
   ""
   "Monkey 3:"
   "  Starting items: 74"
   "  Operation: new = old + 3"
   "  Test: divisible by 17"
   "    If true: throw to monkey 0"
   "    If false: throw to monkey 1"
   ])

(defn input-11-1 []
  (->> "resources/day_11_1.txt"
       utils/per-line-input))

(defn- to-number [s] (clojure.edn/read-string s))

(defn- number-str? [s]
  (when-let [n (re-matches #"^\d+$" s)]
    (to-number n)))

(defn- monkey-keyword [line]
  (let [[_ id] (re-matches #"^.*[Mm]onkey (\d+).*$" line)]
    (keyword (format "monkey-%d" (to-number id)))))

(defn- read-starting-items [line]
  (let [[_ items-str] (re-matches #"^\s+Starting items:\s+(.*)$" line)
        items (clojure.string/split items-str #",\s+")]
    (map to-number items)))

(defn- get-op
  "returns:
  - symbol for defined function
  - number for number
  - keyword for anything else (presumably a var)"
  [op-str]
  (try (let [sym (symbol op-str)]
         (do (eval (symbol op-str))
             sym))
       (catch Exception e
         (or (number-str? op-str)
             (keyword op-str)))))

(defn- build-compound-expression [ops]
  ;; TODO: order of operations
  (loop [lhs (first ops)
         [op rhs & ops] (rest ops)]
    (let [o (list op lhs rhs)]
      (if (empty? ops)
        o
        (recur o ops)))))

(defn- resolve-var [var-map e]
  (if (keyword? e)
    (e var-map)
    e))

(defn- resolve-vars [var-map e]
  (clojure.walk/postwalk (partial resolve-var var-map) e))

(defn eval-operation [var-map e]
  (if-not (seq? e)
    (resolve-var var-map e)
    (eval (resolve-vars var-map e))))

(defn- read-operation [line]
  (let [[_ op-str] (re-matches #"^\s+Operation:\s+(.*)$" line)
        [_ rhs]    (re-matches #"^new = (.*)$" op-str)
        parts      (clojure.string/split rhs #" ")
        parts      (map get-op parts)]
    (assert (= 1 (mod (count parts) 2))
            (format "'%s' doesn't seem like a valid operation" line))
    (if (= 1 (count parts))
      (first parts)
      (let [ops (map first (partition 1 2 (rest parts)))]
        (assert (every? symbol? ops)
                (format "'%s' doesn't seem like a valid operation" line))
        (build-compound-expression parts)))))

(defn- read-test [[test-line if-line else-line]]
  (let [[_ div-by]  (re-matches #"^\s+Test: divisible by (\d+)$" test-line)
        div-by        (to-number div-by)
        if-moneky   (monkey-keyword if-line)
        else-moneky (monkey-keyword else-line)]
    {
     :div-by  div-by
     :if-id   if-moneky
     :else-id else-moneky
     }))

(defn read-monkey [lines]
  (let [monkey (monkey-keyword      (nth lines 0))
        items  (read-starting-items (nth lines 1))
        op     (read-operation      (nth lines 2))
        test   (read-test           (->> lines (drop 3) (take 3)))
        ]
    (-> {:monkey monkey}
        (merge test)
        (assoc ,,, :items            (vec items))
        (assoc ,,, :op               op)
        (assoc ,,, :inspection-count 0)
        )))

(defn read-monkeys [lines]
  (->> lines
       (partition-by empty?)
       (filter (comp (partial < 1) count))
       (map read-monkey)
       (reduce (fn [mm m]
                 (assoc mm (:monkey m) m))
               {})
       ))

(defn- find-target [{:keys [div-by if-id else-id]} worry]
  (if (zero? (mod worry div-by)) if-id else-id))

(defn throw-item [worry-reduction mm monkey-id]
  (let [{:keys [items op] :as monkey} (monkey-id mm)
        [item & items]                items
        worry                         (eval-operation {:old item} op)
        worry                         (worry-reduction worry)
        target                        (find-target monkey worry)]
    (-> mm
        (update-in ,,, [monkey-id :inspection-count] inc)
        (assoc-in  ,,, [monkey-id :items] (vec items))
        (update-in ,,, [target    :items] conj worry)
        )))

(defn throw-items [worry-reduction mm monkey-id]
  (loop [mm mm]
    (if (empty? (get-in mm [monkey-id :items]))
      mm
      (recur (throw-item worry-reduction mm monkey-id)))))

(defn throw-round [worry-reduction mm]
  (let [monkey-ids (-> mm keys sort)]
    (reduce (partial throw-items worry-reduction) mm monkey-ids)))

(defn throw-rounds [worry-reduction n mm]
  (nth (iterate (partial throw-round worry-reduction) mm) n))

(defn part-1 [input]
  (let [worry-reduction (fn [worry] (long (/ worry 3)))
        rounds          20]
    (->> input
         read-monkeys
         (throw-rounds worry-reduction rounds)
         vals
         (sort-by :inspection-count)
         (reverse)
         (take 2)
         (map :inspection-count)
         (apply *)
         )))

(defn part-2 [input]
  (let [rounds          10000
        monkeys         (->> input read-monkeys)
        divisors        (->> monkeys vals (map :div-by))
        div-prod        (apply * divisors)
        worry-reduction (fn [v] (mod v div-prod))]
    (->> monkeys
         (throw-rounds worry-reduction rounds)
         vals
         (sort-by :inspection-count)
         (reverse)
         (take 2)
         (map :inspection-count)
         (apply *)
         )))

(defn day-11-1 []
  (part-1 (input-11-1))
  )

(defn day-11-2 []
  (part-2 (input-11-1))
  )
