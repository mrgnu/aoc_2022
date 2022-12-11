(ns aoc-2022.day-10
  (:require [aoc-2022.utils :as utils])
  )

(defn- int-parser [arg] (clojure.edn/read-string arg))

(def instruction-data-map
  {
   :addx {
          :cycle-count 2,
          :reg :X,
          :instr-count 1,
          :arg-parsers [int-parser],
          }
   :noop {
          :cycle-count 1,
          }
   })

(defn- make-instruction [name args]
  (let [{:keys [instruction-count arg-parsers]} (name instruction-data-map)]
    (assert (= (count arg-parsers) (count args))
            (format "expected %d args, got %d"
                    (count arg-parsers)
                    (count args)))
    {
     :name name
     :args (map #(%1 %2) arg-parsers args)
     }))

(def test-input-10-1
  [
   "noop"
   "addx 3"
   "addx -5"
   ])

(defn test-input-10-2 []
  (->> "resources/day_10_example_2.txt"
       utils/per-line-input))

(defn input-10-1 []
  (->> "resources/day_10_1.txt"
       utils/per-line-input))

(defn- parse-instruction [line]
  (let [[name & args] (clojure.string/split line #"\s+")]
    (make-instruction (keyword name) args)))

(defn parse-instructions [lines] (map parse-instruction lines))

(defn- make-runtime []
  {
   :clock       0
   :instr       nil
   :instr-cycle 0
   :regs {
          :X 1
          }
   })

(defn- perform-instruction [{:keys [instr regs] :as runtime}]
  (if-not instr
    runtime
    (let [{:keys [name args]} instr
          instr-info (name instruction-data-map)]
      (condp = name
        :noop runtime
        :addx (update-in runtime [:regs (:reg instr-info)] + (first args))
        ))))

(defn- cycle-count [runtime]
  (let [instr (:instr runtime)]
    (if instr
      (:cycle-count ((:name instr) instruction-data-map))
      0)))

(defn- run-cycle [runtime instrs]
  ;; instruction not done
  (if (< (:instr-cycle runtime) (cycle-count runtime))
    [(-> runtime
         (update ,,, :clock inc)
         (update ,,, :instr-cycle inc))
     instrs]
    (let [
          ;; current instruction done - update runtime 
          runtime (perform-instruction runtime)
          ;; get next instruction
          [instr & instrs] instrs]
      (if-not instr
        ;; program done
        [runtime instrs]
        ;; set new instruction in runtime
        [(-> runtime
                   (update ,,, :clock       inc)
                   (assoc  ,,, :instr-cycle 1)
                   (assoc  ,,, :instr       instr))
         instrs]))))

(defn run-program
  ([runtime instrs] (run-program runtime instrs Long/MAX_VALUE))
  ([runtime instrs clock-count]
   (loop [runtime runtime
          instrs  instrs]
     (if (= clock-count (:clock runtime))
       [runtime instrs]
       (let [[runtime instrs] (run-cycle runtime instrs)]
         (recur runtime instrs))))))

(defn get-signal-strength-sum [instrs]
  (loop [[cycle-count & cycle-counts] [20 60 100 140 180 220]
         runtime          (make-runtime)
         instrs           instrs
         signal-strengths []]
    (if (or (not instrs) (not cycle-count))
      (apply + signal-strengths)
      (let [[runtime instrs] (run-program runtime instrs cycle-count)]
        (let [x (get-in runtime [:regs :X])
              signal-strength (* x cycle-count)
              signal-strengths (conj signal-strengths signal-strength)]
          (recur cycle-counts
                 runtime
                 instrs
                 signal-strengths))))))

(defn print-rows [rows]
  (->> rows
       (partition 40)
       (map (partial apply str))))

(defn render-program [instrs]
  (loop [runtime (make-runtime)
         instrs  instrs
         rows    ""]
    (if-not instrs
      (print-rows rows)
      (let [[runtime instrs] (run-cycle runtime instrs)
            X (get-in runtime [:regs :X])
            x (mod (dec (:clock runtime)) 40)
            p (if (<= (dec X) x (inc X)) "#" ".")]
        (recur runtime instrs (str rows p))))))

(defn day-10-1 []
  (get-signal-strength-sum (parse-instructions (input-10-1)))
  )

(defn day-10-2 []
  )
