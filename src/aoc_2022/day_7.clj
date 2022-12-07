(ns aoc-2022.day-7
  (:require [aoc-2022.utils :as utils]
            )
  )

(def test-input-7-1
  [
   "$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"
   ])

(defn input-7-1 []
  (->> "resources/day_7_1.txt"
       utils/per-line-input))

(defn- parse-command? [line]
  (when-let [[_ invocation] (re-matches #"^\$ (.*)$" line)]
    (let [[command & args] (clojure.string/split invocation #"\s+")]
    {
     :type    :command
     :command command
     :args    args
     })))

(defn- parse-file? [line]
  (when-let [[_ size name] (re-matches #"^(\d+) (.*)$" line)]
    {
     :type :file
     :size (clojure.edn/read-string size)
     :name name
     }))

(defn- parse-dir? [line]
  (when-let [[_ name] (re-matches #"^dir (.*)$" line)]
    {
     :type :dir
     :name name
     }))

(defn- parse-line [line]
  (some #(% line) [parse-command? parse-file? parse-dir?]))

(defn parse-input [lines]
  (map parse-line lines))

(defn- collect-files [entries]
  (let [[listings entries] (split-with (comp (partial not= :command) :type)
                                       entries)]
    {
     :entries entries
     :files   (filter (comp (partial = :file) :type) listings)
     }
    ))

(defn- ensure-dir [file-tree dir-path]
  (if (get-in file-tree dir-path)
    file-tree
    (assoc-in file-tree dir-path {:name (last dir-path) :files #{}})))

(defn- add-files [file-tree dir-path files]
  (-> file-tree
      (ensure-dir ,,, dir-path) ;; this makes sure dir exists and :files map to a set
      (update-in  ,,, (conj dir-path :files) into files)))

(defn build-file-tree
  ([entries]
   (let [current (first entries)]
     (assert (and (= :command (:type current))
                  (= "cd" (:command current))
                  )
             (format "expected 'cd', got %s" current))
     (let [target (first (:args current))]
       (assert (= "/" target)
               (format "expected '/', got %s" target))
       (build-file-tree (rest entries)
                        [target]
                        (ensure-dir nil [target]))))
   )

  ([entries dir-path file-tree]
   (loop [[current & entries] entries
          dir-path            dir-path
          file-tree           file-tree]
     (if-not current
       file-tree
       (do
         (assert (= :command (:type current))
                 (format "expected command, got %s" current))
         (condp = (:command current)
           "ls"
           (let [{:keys [entries files]} (collect-files entries)
                 file-tree (add-files file-tree dir-path files)
                 ]
             (recur entries dir-path file-tree))

           "cd"
           (let [arg (first (:args current))
                 dir-path (condp = arg
                               "/" [arg]
                               ".." (vec (butlast dir-path))
                               (conj dir-path arg))
                 ]
             (recur entries dir-path file-tree))))))
   )
  )

(defn- child-dir-names [dir]
  (->> dir
       keys
       (filter string?)))

(defn- child-dirs [dir]
  (->> dir
       child-dir-names
       (map (partial get dir))))

(defn- file-sizes [files]
  (->> files (map :size) (apply +)))

;; recursively populates dir with :rec-size
(defn determine-dir-sizes [dir]
  (let [file-size-sum (->> dir :files (map :size) (apply +))
        dir-names     (->> dir keys (filter string?))
        dir           (reduce (fn [dir dir-name]
                                (assoc dir
                                       dir-name
                                       (determine-dir-sizes (get dir dir-name))))
                              dir
                              dir-names)
        dir-size-sum  (->> dir-names
                           (map (partial get dir))
                           (map #(get % :rec-size))
                           (apply +))
        total-size    (+ dir-size-sum file-size-sum)]
    (assoc dir :rec-size total-size)))

(defn part-1 [input]
  (let [root (-> input parse-input build-file-tree (get "/") determine-dir-sizes)]
    (->> root
         (tree-seq (constantly true) child-dirs)
         (map :rec-size)
         (filter (partial >= 100000))
         (apply +))))

(defn part-2 [input]
  (let [update-size    30000000
        disk-size      70000000
        root           (-> input parse-input build-file-tree (get "/") determine-dir-sizes)
        used           (:rec-size root)
        unused         (- disk-size used)
        required-extra (- update-size unused)]
    (->> root
         (tree-seq (constantly true) child-dirs)
         (map (fn [dir] [(:name dir) (:rec-size dir)]))
         (sort-by second)
         (drop-while (comp (partial > required-extra) second))
         first
         second
         )))

(defn day-7-1 []
  (part-1 (input-7-1))
  )

(defn day-7-2 []
  (part-2 (input-7-1))
  )
