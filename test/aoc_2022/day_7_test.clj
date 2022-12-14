(ns aoc-2022.day-7-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-7 :refer :all]))

(defn- sum-file-sizes [input]
  (->> input parse-input (filter (comp (partial = :file) :type)) (map #(get % :size)) (apply +)))

(deftest unit-test-day-7
  (testing "unit tests day 7"
    (testing "part 1"
      (is (= {"/"
              {
               :name "/"
               :files
               #{{:type :file, :size 14848514, :name "b.txt"}
                 {:type :file, :size 8504156, :name "c.dat"}},
               "a"
               {
                :name "a"
                :files
                #{{:type :file, :size 2557, :name "g"}
                  {:type :file, :size 62596, :name "h.lst"}
                  {:type :file, :size 29116, :name "f"}},
                "e" {:name "e" :files #{{:type :file, :size 584, :name "i"}}}},
               "d"
               {
                :name "d"
                :files
                #{{:type :file, :size 4060174, :name "j"}
                  {:type :file, :size 8033020, :name "d.log"}
                  {:type :file, :size 5626152, :name "d.ext"}
                  {:type :file, :size 7214296, :name "k"}}}}}
             (-> test-input-7-1 parse-input build-file-tree)))
      (let [root-size
            (-> test-input-7-1
                 parse-input
                 build-file-tree
                 (get "/")
                 determine-dir-sizes
                 (get :rec-size))]
        (is (= 48381165 root-size))
        (is (= (sum-file-sizes test-input-7-1)
               root-size)))
      (is (= 95437
             (part-1 test-input-7-1)))
      )

    (testing "part 2"
      (is (= 24933642 (part-2 test-input-7-1)))
      )
    ))

(deftest day-7
  (testing "day 7"
    (testing "part 1"
      (is (= 1325919 (day-7-1)))
      )

    (testing "part 2"
      (let [root-size
            (-> (input-7-1)
                parse-input
                build-file-tree
                (get "/")
                determine-dir-sizes
                (get :rec-size))]
        (is (= (sum-file-sizes (input-7-1))
               root-size)))

      (is (not= "mtvgvvcr" (day-7-2)))
      (is (= 2050735 (day-7-2)))
      )
    ))
