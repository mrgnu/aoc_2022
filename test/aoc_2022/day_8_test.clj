(ns aoc-2022.day-8-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-8 :refer :all]))

(deftest unit-test-day-8
  (testing "unit tests day 8"
    (testing "part 1"
      (let [input test-input-8-1
            map-size (get-map-size input)
            tree-map (-> input parse-map)]
        (is (= {:w 5 :h 5} map-size))
        (is (= 16 (get-edge-tree-count map-size)))
        (is (= #{(make-coord 6 17)
                 (make-coord 8 17)
                 (make-coord 7 16)
                 (make-coord 7 18)}
               (get-adjacent (make-coord 7 17))))

        (testing "tree visibility"
          (is (tree-visible? tree-map map-size (make-coord 1 1)))
          (is (not (tree-visible? tree-map map-size (make-coord 3 1))))
          (is (not (tree-visible? tree-map map-size (make-coord 2 2))))
          )
        (is (= 5 (get-interior-visible-tree-count tree-map map-size)))
        )
      )

    (testing "part 2"
      (let [input test-input-8-1
            map-size (get-map-size input)
            tree-map (-> input parse-map)]
        (is (= 4 (get-scenic-score tree-map map-size (make-coord 2 1))))
        (is (= 8 (get-scenic-score tree-map map-size (make-coord 2 3))))
        (is (= 8 (part-2 test-input-8-1)))
        )
      )
    ))

(deftest day-8
  (testing "day 8"
    (testing "part 1"
      (is (= 1695 (day-8-1)))
      )

    (testing "part 2"
      (is (= 287040 (day-8-2)))
      )
    ))
