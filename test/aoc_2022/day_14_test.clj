(ns aoc-2022.day-14-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-14 :refer :all]))

(deftest unit-test-day-14
  (testing "unit tests day 14"
    (testing "part 1"
      (is (= 24 (part-1-sand-simulation test-input-14-1)))
      )

    (testing "part 2"
      (is (= 93 (part-2-sand-simulation test-input-14-1)))
      )
    ))

(deftest day-14
  (testing "day 14"
    (testing "part 1"
      (is (= 692 (day-14-1)))
      )

    (testing "part 2"
      (is (= 31706 (day-14-2)))
      )
    ))
