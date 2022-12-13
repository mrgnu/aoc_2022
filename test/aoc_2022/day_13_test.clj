(ns aoc-2022.day-13-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-13 :refer :all]))

(deftest unit-test-day-13
  (testing "unit tests day 13"
    (testing "part 1"
      (is (= 13 (part-1 test-input-13-1)))
      )

    (testing "part 2"
      )
    ))

(deftest day-13
  (testing "day 13"
    (testing "part 1"
      (is (= 5208 (day-13-1)))
      )

    (testing "part 2"
      )
    ))
