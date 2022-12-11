(ns aoc-2022.day-11-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-11 :refer :all]))

(deftest unit-test-day-11
  (testing "unit tests day 11"
    (testing "part 1"
      (is (= 10605 (part-1 test-input-11-1)))
      )

    (testing "part 2"
      )
    ))

(deftest day-11
  (testing "day 11"
    (testing "part 1"
      (is (= 151312 (day-11-1)))
      )

    (testing "part 2"
      )
    ))
