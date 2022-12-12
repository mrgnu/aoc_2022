(ns aoc-2022.day-12-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-12 :refer :all]))

(deftest unit-test-day-12
  (testing "unit tests day 12"
    (testing "part 1"
      (is (= 31 (part-1 test-input-12-1)))
      )

    (testing "part 2"
      )
    ))

(deftest day-12
  (testing "day 12"
    (testing "part 1"
      (is (= 394 (day-12-1)))
      )

    (testing "part 2"
      )
    ))
