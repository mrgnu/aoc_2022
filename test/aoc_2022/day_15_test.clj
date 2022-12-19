(ns aoc-2022.day-15-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-15 :refer :all]))

(deftest unit-test-day-15
  (testing "unit tests day 15"
    (testing "part 1"
      (is (= 26 (part-1 test-input-15-1 10)))
      )

    (testing "part 2"
      (is (= (part-2 test-input-15-1 20) 56000011))
      )
    ))

(deftest day-15
  (testing "day 15"
    (testing "part 1"
      (is (= (day-15-1) 5870800))
      )

    (testing "part 2"
      )
    ))
