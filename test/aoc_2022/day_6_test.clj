(ns aoc-2022.day-6-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-6 :refer :all]))

(deftest unit-test-day-6
  (testing "unit tests day 6"
    (testing "part 1"
      (is (= 7 (part-1 test-input-6-1)))
      )

    (testing "part 2"
      (is (= 19 (part-2 test-input-6-1)))
      )
    ))

(deftest day-6
  (testing "day 6"
    (testing "part 1"
      (is (= 1892 (day-6-1)))
      )

    (testing "part 2"
      (is (= 2313 (day-6-2)))
      )
    ))
