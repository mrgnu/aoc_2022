(ns aoc-2022.day-9-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-9 :refer :all]))

(deftest unit-test-day-9
  (testing "unit tests day 9"
    (testing "part 1"
      (is (= 13 (part-1 test-input-9-1)))
      )

    (testing "part 2"
      (is (= 1 (part-2 test-input-9-1)))
      (is (= 36 (part-2 test-input-9-2)))
      )
    ))

(deftest day-9
  (testing "day 9"
    (testing "part 1"
      )

    (testing "part 2"
      )
    ))
