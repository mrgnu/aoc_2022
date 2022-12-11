(ns aoc-2022.day-10-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-10 :refer :all]))

(deftest unit-test-day-10
  (testing "unit tests day 10"
    (testing "part 1"
      (is (= 13140
             (get-signal-strength-sum (parse-instructions (test-input-10-2)))))
      )

    (testing "part 2"
      )
    ))

(deftest day-10
  (testing "day 10"
    (testing "part 1"
      (is (= 15220 (day-10-1)))
      )

    (testing "part 2"
      )
    ))
