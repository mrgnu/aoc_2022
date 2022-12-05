(ns aoc-2022.day-5-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-5 :refer :all]))

(deftest unit-test-day-5
  (testing "unit tests day 5"
    (testing "part 1"
      (is (= "CMZ" (part-1 test-input-5-1)))
      )

    (testing "part 2"
      )
    ))

(deftest day-5
  (testing "day 5"
    (testing "part 1"
      ;; this one was in map (aka random) order
      (is (not= "VVLLPBVLT" (day-5-1)))
      (is (= "LBLVVTVLP" (day-5-1)))
      )

    (testing "part 2"
      )
    ))
