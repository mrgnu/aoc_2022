(ns aoc-2022.range-test
  (:require [clojure.test :refer :all]
            [aoc-2022.range :refer :all]))

(deftest range-test
  (testing "range"

    (testing "make-range"
      (is (= (make-range 7 17) {:from 7 :to 17, :size 11}))
      )

    (testing "range-seq"
      (is (= (range-seq (make-range 7 17)) (range 7 18)))
      )

    (testing "range-contains"
      (let [r (make-range 7 17)]
        (is (range-contains? r 7))
        (is (range-contains? r 17))

        (is (not (range-contains? r 6)))
        (is (not (range-contains? r 18)))
        ))

    (testing "merge-ranges"
      (let [rs [(make-range  7 17)
                (make-range  8 11)
                (make-range  6 10)
                (make-range 16 18)
                (make-range 19 19)
                (make-range 21 23)]]
        (is (= [(make-range 6 19) (make-range 21 23)]
               (merge-ranges rs)))
        ))

    (testing "crop-range"
      (is (not (crop-range 0 10 (make-range  11 12))))
      (is (not (crop-range 0 10 (make-range -10 -1))))

      (is (= (make-range 10 20) (crop-range 10 20 (make-range  5 25))))
      (is (= (make-range 10 17) (crop-range 10 20 (make-range  5 17))))
      (is (= (make-range 17 20) (crop-range 10 20 (make-range 17 23))))
      )

    (testing "crop-ranges"
      (let [rs [(make-range -10  -5)
                (make-range  -3   5)
                (make-range   7  15)]]
        (is (= [(make-range 0 5) (make-range 7 10)]
               (crop-ranges 0 10 rs)))
        ))

    (testing "split-range"
      (let [r (make-range 10 20)]
        (is (= [r] (split-range r 23)))
        (is (= [r] (split-range r 9)))
        (is (= [(make-range 11 20)] (split-range r 10)))
        (is (= [(make-range 10 19)] (split-range r 20)))
        (is (= [(make-range 10 19)] (split-range r 20)))
        (is (= [(make-range 10 14) (make-range 16 20)]
               (split-range r 15)))
        ))
    )
  )
