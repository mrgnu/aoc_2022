(ns aoc-2021.utils)

(defn per-line-input [fn]
  (with-open [rdr (clojure.java.io/reader fn)]
    (doall (line-seq rdr))))
