(ns combinatorics.practice.ninth-palace
  (:require [combinatorics.impl :as impl]
            [clojure.math.combinatorics :as mc]))



(defn show-map
  "打印一个棋盘"
  [np-map]
  (doseq [k (partition 3 np-map)]
    (apply println k))
  (println "------------"))

(defn valid-map?
  "判定棋盘是否有效"
  [np-map]
  (let [k-seq (case (count np-map)
                (3 4 5) [[0 1 2]]
                6 [[0 1 2] [3 4 5]]
                7 [[0 1 2] [3 4 5] [0 3 6] [2 4 6]]
                8 [[0 1 2] [3 4 5] [0 3 6] [2 4 6] [1 4 7]]
                9 (concat [[0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]] (partition 3 (range 9)))
                [])]
    (or (empty? k-seq)
        (every? #(= 15 %) (map (partial apply +) (map #(map (vec np-map) %) k-seq))))))

(comment
  (impl/permutation-ex (range 1 10) 9 valid-map? show-map)

  (reduce (fn [n s]
            (if (valid-map? s)
              (do (show-map s) (inc n)) n))
          0
          (mc/permutations (range 1 10))))


