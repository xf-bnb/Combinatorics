(ns combinatorics.practice.eight-queens
  (:require [clojure.math.combinatorics :as mc]
            [combinatorics.impl :as impl]))

(defn show-map
  "打印一个棋盘"
  [eq-map]
  (doseq [k eq-map]
    (apply println (for [i (range (count eq-map))] (if (= i k) \* \-))))
  (println "-----------------------"))

(defn valid-piece?
  "判定指定位置的棋子是否有效"
  [eq-map i]
  (let [v (nth eq-map i)]
    (not (some #(let [x (nth eq-map %)]
                  (or (= v x)
                      (= (- i %) (Math/abs (- v x)))))
               (range i)))))

(defn valid-last?
  "检查最后一颗棋子是否有效"
  [eq-map]
  (valid-piece? eq-map (dec (count eq-map))))

(defn valid-map?
  "判定棋盘是否有效"
  [eq-map]
  (or (= 1 (count eq-map))
      (every? #(valid-piece? eq-map %) (range 1 (count eq-map)))))

(comment
  (impl/permutation-ex (range 8) 8 valid-last? show-map)

  (reduce (fn [n s]
            (if (valid-map? s)
              (do (show-map s) (inc n)) n))
          0
          (mc/permutations (range 8))))

