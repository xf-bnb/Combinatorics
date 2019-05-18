(ns combinatorics.core
  (:require [clojure.math.combinatorics :as mc]))

(defn find-x [stack start end]
  (loop [i start]
    (when (< i end) (if (some #(= i %) stack) (recur (inc i)) i))))

(defn permutation
  [f coll n]
  (let [v (vec coll)
        size (count v)
        number (atom 0)]
    (when (< 0 n size)
      (loop [stack [] begin 0]
        (if-let [x (some (fn [i] (when-not (some #(= i %) stack) i)) (range begin size))]
          (let [s (conj stack x)]
            (if (= n (count s))
              (do (f (map v s))
                  (swap! number inc)
                  (recur stack (inc x)))
              (recur s 0)))
          (when-not (empty? stack)
            (recur (pop stack) (inc (last stack)))))))
    @number))

(defn combination
  [f coll n]
  (let [v (vec coll)
        size (count v)
        number (atom 0)]
    (if (zero? n)
      (do (f ()) (swap! number inc))
      (when-not (or (< n 0) (< size n))
        (loop [stack [] x 0]
          (if (< x size)
            (let [s (conj stack x)]
              (if (= n (count s))
                (do (f (map v s))
                    (swap! number inc)
                    (recur stack (inc x)))
                (recur s (inc x))))
            (when-not (empty? stack)
              (recur (pop stack) (inc (last stack))))))))
    @number))

(comment

  (permutation println [1 2 3 4 5 6 7] 4)

  (combination println [1 2 3 4 5 6 7] 4))

(fn [x s]
  (letfn [(powerset [s]
            (reduce (fn [ps x]
                      (reduce (fn [ps s] (conj ps (conj s x))) ps ps))
                    #{#{}}
                    s))]
    (set (filter #(= x (count %)) (powerset s)))))

(comment
  (mc/combinations [1 2 3 4 5] 2)

  (mc/permutations [1 2 3 4 5 6 7 8 9 0]))



