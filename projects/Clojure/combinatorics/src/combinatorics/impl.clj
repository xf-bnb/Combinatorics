(ns combinatorics.impl
  (:require [clojure.math.combinatorics :as mc]))

(defn find-x [stack start end]
  (loop [i start]
    (when (< i end) (if (some #(= i %) stack) (recur (inc i)) i))))

(defn permutation
  "遍历一个序列的n个数的排列，注意序列的元素不能重复。"
  [coll n f]
  (let [v (vec coll)
        size (count v)
        number (atom 0)]
    (when (and (< 0 n) (<= n size))
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

(defn permutation-ex
  [coll n check fout]
  (let [v (vec coll)
        size (count v)
        number (atom 0)]
    (when (and (< 0 n) (<= n size))
      (loop [stack [] begin 0]
        (if-let [x (some (fn [i] (when (and (not (some #(= i %) stack))
                                            (check (map v (conj stack i))))
                                   i))
                         (range begin size))]
          (let [s (conj stack x)]
            (if (= n (count s))
              (do (fout (map v s))
                  (swap! number inc)
                  (recur stack (inc x)))
              (recur s 0)))
          (when-not (empty? stack)
            (recur (pop stack) (inc (last stack)))))))
    @number))

(defn combination
  "遍历一个序列的n个数的组合，注意序列的元素不能重复。"
  [coll n f]
  (let [v (vec coll)
        size (count v)
        number (atom 0)]
    (if (zero? n)
      (do (f ()) (swap! number inc))
      (when (and (< 0 n) (<= n size))
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

(defn- index-combination
  [coll n f]
  (let [size (count coll)]
    (loop [stack [] v-seq (vec (repeat size 0)) index 0]
      (if (< index size)
        (let [number (v-seq index)]
          (if (< number (nth coll index))
            (let [v (update v-seq index inc)
                  s (if (zero? number) (conj stack index) stack)]
              (if (= n (apply + v))
                (do (f v)
                    (recur (pop s) (assoc v index 0) (inc index)))
                (recur s v (inc index))))
            (recur (pop stack) (assoc v-seq index 0) (inc index))))
        (when-not (= 0 (apply + v-seq))
          (recur stack v-seq (last stack)))))))

(defn multi-combination
  "遍历一个序列的n个数的组合，序列的元素可以重复。"
  [coll n f]
  (if (zero? n)
    (do (f ()) 1)
    (let [size (count coll)
          number (atom 0)]
      (when (and (< 0 n) (<= n size))
        (let [coll-map (frequencies coll)
              k-seq (keys coll-map)
              v-seq (vals coll-map)]
          (if (= 1 n)
            (do (map f k-seq) (reset! number (count k-seq)))
            (index-combination (vec v-seq) n
                               #(do (f (apply concat (map repeat % k-seq)))
                                    (swap! number inc))))
          @number)))))

(comment

  (permutation [1 2 3 4 5 6 7] 4 println)

  (combination [1 2 3 4 5 6 7] 4 println)

  (multi-combination [:a :b :a :b :c :a :b :c :a :d :e :f] 6 println))

(comment
  (mc/combinations [1 2 3 4 5] 2)

  (mc/permutations [1 2 3 4 5 6 7 8 9 0]))

(comment
  (fn [x s]
    (letfn [(powerset [s]
              (reduce (fn [ps x]
                        (reduce (fn [ps s] (conj ps (conj s x))) ps ps))
                      #{#{}}
                      s))]
      (set (filter #(= x (count %)) (powerset s)))))

  (fn ff [n xs]
    (if (zero? n)
      #{#{}}
      (set (for [x xs
                 y (ff (dec n) (disj xs x))]
             (conj y x))))))
