(ns combinatorics.core
  (:require [clojure.math.combinatorics :as mc]))

(defn permutation
  [f data n]
  (let [size (count data)
        n (if (< n size) n size)
        push-stack (fn [stack start]
                     (loop [i start]
                       (if (< i size)
                         (if (some #(= i %) stack)
                           (recur (inc i))
                           [(conj stack i) i])
                         [stack i])))]
    (if (< n 1)
      (if (f (empty data)) 1 0)
      (loop [stack []]
        (let [stack (loop [stack (first (push-stack stack 0))]
                      (if (= n (count stack))
                        (do (f (reduce (fn [r i] (conj r (nth data i))) [] stack))
                            (recur (loop [[stack i] [stack size]]
                                     (if (and (<= size i) (not (empty? stack)))
                                       (recur (push-stack (pop stack) (inc (last stack))))
                                       stack))))
                        stack))]
          (when-not (empty? stack)
            (recur stack)))))))

(defn combination
  [f data n]
  (let [size (count data)
        n (if (< n size) n size)
        x (- size n)]
    (if (< n 1)
      (if (f (empty data)) 1 0)
      (loop [[stack i] [[] 0]]
        (let [[stack i] (loop [[stack i] [(conj stack i) i]]
                          (if (= n (count stack))
                            (do (f (reduce (fn [r i] (conj r (nth data i))) [] stack))
                                (recur (loop [[stack i] [stack size]]
                                         (if (and (<= (+ x (count stack)) i) (not (empty? stack)))
                                           (let [[stack i] [(pop stack) (inc (last stack))]]
                                             (if (< (+ (count stack) x) i)
                                               (recur [stack i])
                                               [(conj stack i) i]))
                                           [stack i]))))
                            [stack i]))]
          (when-not (empty? stack)
            (recur [stack (inc i)])))))))

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

  (mc/permutations [1 2 3 4]))




