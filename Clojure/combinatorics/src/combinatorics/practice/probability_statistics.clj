(ns combinatorics.practice.probability-statistics)

(def poker "定义港式五张需要的整副扑克"
  [[:T :8] [:X :8] [:M :8] [:F :8]
   [:T :9] [:X :9] [:M :9] [:F :9]
   [:T :10] [:X :10] [:M :10] [:F :10]
   [:T :J] [:X :J] [:M :J] [:F :J]
   [:T :Q] [:X :Q] [:M :Q] [:F :Q]
   [:T :K] [:X :K] [:M :K] [:F :K]
   [:T :A] [:X :A] [:M :A] [:F :A]])

(def card-type "定义牌型"
  {:THS {:name "同花顺" :value 9}
   :BZ  {:name "豹子" :value 8}
   :HL  {:name "葫芦" :value 7}
   :TH  {:name "同花" :value 6}
   :SZ  {:name "顺子" :value 5}
   :ST  {:name "三条" :value 4}
   :LD  {:name "两对" :value 3}
   :DZ  {:name "对子" :value 2}
   :SP  {:name "散牌" :value 1}})

(def poker-point
  {:8  {:name "8" :value 3}
   :9  {:name "9" :value 4}
   :10 {:name "10" :value 5}
   :J  {:name "J" :value 6}
   :Q  {:name "Q" :value 7}
   :K  {:name "K" :value 8}
   :A  {:name "A" :value 9}})

(def poker-flower
  {:T {:name "桃" :value 4}
   :X {:name "心" :value 3}
   :M {:name "梅" :value 2}
   :F {:name "方" :value 1}})

(defn card-name
  [[k v]]
  (str (-> poker-flower k :name) (-> poker-point v :name)))

(defn cards-name
  [card-seq]
  (mapv card-name card-seq))

#_(defn is-THS?
  [card-seq]
  ())

#_(defn get-card-type
  [card-seq]
  ())




