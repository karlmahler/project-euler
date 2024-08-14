(declare least-common-multiple greatest-common-divisor)

(defn least-common-multiple
  ([a b]
   (/ (* a b)
      (greatest-common-divisor a b)))
  ([a b & xs]
   (let [current-multiple (least-common-multiple a b)
         x (first xs)]
     (if (empty? xs)
       (least-common-multiple a b)
       (recur current-multiple x (rest xs))))))

(defn greatest-common-divisor
  ([a b]
   (if (zero? b)
     a 
     (recur b (mod a b))))
  ([a b & xs]
   (let [current-divisor (greatest-common-divisor a b)
         x (first xs)]
     (if (empty? xs)
       (greatest-common-divisor a b)
       (recur current-divisor x (rest xs))))))

(defn -main []
  (let [problem-numbers (range 1 (inc 20))]
    (println (apply least-common-multiple problem-numbers))))

(-main)
