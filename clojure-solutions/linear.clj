(defn equals-len? [elems]
  (apply == (mapv count elems)))
(defn m-size? [& m] (equals-len? (mapv first m)))

(defn is-vector? [vect] (and (vector? vect) (every? number? vect)))
(defn vectors? [& args] (and (every? is-vector? args) (equals-len? args)))
(defn tensor-check [args] (and(every? vector? args) (equals-len? args)))

(defn tensor? [& ts] (or (every? number? ts) (and (tensor-check ts)
                                                     (apply tensor? (apply concat ts)))))
(defn matrix? [m] (and (vector? m) (apply vectors? m) (equals-len? m)))
(defn is-matrices? [ms] (every? matrix? ms) (m-size? ms))

(defn execute "common function for matrices and vectors" [cond? operation]
  (fn [& elems]
    {:pre  [(every? cond? elems) (equals-len? elems)]
     :post [(cond? %)]}
    (apply mapv operation elems))
  )

(defn m "additional function for matrices" [operation] (execute is-matrices? operation))
(defn v "additional function for vectors" [operation] (execute is-vector? operation))


(def v+ "sum of vectors"(v +))
(def v* "multiply of vectors" (v *))
(def v- "subtract of vectors"(v -))

(defn v*s "multiply vector and scalars" [vec & num]
  {:pre  [(and (is-vector? vec)
               (every? number? num))]
   :post [(is-vector? vec) (is-vector? %) (equals-len? (vector vec %))]
   }
  (mapv (partial * (apply * num)) vec))

(defn scalar "scalar multiply vectors" [a b]
  {:pre  [(vectors? a b)]
   :post [(number? %)]
   }
  (apply + (v* a b)))

(defn vect "vectors multiply" [& vec]
  {:pre  [(and (apply vectors? vec) (== (count (first vec)) 3))]
   :post [(is-vector? %) (== (count %) 3)]}
  (reduce (fn [[a0 a1 a2] [b0 b1 b2]]
            (vector (- (* a1 b2) (* a2 b1))
                    (- (* a2 b0) (* a0 b2))
                    (- (* a0 b1) (* a1 b0)))
            )
          vec)
  )

(defn transpose "transpose matrix" [m]
  {:pre  [(matrix? m)]
   :post [(matrix? %) (== (count m) (count (first %)))
          (== (count %) (count (first m)))]}
  (apply mapv vector m)
  )

(def m+ "sum of matrices" (m v+))
(def m- "subtract of matrices" (m v-))
(def m* "multiply of matrices" (m v*))

(defn m*s "multiply matrix and scalars" [m & s]
  {:pre  [(matrix? m) (every? number? s)] :post [(matrix? m) (equals-len? (vector m %))]}
  (let [ss (apply * s)] (mapv #(v*s % ss) m)))

(defn m*v "multiply matrix nad vector" [m v]
  {:pre  [(matrix? m) (is-vector? v) (mapv #(equals-len? (vector % v)) m)]
   :post [(is-vector? %)]}
  (mapv (partial scalar v) m))

(defn matrix-mul "multiply two matrices" ([a b]
        {:pre  [(matrix? a) (matrix? b) (= (count (nth a 0)) (count b) )]
         :post [(matrix? %) (= (count %) (count a)) (= (count (% 0)) (count (b 0)))]}
  (mapv (partial m*v (transpose b)) a)))

(defn m*m "matrices multiply" [& ms] {:pre  [(is-matrices? ms)]
                  :post [(matrix? %) (= (count %) (count (first ms))) (= (count (first (last ms))) (count (% 0)))]}
  (reduce matrix-mul ms))

(defn t "common tensor function" [func]
  (fn execute [& ts]
    {:pre [(or (every? number? ts) (and (tensor-check ts) (apply tensor? ts)))]
     :post [(tensor? %)]}
    (if (every? vector? ts)
      (apply mapv execute ts)
      (apply func ts))
    ))

(def t+ "sum of tensors" (t +))
(def t- "subtract of tensors" (t -))
(def t* "multiply of tensors" (t *))

;(t+ [[1], [[2, 3]]])