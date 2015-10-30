(ns looping-is-recursion)

(defn power [base exp]
  (let [ftn (fn [acc new-exp]
                 (let [new-acc (* base acc)]
                 (cond (= new-exp 0) 1
                       (= new-exp 1) new-acc
                   :else (recur new-acc (dec new-exp)))))]
    (ftn 1 exp)))

(defn last-element [a-seq]
  (let [ftn (fn [so-far r-seq]
            (if (empty? r-seq) so-far
            (recur (first r-seq) (rest r-seq))))]
    (ftn nil a-seq)))

(defn seq= [seq1 seq2]
  (let [e1 (empty? seq1)
        e2 (empty? seq2)]
  (cond (and e1 e2) true
        (or e1 e2 (not= (first seq1) (first seq2))) false
        :else (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [xs a-seq
         i  0]
    (cond (empty? xs) nil
          (pred (first xs)) i
          :else (recur (rest xs) (inc i)))))

(defn avg [a-seq]
  (loop [sum1 0
         n 0
         xs a-seq]
    (cond (empty? xs) (if (zero? n) 0 (/ sum1 n))
          :else (recur (+ sum1 (first xs)) (inc n) (rest xs)))))

(defn toggle [a-set element]
  ((partial (if (contains? a-set element) disj conj)) a-set element))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn fast-fibo [n]
  (cond (= 0 n) 0
        (= 1 n) 1
        :else
  (loop [f-1 1 f-2 0 i 2]
    (if (= n i) (+ f-1 f-2)
        (recur (+ f-1 f-2) f-1 (inc i))))))

(defn cut-at-repetition [a-seq]
  (loop [so-far []
         found #{}
         xs a-seq]
    (let [e (first xs)]
      (if (or (empty? xs) (contains? found e))
      so-far
      (recur (conj so-far e) (conj found e) (rest xs))))))

