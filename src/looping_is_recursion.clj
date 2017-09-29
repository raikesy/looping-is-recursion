(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [base exp acc]
                       (if (zero? exp)
                         acc
                         (recur base (dec exp) (* acc base))))]
    (power-helper base exp 1)))


(defn last-element [a-seq]
  (let [last-element-helper
        (fn [a-seq last]
            (if a-seq
              (recur (next a-seq) (first a-seq))
              last))]
    (last-element-helper a-seq nil)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1)
        (empty? seq2)
        (not= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [seq a-seq
         index 0]
    (cond
      (empty? seq) nil
      (pred (first seq)) index
      :else (recur (rest seq) (inc index)))))

(defn avg [a-seq]
  (loop [acc 0
         cnt 0
         seq a-seq]
    (if (empty? seq)
      (/ acc cnt)
      (recur (+ (first seq) acc) (inc cnt) (rest seq)))))

(defn parity [a-seq]
  (loop [seq a-seq
         set #{}]
    (if (empty? seq)
      set
      (recur (rest seq) (if (contains? set (first seq))
                          (disj set (first seq))
                          (conj set (first seq)))))))

(defn fast-fibo [n]
  (loop [last 0
         this 1
         count 2]
    (cond
      (< n 2) n
      (= n count) (+ this last)
      :else (recur this (+ this last) (inc count)))))


(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         eles #{}
         new-seq []]
    (cond
      (empty? s) new-seq
      (contains? eles (first s)) new-seq
      :else (recur (rest s) (conj eles (first s)) (conj new-seq (first s))))))

