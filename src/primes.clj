(ns primes
  ( :use clojure.contrib.priority-map))

(defn wheel-1 [step-sizes start count-generated]
  (lazy-seq
   (cons start
         (wheel-1 step-sizes
                  (+ start (step-sizes (mod count-generated
                                            (count step-sizes))))
                  (inc count-generated)))))

(defn wheel [primes]
  (let [circumference (apply * primes)
        relatively-prime? (fn [x] (not-any? #(= 0 (mod x %)) primes))
        ;; This produces all but the last step.
        most-steps (->> (range 0 circumference)
                        (filter relatively-prime?)
                        (partition 2 1)
                        (map (fn [[this_rp, next_rp]] (- next_rp this_rp)))
                        (apply vector))
        ;; The step sizes in most-steps will take us most of the way
        ;; around the wheel. Since we know that the wheel's step sizes
        ;; must add up to the wheel's circumference, we can figure out
        ;; what the last step's size is.
        all-steps (conj most-steps
                        (- circumference
                           (apply + most-steps)))]

    (concat (sort primes)
            ;; This sequence contains no numbers that are divisible by
            ;; any of the numbers in primes. However, that includes 1,
            ;; and 1 isn't prime.
            (drop 1 (wheel-1 all-steps 1 0)))))

(defn move-past [iterators n]
  (let [[p next-multiple] (first iterators)]
    (if (> n next-multiple)
      (recur (assoc iterators p (+ next-multiple p))
             n)
      iterators)))

(defn primes-1 [input iterators]
  (let [candidate (first input)
        advanced-iterators #_(move-past iterators candidate)
        (->> iterators
             (take-while #(> candidate (second %)))
             (reduce (fn [iters [p multiple]]
                       (assoc iters p (+ multiple p)))
                     iterators))
        [p next-multiple] (first advanced-iterators)]
    (if (== candidate next-multiple)
      ;; found a multiple of an existing prime (i.e. a
      ;; composite number); skip it and keep looking
      (recur (rest input) advanced-iterators)
      ;; we found a prime; emit it and add an iterator to our set
      (lazy-seq
       (cons candidate
             (primes-1 (rest input)
                       (assoc advanced-iterators candidate (* candidate candidate))))))))

(defn primes
  ([] (primes [2 3 5 7 11]))
  ([wheel-primes]
     (let [input (drop (count wheel-primes) (wheel wheel-primes))
           next-prime (first input)]
       (concat wheel-primes
               (list next-prime)
               (primes-1 (rest input)
                         (priority-map next-prime (* next-prime next-prime)))))))
