(ns clojure.math.test-primes
  (:use [clojure.math.primes :only (primes wheel)])
  (:use [clojure.test]))

(deftest test-primes
  (are [x y] (= x y)
       2 (first (primes))
       3 (second (primes))
       5 (nth (primes) 2)
       7919 (nth (primes) 999)
       57881 (nth (primes) 5863)
       104729 (nth (primes) 9999)))
