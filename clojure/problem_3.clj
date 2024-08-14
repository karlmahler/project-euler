;; Copyright (C) 2024 Karl Mahler
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; TODO: Find a better way to implement this, since it takes way too long
;; with large numbers.

(declare find-prime-factors divisible? generate-primes prime?)

(defn find-prime-factors [number primes-list]
  (loop [x number
         primes primes-list
         factors []]
    (let [prime (first primes)]
      (cond
        (== (/ x prime) 1.0) (cons prime factors)
        (divisible? x prime) (recur (/ x prime)
                                     primes (cons prime factors))
        :else (recur x (rest primes) factors)))))

(defn- divisible? [dividend divisor]
  (zero? (mod dividend divisor)))

(defn generate-primes []
  (for [x (iterate inc 2) :when (prime? x)] x))

(defn prime? [number]
  (every? false?
          (map #(divisible? number %)
               (range 2 number))))

(defn -main []
  (let [primes (generate-primes)
        problem-number 600851475143
        factors (find-prime-factors problem-number primes)
        largest-prime (apply max factors)]
    (println factors largest-prime)))

(-main)
