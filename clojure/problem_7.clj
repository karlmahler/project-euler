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

(declare nth-prime generate-primes prime? divisible?)

(defn nth-prime [number primes]
  (nth primes (dec number)))

(defn- generate-primes []
  (for [x (iterate inc 2) :when (prime? x)] x))

(defn- prime? [number]
  (let [bound (long (Math/sqrt number))]
    (cond
      (= number 2) true
      (or (divisible? number 2) (< number 2)) false
      :else (every? false? (map #(divisible? number %)
                                 (range 3 (inc bound) 2))))))

(defn- divisible? [dividend divisor]
  (zero? (mod dividend divisor)))

(defn -main []
  (let [problem-nth 10001 primes (generate-primes)]
    (println (nth-prime problem-nth primes))))

(-main)
