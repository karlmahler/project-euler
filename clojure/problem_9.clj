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

;; FIX: Not working with all triplets.

(declare find-triplet-for-which-sum
         generate-triplets
         generate-triplet
         natural?)

(defn find-triplet-for-which-sum [triplets sum]
  (or (first (for [triplet triplets
                   :let [triplet-sum (reduce + triplet)]
                   :while (<= triplet-sum sum)
                   :when (= triplet-sum sum)] triplet)) '()))

(defn generate-triplets
  ([]
   (for [n (iterate inc 1) :let [m (inc n)]]
        (generate-triplet n m)))
  ([n]
   (for [m (iterate inc n)]
        (generate-triplet n (inc m)))))

(defn generate-triplet [n m]
  (if-not (and (> m n) (every? natural? [n m]))
    '()
    (let [n-square (Math/pow n 2)
          m-square (Math/pow m 2)
          a (- m-square n-square)
          b (* 2 n m)
          c (+ n-square m-square)]
      (map #(long %) [a b c]))))

(defn- natural? [number]
  (and (== number (long number))
       (pos? number)))

(defn -main []
  (let [problem-sum 1000
        triplets (generate-triplets 5)
        result-triplet (find-triplet-for-which-sum triplets problem-sum)
        result-product (reduce * result-triplet)]
    (println result-product)))

(-main)
