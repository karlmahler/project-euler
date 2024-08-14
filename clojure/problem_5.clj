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
