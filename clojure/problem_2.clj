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

(defn generate-fibonacci-series []
  (let [natural-numbers (iterate inc 1)]
    (for [x natural-numbers] (nth-fibonacci x))))

(defn nth-fibonacci [number]
  (if (< number 2)
    number
    (+ (nth-fibonacci (dec number))
       (nth-fibonacci (- number 2)))))

(defn- filter-even-fibonacci [series]
  (filter even? series))

(defn- filter-fibonacci-below [series upper-bound-inclusive]
  (take-while #(<= % upper-bound-inclusive) series))

(defn -main []
  (let [upper-bound 4000000
        series (generate-fibonacci-series)
        even-series (filter-even-fibonacci series)
        even-series-below (filter-fibonacci-below even-series upper-bound)
        sum (reduce + even-series-below)]
    (println sum)))

(-main)
