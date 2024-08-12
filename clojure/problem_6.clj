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

(declare gauss-sum sum-of-squares)

(defn gauss-sum
  ([start end] (/ (* (+ start end) (inc (- end start))) 2))
  ([end] (gauss-sum 1 end)))

(defn sum-of-squares [number]
  (reduce + (map #(Math/pow % 2) (range 1 (inc number)))))

(defn -main []
  (let [problem-number 100
        natural-numbers-sum (gauss-sum problem-number)
        square-of-sum (Math/pow natural-numbers-sum 2)
        squares-sum (sum-of-squares problem-number)
        difference (long (- square-of-sum squares-sum))]
    (println difference)))

(-main)
