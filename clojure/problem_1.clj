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

(declare find-multiples build-factors-predicate multiple?)

(defn find-multiples [& factors]
  (let [natural-numbers (iterate inc 1)
        predicate (apply build-factors-predicate factors)]
    (filter predicate natural-numbers)))

(defn- build-factors-predicate [& factors]
  (let [predicates (for [factor factors] #(multiple? % factor))
        composed-predicate (apply some-fn predicates)]
    composed-predicate))

(defn- multiple? [number factor]
  (zero? (mod number factor)))

(defn -main []
  (let [largest-multiple-to-find 1000
        factors [3 5]
        multiples (apply find-multiples factors)
        multiples-below-largest (take-while
                                 #(< % largest-multiple-to-find) multiples)
        sum-of-multiples (reduce + multiples-below-largest)]
    (println sum-of-multiples)))

(-main)
