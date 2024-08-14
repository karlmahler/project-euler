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

(require '[clojure.string :as str])

(declare generate-n-digits-palindrome-products
         generate-n-digits-palindrome-product-factors
         get-smallest-n-digits-number
         get-largest-n-digits-number
         palindromic-number?)

(defn generate-n-digits-palindrome-products [number-of-digits]
  (let [factors-set (generate-n-digits-palindrome-product-factors number-of-digits)
        products (for [factors factors-set] (apply * factors))]
    products))

(defn generate-n-digits-palindrome-product-factors [number-of-digits]
  (let [smallest-number (get-smallest-n-digits-number number-of-digits)
        largest-number (get-largest-n-digits-number number-of-digits)]
    (loop [first-product largest-number
           second-product largest-number
           palindromes #{}]
      (cond
        (and (= first-product smallest-number)
             (= second-product smallest-number)) palindromes
        (<= first-product smallest-number) (recur largest-number
                                                 (dec second-product) palindromes)
        (palindromic-number? (* first-product second-product))
        (recur (dec first-product) second-product
               (conj palindromes
                     (if (= first-product second-product)
                       #{first-product}
                       #{first-product second-product})))
        :else (recur (dec first-product) second-product palindromes)))))

(defn- get-smallest-n-digits-number [number-of-digits]
  (long (Math/pow 10 (dec number-of-digits))))

(defn- get-largest-n-digits-number [number-of-digits]
  (Long/parseLong (str/join "" (repeat number-of-digits "9"))))

(defn- palindromic-number? [number]
  (let [number-as-string (str number)
        reversed-number (str/reverse number-as-string)]
    (= number-as-string reversed-number)))

(defn -main []
  (let [problem-number-of-digits 3
        products (generate-n-digits-palindrome-products problem-number-of-digits)
        largest-palindrome-product (apply max products)]
    (println largest-palindrome-product)))

(-main)
