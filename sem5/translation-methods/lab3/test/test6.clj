(ns test6)

(defn eq [x y] (= (< x y) (> x y)))

(print (eq 0 0) "\n")
(print (eq 0 1))