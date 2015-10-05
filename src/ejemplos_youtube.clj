

;ejemplo for
(take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))

;LIST COMPREHENSION, the for macro
;https://www.youtube.com/watch?v=5lvV9ICwaMo&list=PLgdGONiCNy-1g-YSebHnNzcwFtO5GfHO6&index=4

(for [number [1 2 3]] (* number 2))
;se puede hacer con map
(map #(* % 2) [1 2 3])

(for [number [1 2 5]
      letters [:a :b :c]]
  (str number letters))
;esto sería más complicado de hacerlo con map



;;**********************************************************************************************

(def common-words (-> (slurp "http://www.textfixer.com/resources/common-english-words.txt")
                      (clojure.string/split #",")
                      set
                      ))



(def text (slurp "http://www.clearwhitelight.org/hitch/hhgttg.txt"))
text

(->> text
     (re-seq #"[\w|']+")
     (map #(clojure.string/lower-case %))
     (remove common-words)
     frequencies
     (sort-by val)
     reverse)
