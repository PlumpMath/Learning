;; 1.11  ****************************************************************
;; A function f is defined by
;; f(n) = { n if n < 3,
;;          f(n-1)+2f(n-2)+3f(n-3) if n >= 3
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of a iterative process.

;; As a recursive process:
(defn frec [n]
  (if (< n 3)
    n
    (+ (frec (- n 1)) (* 2 (frec (- n 2))) (* 3 (frec (- n 3))))))

(frec 3)
;; 25

;; sustitution model
;; (f 5)
;; (+ (f 4) (* 2 (f 3)) (* 3 (f 2)))
;; (+ (+ (f 3) (* 2 (f 2)) (* 3 (f 1))) (* 2 (+ (f 2) (* 2 (f 1)) (* 3 (f 0)))) (* 3 2))
;; (+ (+ (+ (f 2) (* 2 (f 1)) (* 3 (f 0))) (* 2 2) (* 3 1)) (* 2 (+ 2 (* 2 1) (* 3 0))) 6)
;; (+ (+ (+ 2 (* 2 1) (* 3 0)) 4 3) (* 2 (+ 2 2 0)) 6)
;; (+ (+ (+ 2 2 0) 4 3) (* 2 4) 6)
;; (+ (+ 4 4 3) 8 6)
;; (+ 11 8 6)
;; 25

;; Tree-recursive process
;;                          f5
;;                       /   |  \
;;                     /     |    \
;;                   /       |      \
;;                 f4        2*f3    3*f2
;;               /| \        / | \     |
;;             /  |  \      /  |  \    |
;;           /    |   \    /   |   \   |
;;         f3  2*f2 3*f1 f2 2*f1 3*f0  2
;;       / | \    |    |  |    |    |
;;     /   |   \  |    |  |    |    |
;;   f2 2*f1 3*f0 2    1  2    1    0
;;    |    |    |
;;    2    1    0


;; As a iterative process
(defn fiter
  ([n] (cond
         (= n 0) 0
         (= n 1) 1
         :else (fiter 2 1 0 n)))
  ([a b c count]
   (if
     (< count 3)
     a
     (fiter (+ a (* 2 b) (* 3 c)) a b (- count 1))
     )))

(fiter 5)
;; sustitution model
;; (fiter 5)
;; (fiter 2 1 0 5)
;; (fiter 4 2 1 4)
;; (fiter 11 4 2 3)
;; (fiter 25 11 4 2)
;; 25

;; 1.12 ****************************************************************
;; Write a procedure that computes elements of Pascals Triangule by means
;; of a recursive process

;;      1
;;     1 1
;;    1 2 1
;;   1 3 3 1
;;  1 4 6 4 1

;; (pascal row col)
;; (pascal 5 3) 6

(defn p [c r]
  (cond
    (= r 1) 1
    (= r c) 1
    :else (+ (p (- c 1) (- r 1)) (p (- c 1) r))))

(p 5 3)

;; sustitution model (tree-recursive process)
;; (p 5 3)
;; (+ (p 4 2)                         (p 4 3))
;; (+ (+ (p 3 1) (p 3 2))             (+ (p 3 2)             (p 3 3)))
;; (+ (+ 1       (+ (p 2 1) (p 2 2))) (+ (+ (p 2 1) (p 2 2)) 1))
;; (+ (+ 1       (+ 1       1))       (+ (+ 1       1)       1))
;; (+ (+ 1       2)                   (+ 2                   1))
;; (+ 3                               3)
;; 6





