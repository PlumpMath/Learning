;; permutations ***************************************************************
;; my solution

(defn perms [s]
  (if (= (count s) 1)
    [s]
    (let [rotations (loop [res [] arg s counter (count s)]
                      (if (> counter 0)
                        (recur (conj res arg) (conj (vec (rest arg)) (first arg)) (dec counter))
                        res))]
      (reduce (fn [r v] (into r (map #(conj % (first v)) (perms (vec (rest v)))))) [] rotations))))


(perms [1 2 3])

;; My solution's problems: it's not lazy

;; Another solution (Stackoverflow)
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(permutations [1 2 3])

;; I didn't know how to remove an element from a list, that's why I did the rotation part
(def a (list 1 2 3 4))
(def b  [1 2 3 4])
(remove #{2} a)
(remove #{2} b)
