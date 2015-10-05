(ns learning01)
*ns*

(def x 1)

(let [x 2] x)

x

(map + [1 2 3 4 5])
(reduce + [1 2 3 4 5])
(apply + [1 2 3 4 5])


; ***********PARTIAL*********************
; Si escribo esto:
(take 10 (repeatedly (rand-int 10)))
; Me lanza un java.lang.ClassCastException porque el resultado de (rand-int 10) es un int
; y repeatedly sólo acepta funciones como argumentos, no integers
; para que funcione tengo que interponer partial
(take 10 (repeatedly (partial rand-int 10)))
; (partial rand-int 10) me devuelve una función
(partial rand-int 10)
; que sólo cuando es llamada por repeatedly, toma el argumento que le hemos dado antes
; no hace falta usar take porque repeatedly me da la opción de que le diga cuántas veces
;quiero que se repita
(repeatedly 10 (partial rand-int 10))

;**************APPLY*********************
; apply aplica una función a una colección que puede estar precedida o no de argumentos sueltos
(apply + [1 2 3])

(apply + 1 [2 3])

(apply + 1 2 [3 4])

; Esto (apply + [1 2 3]) es lo mismo que (+ 1 2 3)
; Estos dos códigos también son equivalentes:
(hash-map :a 1 :b 2)
(apply hash-map [:a 1 :b 2])
; En el primero tengo los datos sueltos, y en el segundo están dentro de un vector, formando una colección

(defn concat-rest
  [x & resto]
  (println resto)
  (apply str (butlast resto)))

(concat-rest 0 1 2 3 4)

; para comprender lo que hace la función concat-rest:
(butlast [1 2 3 4])
(str 0 1 2 3)
(str '(0 1 2 3))
(str (butlast [1 2 3 4]))
(apply str (butlast [1 2 3 4]))













