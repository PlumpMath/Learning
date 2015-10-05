;Ejercicios de la página: https://projecteuler.net/archives


;Problem #1
(defn multiplos [x]
  (if  (or (zero? (rem x 3))
           (zero? (rem x 5)))
    x
    (str "no es un múltiplo de 3 o de 5")))


(apply +   (filter #(or (zero? (rem % 3))
                        (zero? (rem % 5))) (range 1000)))

;Problem #3
;propuesta inicial
(defn primo? [x]
  "comprueba si el número dado es un número primo"
  (loop [den (dec x)]
    (if (zero? (rem x den))
      (if (= den 1)
        true
        false)
      (recur (dec den)))))

(defn maxfactor [x]
  "calcula el máximo factor primo de un número"
  (loop [primos (filter primo? (range 2 (inc x)))
         division (/ x (first primos))] ;DUDAAAAAAAAA si en los bindings, un binding de pepende del anterior
    ;no se actualizan a la vez???? problemas con recur
    (if (integer? division)
      (if (= division 1)
        (first primos)
        (maxfactor division))
      (recur (rest primos) (/ x (first (rest primos)))))))

(maxfactor 600851475143)
;funciona!!!

;segunda reflexión. No necesito calcular primero los números primos menores que el número dado
;Si empiezo probando primero si es divisible por 2, que es primo, cuando vaya a probar si
;si es divisible por 4 ya no lo será si he agorado todas las divisiones por 2

(defn maxfactor2 [x]
  "calcula el máximo factor primo de un número"
  (loop [denom 2
         division (/ x denom)]
    (if (integer? division)  ;Me imagino que esto de usar un if dentro de un if no es muy ortodoxo
      (if (= division 1)
        denom
        (maxfactor2 division))
      (recur (inc denom) (/ x (inc denom))))))

(maxfactor 600851475143) ;Funciona!!!!

;Problem #4




(apply max (for [numeros-1 (range 100 1000)
                 numeros-2 (range 100 1000)
                 :let [producto (* numeros-1 numeros-2)]
                 :when (= (clojure.string/reverse (str producto)) (str producto))]
             producto))


















