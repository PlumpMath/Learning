
;INTERLEAVE sin usar interleave. *********************************************************************
;Esta es mi solución poco elegante
;mi tendencia todavía al imperative control flow

(defn interl [s t] (loop [result []
                          s s
                          t t]
                     (if (and (seq s) (seq t))
                       (recur (concat result [(first s) (first t)]) (rest s) (rest t))
                       result)))


(interl [1 2 3 2 1] [1 2 2 2 3 4 4 4 4 4 2 5 7])

;Esta es la más concisa. Usando mapcat, que devuelve el resultado de aplicar concat
; al resultado de aplicar map a una función y unas colecciones. Por ello, la función f
; debería devolver como resultado una colección.

(mapcat list [1 1 1] [2 2 2])
; esto sería igual que

(apply concat (map list [1 1 1] [2 2 2]))
(map list [1 1 1] [2 2 2])



;FLATTEN una coll sin usar flatten *********************************************************************
;orden clave --> tree-seq
;Páginas de ayuda con tree-seq "http://thornydev.blogspot.com.es/2012/09/beautiful-clojure.html"
;"http://ideolalia.com/2013/12/18/rhizome.html"

;Importante!!! Un vector es sequential pero no es seq
;Sequential son las listas, los vectores, pero no los mapas o los sets
(sequential? [1 2 3 2 1])
(seq? [1 2 3 2 1])

;Tengo varios ejemplos pero no sé cómo funcionan o por qué a veces no funcionan.
(tree-seq vector? seq [1 [2 3] 4 [5 6]])

(map first (tree-seq next rest '(:A (:B (:D) (:E)) (:C (:F)))))

(map first (tree-seq next rest '((1 2) 3 [4 [5 6]]))) ;este arroja una excepción

(map first (tree-seq next rest '((1 2 (3)) (4))))

(tree-seq map? #(interleave (keys %) (vals %)) {:a 1 :b {:c 3 :d 4 :e {:f 6 :g 7}}})
(tree-seq map? #(interleave (keys %) (vals %)) {:a 1 :b 2})

(#(interleave (keys %) (vals %))  {:a 1 :b {:c 3 :d 4 :e {:f 6 :g 7}}})

;tree-seq trata una colección anidada como una árbol, donde la colección y sus subcolecciones
;son las ramas y los valores las hojas.

;(tree-seq branch? children root)
;tree-seq takes two functions in addition to the structure itself, branch? and children.
;branch? should return a falsey value if the node is a leaf, and a truthy value otherwise. children should return a
;sequence of the node's children, and will only be invoked if branch? is true.

(def lista '((1 2 (3)) (4)))
lista
(tree-seq sequential? seq lista)
;branch? es en este caso sequential? Si es true, me devuelve la rama (colección)
; si es falso pasa al children, que en este caso es seq, por lo que simplemente me devuelve el valor
(tree-seq next rest lista)
;Si dentro del nodo que estamos mirando hay más de un elemento, es decir, si next existe y por lo tanto me
;devuelve true, invoca una secuencia del children
(tree-seq next rest '(1 (2 (3))))
(tree-seq next rest '(1 2 (3))) ;no entiendo por qué no funciona este??????????????????????

(tree-seq sequential? seq '(1 (2 (3))))
;Si sólo queremos que nos muestre las hojas del árbol, tenemos que filtrar
(defn flatten2 [x]
  (filter (complement sequential?)
   (rest (tree-seq sequential? seq x)))) ;Creo que al poner rest me quito de en medio el primer elemento
;que es siempre la propia colección???????????????
(flatten2 '((1 2) 3 [4 [5 6]]))

(#(filter (complement sequential?) (rest (tree-seq sequential? seq %))) '((1 2) 3 [4 [5 6]]))


;complement. (complement f) Takes a fn f and returns a fn that takes the same arguments as f,
;has the same effects, if any, and returns the opposite truth value.
(def not-empty? (complement empty?))
(empty? [])
(not-empty? [])
((complement sequential?) [1 2 3 2 1])

;Otra forma de solucionar el probema. Flipante
(defn flt [s]
   (if (sequential? s)
     (mapcat flt s)
     (list s)))
(flt '((1 2) 3 [4 [5 6]]))

(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
;hace lo mismo que

(apply concat (map reverse [[3 2 1 0] [6 5 4] [9 8 7]]))

(map reverse [[3 2 1 0] [6 5 4] [9 8 7]])
(concat [1 2 3] [4 5 6])
(apply concat [[1 2 3] [4 5 6]])



;ELIMINAR ELEMENTOS REPETIDOS *********************************************************************

;primera prueba fallida
(defn quitarep [s]
  (loop [result []
         s s]
    (if (seq s)
      (recur (if (some #(= (first s) %) result) result (concat [(first s)] result)) (rest s))
      (reverse result))))
(quitarep "Hellooooooooo Woooooooooooorld")

;mi poco elegante solución :S
(defn elimrep [s]
  (let [svec (vec s)]
   (loop [cnt 0
         result [(first svec)]]
    (if (< cnt (- (count svec) 1))
      (recur (inc cnt) (if (= (svec cnt)(svec (+ cnt 1))) result (concat [(svec (+ cnt 1))] result)))
      (reverse result)))))

(apply str (elimrep "Hellooooooooo Woooooooooooorld"))

;otras soluciones más elegantes!

(#(map first (partition-by identity %)) [1 2 2 2 3 4 4 4 4 4 2 5 7])

(#(partition-by identity %) [1 2 2 2 3 4 4 4 4 4 2 5 7]) ;(partition-by f coll) Aplica f a cada valor en la colección, cortandola cada vez
                               ; que f devuelve un nuevo valor.

;otra solución
(reduce #(do (println %1 %2)(if (= (last %1) %2) %1 (conj %1 %2))) [] [1 2 2 2 3 4 4 4 4 4 2 5 7])
; Si aplico conj a dos vectores como en (conj [1] [2 3]), el resultado sería [1 [2 3]]
; Sin embargo al aplicar reduce %2 representa consecutivamente cada elemento de la coleccion a la que lo apliquemos




;INTERPOLACIÓN *********************************************************************
; Función de interpolación entre puntos
(defn interpolate [points]                            ; el argumento que recibe interpolate son puntos
  (let [results (into (sorted-map) (map vec points))] ;(defn nombre [argumentos] cuerpo)
                                                      ; dentro del cuerpo de defn podemos empezar con un let para declarar? un local (results)
     (println results)                                ;(map vec points) nos asegura que cada punto es un vector
                                                      ; y puede ser añadido como una entrada en un mapa (results)
                                                      ; Supongo que sólo funciona para puntos del plano con dos coords (x y)
    (fn [x]                                           ; cuando hayamos definido interpolate (en función de points), al aplicarla habrá que pasarle
                                                      ; nuevos argumentos, estos son las coord x de los puntos de los que queremos hayar las coord y
                                                      ; para que sean producto de la interpolación de los points
      (let [[xa ya] (first (rsubseq results <= x))    ; El argumento de fn [x] es un numero, por ej, 2. Cuando escribimos (subseq results > x) lo que
                                                      ; obtenemos son los elementos del mapa result cuya key!!! es mayor que 2, es decir, los puntos
                                                      ; cuya coord x es mayor que 2.
           [xb yb] (first (subseq results > x))]      ; lo que estamos obteniendo por lo tanto dentro de los bindings del let son las coordenadas de los 2 puntos
                                                      ; más cercanos al que nosotros queremos comprobar
          #_(println x)
          #_(println (rsubseq results <= x))
          #_(println (first (rsubseq results <= x)))
          #_(println "xa:" xa "ya:" ya "xb:" xb "yb:" yb)
            (if (and xa xb)                           ; Si queremos comprobar un punto de coord x: 30, por ej, está fuera de rango y por
                                                      ; lo tanto xb = nil. No tenemos xb. Por eso necesitamos (or ya yb). Es decir, le decimos
                                                      ; que nos dé lo únicos valores que conocemos, los de los extremos. Nos dará ya o yb
                                                      ; dependiendo si nos vamos fuera de rango por arriba o por abajo
              (/ (+ (* ya (- xb x)) (* yb (- x xa)))
               (- xb xa))
                (or ya yb))))))

;desglose de interpolate
(def puntos [[0 0][10 10][15 5][20 4]])
(map vec [puntos])
(def maparesultados (into (sorted-map) (map vec puntos)))
maparesultados
(def puntoej [2 10 12 16])
(first puntoej)
 (rsubseq maparesultados <= (first puntoej))
 (#(if (and (= (first %) 1) (= (second %) 2))
   true
   (or 3 4)) [1 5 5 6])


; aplicación de interpolate
(interpolate [[0 0][10 10][15 5][20 4]]) ;defino la función interpolate para estos puntos pero no obtengo ningún
                                         ;resultado hasta que la aplique a un punto
(def f (interpolate [[0 0][10 10][15 5][20 4]]))

   (map f [2 10 12 30]) ;para unas coord x conocidas, podemos encontrar las cood y de la interpolación de los puntos dados



;; #42 Factorial Fun *********************************************************************

(= (__ 5) 120)

(defn factorial [x] (
     loop [cnt x acc 1]
     (if (zero? cnt)
       acc
       (recur (dec cnt) (* cnt acc))
       )))
(factorial 6)


;; Otras soluciones
(#(reduce * % (next (range %))) 5)

#(reduce * (range % 0 -1))

#(apply * (range 1 (inc %)))



;MAX. Encontrar el máx de una serie de números. Esta es mi opción: *********************************************************************

((fn [& numeros] (last(sort(apply conj () numeros)))) 1 7 3)

; esta es más simple. Yo pensaba que sort sólo se podía aplicar a una colección
((fn [& numeros] (last (sort numeros))) 1 7 3)


;PALINDROME *********************************************************************
(= [1 2 3 2 1] (reverse [1 2 3 2 1]))

(reverse "racecar")

(apply str (reverse "racecar"))

(#(cond
   (coll? %)  (= % (reverse %))
   (string? %) (= % (apply str (reverse %)))
   )
  "racecar")

;otras coluciones. En vez de volver a convertir en string la secuencia para que coincida con el
; original, lo que hace es convertir primero el original en una secuencia. De esta forma sirve
; tanto para collecciones como para strings

(#(= (seq %) (reverse %)) "racecar")



;SECUENCIA INVERTIDA. mi solución. sin usar rseq *********************************************************************
(#(seq (replace (vec %) (vec (take (count %) (iterate dec (- (count %) 1)))))) #{1 2 3 4 5})

;yo no había tenido en cuenta esta posibilidad. No sabía como invertir el orden
; de los valores que me da range: asignando al paso -1
(range (count [3 2 1 3]) -1 -1)


;soluciones más simples al ejemplo anterior
(into () [1 2 3 2 1]) ; () es una secuencia vacía. Con into metemos los elementos de v en la secuencia
                      ; vacía, al hacerlo empieza por el último y acaba por el primero. por eso la
                      ; secuencia aparece invertida
(into () #{1 2 3 4 5})
(into () (list 1 2 3 4)) ; funciona también para listas

;conj me devuelve la colección con el o los nuevos argumentos añadidos.
;si lo hago sin reduce entiende que el único argumento que le estoy pasando es el v
; y lo mete tal cual en la lista vacía. Si añado reduce antes, hace eso pero con cada
; elemento del vector
(reduce conj () [1 2 3 2 1] )
(conj () [1 2 3 2 1])

; la siguiente solución no es elegante pero es un ejemplo de cómo usar loop y recur
; REVISAR!
((fn [s]
  (loop [result []
         s s]
    (if (seq s)
      (recur (concat [(first s)] result) (rest s))
      result))) [:a :b :c :d :e])

; función que devuelve una secuencia sin los números pares a partir de una colección
; Si queremos iterar por los elementos de una colección --> for!!!!
; podemos usar :when para que haga algo sólo cuando se cumpla esa condición
(defn odd [x] (for [a x :when (odd? a)] a))
(odd [1 2 3 2 1])

;hacen lo mismo que la de arriba. Usando remove quitamos los elementos que cumplen
; la condición de ser par
(remove even? [1 2 3 2 1])
(remove even? #{1 2 3 4 5})

;otras opciones. Usando filter, filtramos los elementos que son impares
(filter odd? [1 2 3 2 1])



; con do, podemos hacer que pasen varias cosas
(defn par [x] (if (even? x)
                (do
                (println "es par")
                  (println "yeahhhhh"))
                (println "es impar o cero")
                ))

; uso de loop y recur. Los argumentos de recur son los mismos que los que le demos
; a loop (nº de parejas). La función que apliquemos en cada uno de los argumentos que
; le demos a recur es la que nos va a indicar de qué manera se van a ir actualizando
; los locals del vector de definición de loop. En este caso cnt y acc.
(def factorial
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
        acc
        (recur (dec cnt) (* acc cnt)))
         )))



; #83 A Half-Truth *********************************************************************
(#(if (apply = % %&) false true) true true)


; #66 Greatest common divisor *********************************************************************
(defn gcd [x y]
  (loop [denom (if (> x y) y x)]
    (if (and (integer? (/ x denom))
             (integer? (/ y denom)))
      denom
      (recur (dec denom)))))

(gcd 8 4)
;otra versión
(defn gcdotra [x y]
  )

;Versión más elegante
(defn gcdelegant [x y]
  (if (= y 0)
    x
    (gcdelegant y (rem x y))))

(gcdelegant 8 4)


;Variación de #66 Greatest common divisor --> aplicar a más de dos números
;Trato de meterlo todo en una función. Como quiero a provechar la función
;que ya he creado para dos números, meto una función dentro de otra con let
(defn gcdvar [& numbers]
  (let [gcd2 (fn [x y] (loop [denom (if (> x y) y x)]
                         (if (and (integer? (/ x denom))
                                   (integer? (/ y denom)))
                           denom
                           (recur (dec denom)))))]
    (reduce gcd2 numbers)))


  (gcdvar 8 4 24)

;Aquí hago lo mismo pero con la versión más elegante de gcd
;como es recursiva, tengo que darle un nombre a la función anónima fn
;para poder llamarla dentro de ella misma.
(defn gcdvar2 [& numbers]
  (let [gcd (fn gcd2 [x y] (if (= y 0)
                             x
                            (gcd2 y (rem x y))))]
    (reduce gcd numbers)))

(gcdvar2 8 24 28)


; #100 Least Common Multiple *********************************************************************

;probando cosas...
;recur puede funcionar sin loop! Sin loop, devuelve el control al comienzo de la funcion
; Con loop devuelve el contro a loop
;diferencia entre when y if.
;cuando necesitamos un implicit do para conseguir side effects y no necesitamos una else-part, usamos when.

(defn lcmprueba [& numbers]
   (when (seq numbers)
   (println (first numbers))
   (recur (rest numbers))))

(defn lcm2 [x y]
  (loop [denom (max x y)]
    (if (and (zero? (mod denom x))
             (zero? (mod denom y)))
      denom
      (recur (inc denom)))))

(lcm2 21 6)

(defn lcm [& numbers]
  (let [lcmvar (fn [x y]
                 (loop [denom (max x y)]
                   (if (and (zero? (mod denom x))
                            (zero? (mod denom y)))
                     denom
                     (recur (inc denom)))))]
    (reduce lcmvar numbers)))

(lcm 85 25 6)
;funcionan pero solo para números enteros, no negativos y ninguno cero.


(defn lcm_ [x y]
  (let [denom (fn gcd [a b]
                (if (= b 0)
                  a
                  (gcd b (rem a b))))]
(/ (Math/abs (* x y)) (denom x y))))

(lcm_ 2/3 7/5)



(defn  lcm [& numbers]
  (let [lcm2 (fn [x y]
               (let [gcd2 (fn gcd [a b]
                             (if (= b 0)
                               a
                               (gcd b (rem a b))))]
                 (/ (* x y) (gcd2 x y))))]
    (reduce lcm2 numbers)))

(lcm 7 5/7 2 3/5)


;#33 Replicate a seq *********************************************************************
(fn [x y]
  (mapcat #(repeat y %) x))

; #40 Interpose a seq *********************************************************************
(fn [x y]
  (drop-last (interleave y (repeat (count y) x))))


;#41 Drop Every Nth Item *********************************************************************
;Mi solución... poco elegant
(defn drop-nth [coll p]
(for [x (range (count coll))
      :when (not= 0 (nth (map #(mod % p) (map #(+ 1 %) (range (count coll)))) x))]
        (nth coll x)))

(drop-nth [:a :b :c :d :e :f] 2)
(drop-nth [1 2 3 4 5 6 7 8] 3)

;otras versiones
;No conocía partition-all
(partition-all 3 [1 2 3 4 5 6 7 8])
(fn [c n]
  (mapcat #(take (dec n) %) (partition-all n c)))

;Otra solución mía más elegante despues de inspirarme con la anterior
(defn drop-nth2  [coll n] (flatten (partition-all (dec n) n coll)))
(drop-nth2  [:a :b :c :d :e :f] 2)
(drop-nth2 [1 2 3 4 5 6 7 8] 3)
(drop-nth2 [1 2 3 4 5 6] 4)

;#49 Split a sequence, special restrictions split-at *********************************************************************

(defn splitat [n coll] (let [s (partition-all n coll)]
  (list (first s) (apply concat (rest s)))))

(splitat 2 [[1 2] [3 4] [5 6]])

;Pero siempre los hay más elegantes... me cachis!
;era mucho más sencillo... jjjj

(fn [n s] [(take n s) (drop n s)])

;#99 Product Digit *********************************************************************
;Mi solución
(defn sepint [a b] (map #(- (int %)(int \0)) (str (* a b))))

(sepint 999 99)

;otra solución: también se puede usar Integer/parseInt

(fn [x y]
  (map #(Integer/parseInt (str %)) (seq (str (* x y)))))

;#63 Group a sequence *********************************************************************
;Esta es mi solución
(defn groupby [f coll]
  (zipmap (set (map f coll)) (for [x (set (map f coll))]
                                       (for [y coll
                                             :when (= x (f y))]
                                         y))))

;como siempre... los hay más elegantes...
;La clave es merge-with
    ;Returns a map that consists of the rest of the maps conj-ed onto
    ;the first.  If a key occurs in more than one map, the mapping(s)
    ;from the latter (left-to-right) will be combined with the mapping in
    ;the result by calling (f val-in-result val-in-latter).
(fn [f s]
  (apply merge-with concat (map #(hash-map (f %1) [%1]) s)))

(map #(hash-map (count %1) [%1]) [[1] [1 2] [3] [1 2 3] [2 3]])

;#61 Map construction *********************************************************************

(defn mapcons [key val] (apply hash-map (interleave key val)))

;#62 Iterate *********************************************************************
;Mi solución. No entiendo muy bien por qué sin cons no funciona

(defn _iterate [f init]
  (lazy-seq (cons init (_iterate f (f init)))))

(take 5 (_iterate #(* 2 %) 1))

;Otros han usado reductions

(fn [f x] (reductions #(%2 %1) x (repeat f)))

;#81 Set Interseccion *********************************************************************
 (defn intersec [set1 set2]
  (disj (set (for [x set1]
         (if (contains? set2 x)
           x))) nil))

(intersec #{0 1 2 3} #{2 3 4 5})

;Otras soluciones más elegantes... grrrrr
(comp set filter)
(= ((comp set filter) #{0 1 2 3} #{2 3 4 5}) #{2 3})
(set (filter #{0 1 2 3} #{2 3 4 5}))

(fn [s t] (->> (map s t) (remove nil?) set ))
(map #{0 1 2 3} #{2 3 4 5})
(remove nil? (map #{0 1 2 3} #{2 3 4 5}))
(set (remove nil? (map #{0 1 2 3} #{2 3 4 5})))

;#166 Comparisons *********************************************************************

(defn comparisons [f x y]
  (cond
   (f x y) :lt
   (f y x) :gt
   :else :eq
   ))

(comparisons > 0 2)

; #90 Cartesian product *********************************************************************
(defn prd [x y]
 (set (for [a x
        b y]
     (vector a b))))

(prd #{1 2 3} #{4 5})


; #122 Read binary numbers *********************************************************************
; 4 ejercicios in a row! yuju! i'm on fire!

(defn binary-to-decimal [x] (int (reduce +
                                    (map #(* % (Math/pow 2 %2))
                                         (->> x (map str) (map #(Integer/parseInt %)) reverse)
                                         (range(count x))))))

(binary-to-decimal "10010101")

;otras soluciones... mecachis!

;no entiendo cómo funciona esta, debe ser una propiedad de read-string
#(read-string (str "2r" %))

(#(read-string (str "2r" %)) "10010101")
(str "2r" "10010101")
;en cualquier caso está guay conocer read-string, ya sé como sacar un número de un string
;sin tener que usar parseInt
(read-string "1001")

; Lo yo he resuelto con (range (count x)) para obtener los índices de la iteración
; se puede conseguir con map-indexed
; ejemplo de solución que lo utiliza
; tampoco hace nada de Integer/parseInt, lo soluciona con un condicional if
(fn [s]
  (reduce + 0

          (map-indexed (fn [i x]
      (if (= \1 x)
        (Math/pow 2 i)
        0))
      (reverse s))))


; #157 Indexing sequences *********************************************************************
; ahora sí lo he clavao! en 2 min!

(map-indexed (fn [i x] [x i]) [0 1 3])
(map-indexed (fn [i x] [x i]) [:a :b :c :d :e])


; #88 Symmetric difference *********************************************************************

(defn symmetric-diff [x y]
 (set (mapcat #(remove (clojure.set/intersection x y) %) [x y])))

(symmetric-diff #{1 2 3 4 5 6} #{1 3 5 7})

; otra solución
#(into (clojure.set/difference % %2)
       (clojure.set/difference %2 %))

; #107 lexical closures *********************************************************************
; Given a positive integer n, return a function (f x) which computes xn.

(defn pow-n [n]
 (fn [x]
   (int (Math/pow x n))))

((pow-n 2) 16)


; #156 Dot product *********************************************************************

#(reduce + (map * % %2))

; #126 Through the Looking Class *********************************************************************
 (let [x (type (class class)) ]
  (and (= (class x) x) x))

; Otras soluciones
; Class
; java.lang.Class
; yo lo había intentado con class, pero es una función, no sabía la diferencia entre class y Class

; #118 Re-implement map *********************************************************************
; primer intento.
(defn __map [f x]
  (loop [result []
         x x]
    (if (seq x)
      (recur (concat result [(f (first x))]) (rest x))
      result)))
; segunda versión. A pesar de lazy-seq no funciona para seqs infinitas.
(defn _map
  [f x]
    (lazy-seq (reduce #(conj %1 (f %2)) [] x)))

;solución: es la definición de map
(defn map_
  ([f x]
   (lazy-seq
    (when-let [s (seq x)]
      (cons (f (first s)) (map_ f (rest s)))))))


(_map inc [0 1 2 3])


(= [1000000 1000001]
   (->> (map_ inc (range))
        (drop (dec 1000000))
        (take 2)))

; #135 Infix Calculator *********************************************************************
; mi solución... sigo con el p. loop metido en la cabeza
; mi problema para usar reduce era no saber como desestructurar...

(defn infixop [i & r]
  (loop [i i
         r r]
  (if (seq r)
    (recur ((first r) i (second r)) (rest (rest r)))
    i)))


(infixop 1 * 2 + 3 * 4)

; solución de amcnamaras... qué crack
; una función recursiva

(defn f [a o b & c]
  (if c
    (apply f (o a b) c)
    (o a b)))
(f 1 * 2 + 3 * 4)

; con reduce

(defn r [& c]
   (reduce #((first %2) %1 (second %2)) (first c) (partition 2 (rest c))))

(r 1 * 2 + 3 * 4)

; #120 Sum of square of digits *********************************************************************
; yeah yeah yeah

(defn sumsqu [x]
  (let [sq (fn [s] (apply + (map (fn [z] (Math/pow z 2))(map read-string (map str (into [] (str s)))))))]
    (count (filter #(> (sq %) %) x))))


(sumsqu (range 1000))


; #97 Pascal's Triangle *********************************************************************
; mi solución

(defn pascal [x]
  (loop [cnt x
         result [1]]
    (if (> cnt 1)
      (recur (dec cnt)  (vec (flatten [1 (map #(apply + %)(partition 2 1 result)) 1])))
      result
      )))


(pascal 11)

; siempre los hay más idiomáticos... por qué me cuesta tanto usar recur??? por qué zeñó por qué???
; este usa una fómula para el algoritmo que no sé de donde la saca
(defn pascal_ [i]
  (reduce
    #(conj %1 (* (last %1) (/ (- i %2) %2)))
    [1] (range 1 i)))


(pascal_ 4)


; Usando una función recursiva. Más parecido a mi solución
; lo de usar concat mola más que lo que yo he hecho con vec, flatten...
(fn pt_row [n]
  (if (= n 1)
    [1]
    (concat [1]
            (map (partial apply +) (partition 2 1 (pt_row (- n 1))))
            [1])))


; #26 fibonacci *********************************************************************

(defn fibonacci [x]
  (cond
    (= x 1) [1]
    (= x 2) [1 1]
    (>= x 3) (concat (fibonacci (- x 1)) [(#(+ (last %) (last (butlast %))) (fibonacci (- x 1)))])))


(fibonacci 10)


; #128 Recognize Playing Cards *********************************************************************
; Mi penca solución...
(defn rpc [x]
 (let [suit (let [s (str (first x))]
               (cond
                (= s "S") :spade
                (= s "H") :heart
                (= s "D") :diamond
                (= s "C") :club
               ))
       rank (let [r (str (last x))]
               (cond
                (= r "2") 0
                (= r "3") 1
                (= r "4") 2
                (= r "5") 3
                (= r "6") 4
                (= r "7") 5
                (= r "8") 6
                (= r "9") 7
                (= r "T") 8
                (= r "J") 9
                (= r "Q") 10
                (= r "K") 11
                (= r "A") 12
                ))]
   {:suit suit :rank rank}))

;otras soluciones más elegantes:

((#(fn [[a b]]
  {:suit (% a) :rank (if (% b) (% b) (- (int b) 50))}
  )
(zipmap "DHCTJQKA" [:diamond :heart :club 8 9 10 11 12])) "DQ")

;paso a paso:
;con esto construyo un mapa donde guardo todas las correspodencias, tanto las de los suits como las de los ranks (solo para las letras)
(zipmap "DHCTJQKA" [:diamond :heart :club 8 9 10 11 12])

((zipmap "DHCTJQKA" [:diamond :heart :club 8 9 10 11 12]) \D)
((zipmap "DHCTJQKA" [:diamond :heart :club 8 9 10 11 12]) \Q)

;una solución parecida a la mía pero un pelín más concisa gracias a condp
; no necesito usar str, si en vez de poner las letras como strings "A" las pongo como carácter \A
(fn [lp]
  (let [suit (condp = (first lp)
               \S :spade
               \H :heart
               \D :diamond
               \C :club)
        rank (condp = (second lp)
               \2 0
               \3 1
               \4 2
               \5 3
               \6 4
               \7 5
               \8 6
               \9 7
               \T 8
               \J 9
               \Q 10
               \K 11
               \A 12)]
    {:suit suit :rank rank}))

;esta es la caña
(fn [[s r]]
    { :suit ({\S :spade \H :heart \D :diamond \C :club} s)
      :rank ((zipmap "23456789TJQKA" (range)) r) })

;Tengo que acordarme de usar mejor destructuring! en este caso al escribir el argumento de la función como [s r],
;no hace falta luego usar first o second...
;Usa s y r como argumentos para buscar dentro de un mapa.


; #153 Pairwise Disjoint Sets *********************************************************************
; Given a set of sets, create a function which returns true if no two of those sets have any elements in
;common and false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them.

(defn pds [set]
   (every? empty? (for [x set y set :while (not= x y)] (clojure.set/intersection x y))))

(= (pds #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false)

;; #46 Flipping out *********************************************************************
;; Write a higher-order function which flips the order of the arguments of an input function.

(defn flip [funcion]
  (fn [a b] (funcion b a))
  )

;; #44 Rotate sequence
;; Write a function which can rotate a sequence in either direction.

(defn rotate [x coll]
  (let [a (mod x (count coll))]
  (concat (drop a coll) (take a coll))))

(= (rotate 2 [1 2 3 4 5]) '(3 4 5 1 2))

(= (rotate -2 [1 2 3 4 5]) '(4 5 1 2 3))

;; #43 Reverse Interleave *********************************************************************
;; Write a function which reverses the interleave process into x number of subsequences.


(defn rinterleave [coll paso]
 (apply map vector (partition paso coll)))


(apply map vector ['(0 1 2) '(3 4 5) '(6 7 8)])


(= (rinterleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))

(= (rinterleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))


;; #50 Split by Type *********************************************************************
;; Write a function which takes a sequence consisting of items with
;; different types and splits them up into a set of homogeneous sub-sequences.
;; The internal order of each sub-sequence should be maintained,
;; but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).


(defn split_type [x] (vals (group-by type x)))

(= (set (split_type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})

;; #55 Count Occurrences *********************************************************************
;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
;; Special restrictions: frequencies


(defn counto [coll]
  (let [a (group-by identity coll)]
    (zipmap (keys a) (map #(count %)(vals a)))))


;; para aclararme entre apply y map:
(max 2 3 4)
(max [2 3 4])
(apply max [2 3 4])
(map inc [1 2 3])
(map #(map inc %) [[1 2 3] [4 5 6]])
(map #(apply max %) [[1 2 3][4 5 6][7 8 9]])


(= (counto '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})
(= (counto [:b :a :b :a :b]) {:a 2, :b 3})
(= (counto [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

;; soluciones alternativas
;; esta es parecida a la mia. Usa reduce.
#(let [instances (group-by identity %)]
   (reduce (fn [acc v] (assoc acc v (-> (instances v) count)))
           {}
           (keys instances)))
;;
(fn [s]
  (into {}
    (for [[k v] (group-by identity s)] [k (count v)])))

;;
reduce #(update-in % [%2] (fnil inc 0)) {}

;;
(fn [c]
  (reduce #(assoc % %2 (count (filter #{%2} c))) {} c))


;; #56 Find Distinct Items *********************************************************************

;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
;; Special Restrictions: distinct

(= (_distinct [1 2 1 3 1 2 4]) [1 2 3 4])

(= (_distinct [:a :a :b :b :c :c]) [:a :b :c])

(= (_distinct (range 50)) (range 50))

;; esta solución funciona, pero no mantiene el orden. Por eso no sirve para (range 50)
(defn distinct_ [coll]
  (keys (group-by identity coll)))


;; Esta sí funciona, pero para ello, es muy importante que en el if, si es verdad el predicado,
;; la devolución sea %,
(defn _distinct [coll] (reduce #(if (some #{%2} %) % (conj % %2)) [] coll))


;; #58 Function composition *********************************************************************
;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and create a function that applies them from right-to-left.
;; Special Restrictions: comp

(= [3 2 1] ((comp_ rest reverse) [1 2 3 4]))

(= 5 ((comp_ (partial + 3) second) [1 2 3 4]))

(= true ((comp_ zero? #(mod % 8) +) 3 5 7 9))

(= "HELLO" ((comp_ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

;; Ejemplo de higher-order-function
(defn adder [saludo]
  (fn [nombre] (str saludo " " nombre)))

((adder "hola") "Espe")


;; mi solución
(defn comp_ [& fs]
  (fn [& args]
    (let [rfs (reverse fs)
          args (apply (first rfs) args)]
      (loop [result args
             f (next rfs)]
        (if f
          (recur ((first f) result)(next f))
          result)))))

((comp_ rest reverse) [1 2 3 4])

;; Otras soluciones
(defn comp-1 [& fs]
  (fn [& xs]
    (first (reduce #(vector (apply %2 %1)) xs (reverse fs)))))


;; A higher-order function is:
;;    a function that takes one or more functions as arguments
;;    or
;;    a function that returns a function

;; Esta función es del caso dos. Una función que devuelve una función

;; Esto es una higher-order-function de "tres grados": es decir una función (reduce),
;; que devuelve una función (UNO) que devuelve una función (DOS).

;; fs son las funciones que queremos componer
;; reduce aplica la función UNO sobre la lista de funciones fs que queremos componer
;; primero aplica f a g, es decir la primera función de la lista a la segunda y
;; crea una función temporal resultado de la composición de estas dos funciones
;; luego aplica esa función temporal a la siguiente función en la lista hasta que
;; no hay más funciones
;; Esta función UNO devuelve una función DOS que toma como argumentos los elementos
;; sobre los que queremos aplicar la composición de funciones.
;; Cuando se aplica (f (apply g args)) por primera vez, g es la última función de fs
;; y f es una composición de encadenada de las funciones anteriores que de van
;; aplicando en el orden inverso al que se han ido componiendo


(defn comp-2 [& fs] (reduce
                     (fn UNO [f g]
                        (fn DOS [& args]
                           (f (apply g args))))
                     fs))

(defn comp-2 [& fs] (reduce
                     (fn UNO [f g]
                       (do
                        (println "f1" f)
                        (println "g1" g)
                        (fn DOS [& args]
                         (do
                           (println "f2" f)
                           (println "g2" g)
                           (println "a" args)
                           (f (apply g args))))))
                     fs))


((comp-2 false? zero? (fn MODU [x] (mod x 8)) +) 3 5 7 9)





;; #54 Partition a sequence *********************************************************************
;; Write a function which returns a sequence of lists of x items each.
;; Lists of less than x items should not be returned.
;; Special Restrictions: partition, partition-all

;; Primera solución encontrada
(defn part [n coll]
  (loop [result []
         s coll]
    (if (seq s)
      (recur (if (>= (count s) n) (conj result (take n s)) result) (drop n s))
      result)))

(= (part 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))

(= (part 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))

(= (part 3 (range 8)) '((0 1 2) (3 4 5)))


;; #67 Prime numbers SIN RESOLVERRRR *********************************************************************
;; Write a function which returns the first x number of prime numbers.


(= (__ 5) [2 3 5 7 11])
(= (last (__ 100)) 541)


(defn nprimo [a b]
(reduce #(if (loop [den (dec %2)]
               (if (zero? (rem %2 den))
                 (if (= den 1)
                   true
                   false)
                 (recur (dec den)))) (conj % %2)) a b))



(defn nprimo [coll]
 (reduce #(if (loop [den (dec %2)]
               (if (zero? (rem %2 den))
                 (if (= den 1)
                   true
                   false)
                 (recur (dec den)))) (do (println % %2) (conj % %2))) [] coll))


(#(loop [den (dec %)]
               (if (zero? (rem % den))
                 (if (= den 1)
                   true
                   false)
                 (recur (dec den)))) 2)

(reduce #(conj % %2) [] [1 2 3])


%2 --> 1
(dec 1)
(rem 1 0)


(nprimo [2 3 4 5 6 7 8 9 10])


(defn primo? [x]
  (loop [den (dec x)]
    (if (zero? (rem x den))
      (if (= den 1)
        true
        false)
      (recur (dec den)))))


(primo? 3)
(primo? 0)

(defn _map
  [f x]
     (reduce #(conj %1 (f %2)) [] x))



(primo? 8)



;; #29 Get the Caps *********************************************************************

;; Write a function which takes a string and returns a new string containing only the capital letters.


(#(apply str (re-seq #"[A-Z]" %)) "HeLlO, WoRlD!")

;; #95 To Tree, or not to Tree *********************************************************************
;; Write a predicate which checks whether or not a given sequence represents a binary tree.
;; Each node in the tree must have a value, a left child, and a right child.


(= (tree? '(:a (:b nil nil) nil)) true)

(= (tree? '(:a (:b nil nil))) false)

(= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]]) true)

(= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false)

(= (tree? '(:a nil ())) false)


(defn tree? [[v i d :as nodo]]
  (if (= 3 (count nodo))
     (if-not (or (= nil v)(coll? v))
      (if (= i nil)
        (if (= d nil)
          true
          (if (coll? d)
            (tree? d)
            false))
        (if (coll? i)
          (tree? i)
          false))
      false)
      false))

;; Otras soluciones

(defn tree-2? [e]
  (or (nil? e)
      (and (counted? e)
           (= 3 (count e))
           (every? tree? (next e)))))

;; #59 Juxtaposition *********************************************************************
;; Take a set of functions and return a new function that takes a variable number of arguments
;; and returns a sequence containing the result of applying each function left-to-right to the argument list.

(= [21 6 1] ((juxt_ + max min) 2 3 5 1 6 4))

(= ["HELLO" 5] ((juxt_ #(.toUpperCase %) count) "hello"))

(= [2 6 4] ((juxt_ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))


(defn juxt_ [& fs]
  (fn [& args]
    (vec (for [f fs]
      (apply f args)))
    )
  )

((juxt_ + max min) 2 3 5 1 6 4)

;; otras soluciones


(defn juxt-1 [& f]
  (fn [& a]
    (map #(apply % a) f)))

((juxt-1 + max min) 2 3 5 1 6 4)


(fn [& fs]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args) ) [] fs)))

;; #70 Word Sorting *********************************************************************
;; Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be ignored.

(= (sorting  "Have a nice day.")
   ["a" "day" "Have" "nice"])

(= (sorting  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])

(= (sorting  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])


(defn sorting [x] (sort-by #(.toUpperCase %) (#(re-seq #"\w+" %) x)))

;; Otra solución. Es la misma pero usando el thread macro

#( ->> % (re-seq #"\w+") (sort-by clojure.string/lower-case))

;; #74 Filter Perfect Squares *********************************************************************

;; Given a string of comma separated integers, write a function which
;; returns a new comma separated string that only contains the numbers which are perfect squares.

(= (fps "4,5,6,7,8,9") "4,9")

(= (fps "15,16,25,36,37") "16,25,36")


((complement #(ratio? (rationalize (Math/sqrt %)))) 9)
(map read-string (re-seq #"\d+" "15,16,25,36,37"))

(defn fps [x] (let [sqrs (filter (complement #(ratio? (rationalize (Math/sqrt %)))) (map read-string (re-seq #"\d+" x)))]

               (apply str (butlast (interleave sqrs (repeat (count sqrs) \,))))))

;; otras soluciones

(fn [s] (clojure.string/join ","
  (filter #(= 0.0 (mod (Math/sqrt %) 1))
    (read-string (str "[" s "]")) )) )

;; importante:
(clojure.string/join "," [1 2 3])


;; #80 Perfect Numbers *********************************************************************

;; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect
;; number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.

(= (perfect 6) true)
(= (perfect 7) false)
(= (perfect 496) true)
(= (perfect 500) false)
(= (perfect 8128) true)

(defn perfect [x]  (if (= x (reduce + (rest (filter #(= 0 (mod % 1)) (map #(/ x %) (range 1 (inc x))))))) true false))

;; otras soluciones

(fn [n]
  (= n (apply + (filter #(= 0 (mod n %)) (range 1 n)))))

;; #77 Anagram Finder *********************************************************************

;; Write a function which finds all the anagrams in a vector of words. A word x is an anagram of
;; word y if all the letters in x can be rearranged in a different order to form y. Your function
;; should return a set of sets, where each sub-set is a group of words which are anagrams of each other.
;; Each sub-set should have at least two words. Words without any anagrams should not be included in the result.

(= (anagram ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})

(= (anagram ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})


(defn anagram [x] (into #{} (map set (filter #(> (count %) 1) (vals (group-by sort x))))))


;; #102 intoCamelCase *********************************************************************

;; When working with java, you often need to create an object with fieldsLikeThis, but you'd rather
;; work with a hashmap that has :keys-like-this until it's time to convert. Write a function which
;; takes lower-case hyphen-separated strings and converts them to camel-case strings.

(= (intoCamelCase "something") "something")

(= (intoCamelCase "multi-word-key") "multiWordKey")

(= (intoCamelCase "leaveMeAlone") "leaveMeAlone")


(defn intoCamelCase [x]
  (let [w (re-seq #"\w+" x)]
    (apply str (first w) (map #(clojure.string/capitalize %) (next w)))))


;; otras soluciones

(fn [s] (clojure.string/replace s #"-(\w)" #(str (.toUpperCase (% 1)))))


;; #60 Sequence Reductions ********************************************************************* SIN RESOLVER

;; Write a function which behaves like reduce, but returns each intermediate value of the reduction.
;; Your function must accept either two or three arguments, and the return sequence must be lazy.

;; Special Restrictions: reductions

(= (take 5 (reduction + (range))) [0 1 3 6 10])

(= (reduction conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])

(= (last (reduction * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)


(reductions + '(1 2 3))

(reduce + '(1 2 3))


(defn reduction
  ([f coll] (r f (first coll) coll))
  ([f v coll]
   (lazy-seq
    (loop [res [v]
           s   coll]
      (if s
        (recur (do (println res s) (conj res (f (last res) (first s)))) (next s))
        res)))))


(reduce * (range 1 5))

(r * 2 [3 4 5])


(defn factorial [x] (
     loop [cnt x acc 1]
     (if (zero? cnt)
       acc
       (recur (dec cnt) (* cnt acc))
       )))


(if (next []) true false)
(rest [])

(* 1 (last []))


(conj [(first [0 1 2])] (second [0 1 2]))


(cons 1 [1 2 3])

;; #69 Merge with a Function *********************************************************************
;; Write a function which takes a function f and a variable number of maps.
;; Your function should return a map that consists of the rest of the maps conj-ed
;; onto the first. If a key occurs in more than one map, the mapping(s) from the
;; latter (left-to-right) should be combined with the mapping in the result by
;; calling (f val-in-result val-in-latter)

;; Special Restrictions: merge-with

(= (mergew * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})

(= (mergew - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})

(= (mergew concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})

(defn mergew [f m & maps]
  (let [s (mapcat seq maps)]
    (reduce #(if (contains? % (first %2))
               (update-in % [(first %2)] f (second %2))
               (assoc % (first %2) (second %2))) m s)))

;; #86 Happy numbers *********************************************************************
;; Happy numbers are positive integers that follow a particular formula: take each
;; individual digit, square it, and then sum the squares to get a new number. Repeat
;; with the new number and eventually, you might get to a number whose squared sum is 1.
;; This is a happy number. An unhappy number (or sad number) is one that loops endlessly.
;; Write a function that determines if a number is happy or not.

(= (happy? 7) true)

(= (happy? 986543210) true)

(= (happy? 2) false)

(= (happy? 4) false)

(defn happy?
  ([n] (happy? n 0))
  ([n c]
   (let [h (reduce + (map #(* % %) (read-string (str "[" (clojure.string/join " " (str n)) "]"))))]
  (if (= n h)
    true
    (if (> c 8)
     false
     (happy? h (inc c)))))))


;; #115 The Balance of N *********************************************************************
;; A balanced number is one whose component digits have the same sum on the left and right
;; halves of the number. Write a function which accepts an integer n, and returns true iff n is balanced.

(= true (balanced? 11))

(= true (balanced? 121))

(= false (balanced? 123))

(= true (balanced? 0))

(= false (balanced? 88099))

(= true (balanced? 89098))

(= true (balanced? 89089))

(= (take 20 (filter balanced? (range)))
   [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])


 (defn balanced? [n]
   (let [s (->> n
            (str)
            (map #(- (int %) (int \0))))
         p (-> s
               count
               (/ 2)
               int)]
     (= (reduce + (take p s)) (reduce + (take-last p s)))))

;; #137 Digits and bases

;; Write a function which returns a sequence of digits of a non-negative number
;; (first argument) in numerical system with an arbitrary base (second argument).
;; Digits should be represented with their integer values, e.g. 15 would be [1 5]
;; in base 10, [1 1 1 1] in base 2 and [15] in base 16.

(= [1 2 3 4 5 0 1] (__ 1234501 10))

(= [0] (__ 0 11))

(= [1 0 0 1] (__ 9 2))

(= [1 0] (let [n (rand-int 100000)](__ n n)))

(= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))



;; Integer/MAX_VALUE es el mayor número que puede ser considerado como interger: 2147483647
Integer/MAX_VALUE

(class Integer/MAX_VALUE)
(class 2147483647)

;; No entiendo por qué si pregunto la clase de Integer/MAX_VALUE me dice que es
;; integer y si pregunto la de su valor (2147483647) me dice que es Long
;; Parece que clojure decide que es long si yo no le obligo a que sea int.

(int 2147483647) ;; puedo hacer coerce a integer
(int 2147483648) ;; aquí no me deja


(Integer/toString 1234501 10)

;; Con Integer/toString estamos accediendo directamente a un método de Java que toma
;; un integer y un radix (base)
;; public static String toString(int i, int radix)
;; If the radix is smaller than Character.MIN_RADIX or larger than Character.MAX_RADIX,
;; then the radix 10 is used instead.




































