
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

;tree-seq trata una colección anidada como un árbol, donde la colección y sus subcolecciones
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


;; #60 Sequence Reductions *********************************************************************

;; Write a function which behaves like reduce, but returns each intermediate value of the reduction.
;; Your function must accept either two or three arguments, and the return sequence must be lazy.

;; Special Restrictions: reductions

(= (take 5 (reduction-2 + (range))) [0 1 3 6 10])

(= (reduction-2 conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])

(= (last (reduction-2 * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;; Primera solución. No es válida porque por mucho que le ponga lazy-seq delante
;; no es lazy.

(defn reduction
  ([f coll] (reduction f (first coll) (next coll)))
  ([f i coll] (lazy-seq (reduce #(conj % (f (last %) %2)) [i] coll))))

;; En vez de reduce tengo que usar una función recursiva
(defn reduction-2
  ([f coll] (reduction-2 f (first coll) (next coll)))
  ([f i coll]
  (concat [i]
   (when-let [s coll]
     (lazy-seq (reduction-2 f (f i (first s)) (next s)))
      ))))



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

;; #137 Digits and bases *********************************************************************

;; Write a function which returns a sequence of digits of a non-negative number
;; (first argument) in numerical system with an arbitrary base (second argument).
;; Digits should be represented with their integer values, e.g. 15 would be [1 5]
;; in base 10, [1 1 1 1] in base 2 and [15] in base 16.

(= [1 2 3 4 5 0 1] (digits 1234501 10))

(= [0] (digits 0 11))

(= [1 0 0 1] (digits 9 2))

(= [1 0] (let [n (rand-int 100000)](digits n n)))

(= [16 18 5 24 15 1] (digits Integer/MAX_VALUE 42))

(defn digits [number base]
  (if (= number 0)
      [0]
      (let [ch (concat (map char (range 48 58)) (map char (range 65 91)))
            dig (zipmap ch (range))]
        (loop [num number
               acc []]
          (if (zero? num)
              (map #(dig %) (reverse acc))
              (recur (int (/ num base))
                     (conj acc (nth ch (mod num base)))))))))


;; Otras soluciones en forclojure

;; Esta es obra maestra! Es recursiva en vez de usar loop... of course!!!
(fn base [x b] (if (< x b) [x] (conj (base (quot x b) b) (mod x b))))

;; Primer predicado del if : [x]
;; Si el número es menor que la base, el número en esa base es directamente él mismo

;;mi proceso:
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
;; Character.MAX_RADIX es igual a 36 (los números del 0 al 9 más las letras del abecedario)

Character/MAX_RADIX
(count "0123456789abcdefghijklmnopqrstuvwxyz")

;; Por eso pasa esto:
(= (Integer/toString Integer/MAX_VALUE 10) (Integer/toString Integer/MAX_VALUE 42))

;; Los caracteres que se pueden utilizar son 0123456789abcdefghijklmnopqrstuvwxyz
;; These are '\u0030' through '\u0039' and '\u0061' through '\u007A'.
;; De forma que si el radix es 16, los caracteres usados son 0123456789abcdef.

;; No sé cómo solucionar la limitación de Character/MAX_RADIX

(Integer/decode "0xf")



;; He encontrado esto en internet que hace lo mismo pero de forma "artesanal"
(def characters
  (concat (map char (range 48 58)) (map char (range 65 91))))

characters

(def conversion-table
  (zipmap
   characters
   (range)))

(into (sorted-map) conversion-table)

(defn base-n-to-base-10
  [^String string ^Integer base]
  (let [string (clojure.string/upper-case string)]
    (assert (every? #(< (conversion-table %) base) string))
    (loop [num string
           acc 0]
      (if (seq num)
        (recur (drop 1 num) (+ (* base acc) (get conversion-table (first num))))
acc))))

(defn base-10-to-base-n
  [^Integer number ^Integer base]
  (loop [num number
         acc []]
    (if (zero? num)
      (clojure.string/join (reverse acc))
      (recur (int (/ num base))
             (conj acc (nth characters (mod num base)))))))

(base-10-to-base-n Integer/MAX_VALUE 42)

;; #158 Decurry *********************************************************************
;; Write a function that accepts a curried function of unknown arity n.
;; Return an equivalent function of n arguments. You may wish to read this.

(= 10 ((curry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))

(= 24 ((curry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4))

(= 25 ((curry (fn [a]
                (fn [b]
                  (* a b))))
       5 5))

;; mi solución
(defn curry [fun]
  (fn [& args]
   (loop [f fun
          a args]
    (if a
      (recur (f (first a))(next a))
      f))))

;; otras soluciones
(defn curry [f]
  (fn [& args]
    (reduce #(% %2) f args)
    ))

;; #144 Oscilrate *********************************************************************

;; Write an oscillating iterate: a function that takes an initial value and
;; a variable number of functions. It should return a lazy sequence of the
;; functions applied to the value in order, restarting from the first function
;; after it hits the end.

(= (take 3 (oscilate 3.14 int double)) [3.14 3 3.0])

(= (take 5 (oscilate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])

(= (take 12 (oscilate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])

(defn oscilate [n & fns]
  (lazy-seq  (reductions #(%2 %) n (cycle fns))  )
  )

;; #76 Intro to Trampoline *********************************************************************
;; The trampoline function takes a function f and a variable number of parameters.
;; Trampoline calls f with any parameters that were supplied. If f returns a function,
;; trampoline calls that function with no arguments. This is repeated, until the return
;; value is not a function, and then trampoline returns that non-function value. This is
;; useful for implementing mutually recursive algorithms in a way that won't consume the stack.

(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))


(defn foo [x]
   (if (< x 0)
     (println "done")
     #(foo (do (println :x x) (dec x)))))

((((((((((((foo 10))))))))))))
(trampoline foo 10)

;; #85 Power Set *********************************************************************
;; Write a function which generates the power set of a given set.
;; The power set of a set x is the set of all subsets of x, including the empty set and x itself.
(= (powerset #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(= (powerset #{}) #{#{}})
(= (powerset #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(= (count (powerset (into #{} (range 10)))) 1024)

(defn powerset [s]
  (reduce (fn [p x] (set (concat p (map #(conj % x)  p)))) #{#{}} s))

;; #78 Reimplement trampoline *********************************************************************
;; Special Restrictions: trampoline

(= (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (trampolin triple 2))
  82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
          (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
    (map (partial trampolin my-even?) (range 6)))
  [true false true false true false])

(defn trampolin [fun & args]
  (let [f (apply fun args)]
    (if (fn? f) (trampolin f) f)))
;; Pensaba al principio que en la segunda iteración, cuando no hay argumentos, apply me iba a dar
;; problemas porque necesita dos argumentos. No hay problema porque cuando se aplica (tranpolin f)
;; sin argumentos, args es nil, y (apply f nil) no da problemas. Por ejemplo:
(apply + nil)

;; #98 Equivalence Classes *********************************************************************
;; A function f defined on a domain D induces an equivalence relation on D, as follows:
;; a is equivalent to b with respect to f if and only if (f a) is equal to (f b).
;; Write a function with arguments f and D that computes the equivalence classes of D with respect to f.

(= (eq #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})
(= (eq #(rem % 3) #{0 1 2 3 4 5 })
   #{#{0 3} #{1 4} #{2 5}})
(= (eq identity #{0 1 2 3 4})
   #{#{0} #{1} #{2} #{3} #{4}})
(= (eq (constantly true) #{0 1 2 3 4})
   #{#{0 1 2 3 4}})

(defn eq [f s]
  (->> s
       (group-by f)
       (vals)
       (map set)
       (set)))

;; #105 Identify keys and values *********************************************************************
;; Given an input sequence of keywords and numbers, create a map such that each key in
;;the map is a keyword, and the value is a sequence of all the numbers (if any) between
;; it and the next keyword in the sequence.

(= {} (kv []))
(= {:a [1]} (kv [:a 1]))
(= {:a [1], :b [2]} (kv [:a 1, :b 2]))
(= {:a [1 2 3], :b [], :c [4]} (kv [:a 1 2 3 :b :c 4]))

;; primera solución. Me funciona aquí, pero no en la página.
(defn kv [s]
    (reduce #(if (keyword? %2)
                 (assoc % %2 [])
                 (update-in
                  %
                  [(first (last %))]
                  (fn [x] (conj x %2) ))) (array-map) s))

;; Se supone que usando un array-map, en vez de un hash-map, mantenemos
;; las inserciones en el mapa en orden. Por lo tanto, no debería importar
;; si los keywords que nos dan en la sequencia están ordenados o no.
;; El problema es que en la página de 4clojure no me funiona esta solución.
;; He cambiado array-map por sorted-map y sí se lo ha tragado, pero eso es
;; porque da la casualidad de que nos dan todos los keywords ordenados.

(def a (sorted-map))
(last (assoc (assoc (assoc a :c 1) :b 2) :a 8))

(def b (hash-map)) ;; No garantiza que last sea [:b 8]
(last (assoc (assoc (assoc b :c 1) :a 2) :b 8))

(def c (array-map)) ;; Debería garantizar que last sea [:c 8]
(last (assoc (assoc (assoc c :b 1) :a 2) :c 8))

;; Hay otro acercamiento que es trabajar de la misma forma pero con un vector
;; y meterlo todo en un mapa al final:

(fn [v]
  (into {} (reduce
    #(if (keyword? %2)
	     (conj % [%2 []] )
		 (conj (pop %) [(-> % peek first) (conj (-> % peek peek) %2)]))  [] v)))

;; #147 Pascal's Trapezoid *********************************************************************
;; Write a function that, for any given input vector of numbers, returns an infinite
;; lazy sequence of vectors, where each next one is constructed from the previous
;; following the rules used in Pascal's Triangle. For example, for [3 1 2], the next
;; row is [3 4 3 2].
;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use
;; an arithmetic operator like + and the result is too large to fit into a 64-bit integer,
;; an exception is thrown. You can use +' to indicate that you would rather overflow into
;; Clojure's slower, arbitrary-precision bigint.

(= (second (pascal [2 3 2])) [2 5 5 2])
(= (take 5 (pascal [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
(= (take 2 (pascal [3 1 2])) [[3 1 2] [3 4 3 2]])
(= (take 100 (pascal [2 4 2])) (rest (take 101 (pascal [2 2]))))


(defn pascal [v]
   (iterate (fn [x] (map #(apply +' %) (partition 2 1 (cons 0 (conj (vec x) 0))))) v))

(take 10 (pascal [1]))

;; #67 Prime Numbers *********************************************************************
;; Write a function which returns the first x number of prime numbers.
(= (n-primes 2) [2 3])
(= (n-primes 5) [2 3 5 7 11])
(= (last (n-primes 100)) 541)


(defn n-primes [n]
  (take n
      (filter #(loop [den (dec %)]
                  (if (zero? (rem % den))
                      (= den 1)
                      (recur (dec den)))) (nnext (range)))))

;; Otra solución más bonita
(fn [n]
  (->>
  (range)
  (drop 2)
  (filter (fn [x] (every? #(< 0 (mod x %)) (range 2 x))))
  (take n)))
;; Para comprobar si un número es primo lo que hace es comprobar #(< 0 (mod x %))
;; para todos los números que hay por debajo de él hasta el 2 (range 2 x)

;; #53 Longest Increasing Sub-Seq *********************************************************************
;; Given a vector of integers, find the longest consecutive sub-sequence of
;; increasing numbers. If two sub-sequences have the same length, use the one that occurs first.
;; An increasing sub-sequence must have a length of 2 or greater to qualify.

(= (longest [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (longest [5 6 1 3 2 7]) [5 6])
(= (longest [2 3 3 4 5]) [3 4 5])
(= (longest [7 6 5 4]) [])

(defn longest [coll]
  (let [seqs (reduce #(if (= (+ 1 (last (last %))) %2)
                          (assoc-in % [(- (count %) 1) (count (last %))] %2)
                          (conj % (vector %2))) [[(first coll)]] (next coll))
        max (apply max (map count seqs))
        candidate (first (drop-while  #(not= max (count %)) seqs))]
    (if (> (count candidate) 1)
      candidate
      [])))

;; Otras soluciones:

(fn lss [v]
  (loop [[h & t] v, lsf [], cur []]
    (if h
       (if (and t (> (first t) h))
                (recur t lsf (conj cur h))
                (if (>= (count cur) (count lsf)) (recur t (conj cur h) []) (recur t lsf [])))
        (if (next lsf) lsf []))))

(fn [-seq]
   (->> (map
         (fn [it index]
           (loop [next-items (next (last (split-at index -seq))) stock [it]]
             (if (= (first next-items) (inc (last stock)))
               (recur (rest next-items) (conj stock (first next-items)))
               stock
               )))
         -seq (range (count -seq)))
        (map  #(if (= 1 (count %)) [] %))
        (sort #(compare (count %2) (count %1)))
        (first)))

;; #75 Euler's Totient Function
;; Two numbers are coprime if their greatest common divisor equals 1. Euler's totient
;; function f(x) is defined as the number of positive integers less than x which are
;; coprime to x. The special case f(1) equals 1. Write a function which calculates Euler's totient function.

(= (euler 1) 1)
(= (euler 10) (count '(1 3 7 9)) 4)
(= (euler 40) 16)
(= (euler 99) 60)


(defn euler [n]
  (if (= n 1)
      1
      (let [gcd (fn gcd [x y] (if (= y 0) x (gcd y (rem x y))))]
        (count (filter #(= 1 (gcd n %)) (range 1 n))))))


;; #73 Analyze a Tic-Tac-Toe Board
;; A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x,
;; O is represented by :o, and empty is represented by :e. A player wins by placing three Xs
;; or three Os in a horizontal, vertical, or diagonal row. Write a function which analyzes a
;; tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won.

(= nil (tic-tac [[:e :e :e]
                 [:e :e :e]
                 [:e :e :e]]))
(= :x (tic-tac [[:x :e :o]
                [:x :e :e]
                [:x :e :o]]))
(= :o (tic-tac [[:e :x :e]
                [:o :o :o]
                [:x :e :x]]))
(= nil (tic-tac [[:x :e :o]
                 [:x :x :e]
                 [:o :x :o]]))
(= :x (tic-tac [[:x :e :e]
                [:o :x :e]
                [:o :e :x]]))
(= :o (tic-tac [[:x :e :o]
                [:x :o :e]
                [:o :e :x]]))
(= nil (tic-tac [[:x :o :x]
                 [:x :o :x]
                 [:o :x :o]]))

(defn tic-tac [board]
 (let [A board
       At (apply map vector A)
       As (map reverse A)
       diag (fn [b] (let [[[x _ _] [_ y _] [_ _ z]] b] [x y z]))
       test (fn [y] (ffirst (drop-while #(apply not= %) y)))
       sol? (fn [sol] (if (= sol :e) nil sol))]
   (cond
    (when (test A) true) (sol? (test A))
    (when (test At) true) (sol? (test At))
    (apply = (diag A)) (sol? (first (diag A)))
    (apply = (diag As)) (sol? (first (diag As)))
    :else nil
    )))

;; Otra solución
(fn ttt [board]
  (let [rows board
        columns (apply map list board)
        diagonals [[(first (first board)) (second (second board)) (nth (nth board 2) 2)]
                   [(first (nth board 2)) (second (second board)) (nth (first board) 2)]]
        lines (concat rows columns diagonals)
        matches (filter #(apply = %) lines)
        winner (first (first (filter #(not-any? #{:e} %) matches)))]
    winner))

(fn [b]
  (some {[:o :o :o] :o [:x :x :x] :x}
        (concat b (partition 3 (apply interleave b))
                  (for [i [[0 4 8][2 4 6]]]
                    (map #(nth (flatten b) %) i)))))

;; #110 Sequence of pronunciations
;; Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers.
;; A pronunciation of each element in the sequence consists of the number of repeating identical
;; numbers and the number itself. For example, [1 1] is pronounced as [2 1] ("two ones"),
;; which in turn is pronounced as [1 2 1 1] ("one two, one one").
;; Your function should accept an initial sequence of numbers, and return an infinite lazy
;; sequence of pronunciations, each element being a pronunciation of the previous element.

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (pron [1])))
(= [3 1 2 4] (first (pron [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (pron [1]) 6))
(= 338 (count (nth (pron [3 2]) 15)))

(defn pron [s]
 (next (iterate
   #(let [seq (partition-by identity %)]
    (interleave (map count seq) (map first seq))) s)))

;; #65 Black Box Testing *********************************************************************

;; Clojure has many sequence types, which act in subtly different ways. The core
;; functions typically convert them into a uniform "sequence" type and work with
;; them that way, but it can be important to understand the behavioral and
;; performance differences so that you know which kind is appropriate for your application.

;; Write a function which takes a collection and returns one of
;; :map, :set, :list, or :vector - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list?
;; - the point is to poke at them and understand their behavior.

;; Special Restrictions
;; class
;; type
;; Class
;; vector?
;; sequential?
;; list?
;; seq?
;; map?
;; set?
;; instance?
;; getClass

(= :map (black-box {:a 1, :b 2}))
(= :list (black-box (range (rand-int 20))))
(= :vector (black-box [1 2 3 4 5 6]))
(= :set (black-box #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map black-box [{} #{} [] ()]))

(defn black-box [c]
 (let [test ((fn [x] (reduce #(conj % %2) (empty x) [[1 1][1 1][2 2]])) c)]
   (if (= 3 (count test))
     (if (= 1 (ffirst test))
         :vector
         :list)
     (if (contains? test [1 1])
         :set
         :map)
   )))

;; #93 Partially Flatten a Sequence *********************************************************************
;; Write a function which flattens any nested combination of sequential things
;; (lists, vectors, etc.), but maintains the lowest level sequential items.
;; The result should be a sequence of sequences with only one level of nesting.

(= (fltp [["Do"] ["Nothing"]])
   [["Do"] ["Nothing"]])
(= (fltp [[[[:a :b]]] [[:c :d]] [:e :f]])
   [[:a :b] [:c :d] [:e :f]])
(= (fltp '((1 2)((3 4)((((5 6)))))))
   '((1 2)(3 4)(5 6)))


(defn fltp [x]
  (filter #((complement sequential?) (first %)) (tree-seq #(sequential? (first %)) seq x)))

;; #92 Read Roman numerals *********************************************************************

;; Roman numerals are easy to recognize, but not everyone knows all the rules
;; necessary to work with them. Write a function to parse a Roman-numeral
;; string and return the number it represents.

;; You can assume that the input will be well-formed, in upper-case, and
;; follow the subtractive principle. You don't need to handle any numbers
;; greater than MMMCMXCIX (3999), the largest number representable with ordinary letters.

(= 14 (rom "XIV"))
(= 827 (rom "DCCCXXVII"))
(= 3999 (rom "MMMCMXCIX"))
(= 48 (rom "XLVIII"))

 (defn rom [s]
   (let [A {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
         R [["IV" "IIII"]
            ["IX" "VIIII"]
            ["XL" "XXXX"]
            ["XC" "LXXXX"]
            ["CD" "CCCC"]
            ["CM" "DCCCC"]]]
     (apply + (map A (reduce #(clojure.string/replace % (first %2)(second %2)) s R )))))

;; Otras soluciones
(fn [s]
  (let [roman {"M" 1000 "CM" 900 "D" 500 "CD" 400
    "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9
    "V" 5 "IV" 4 "I" 1}]
    (reduce + (map roman
      (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s)))))

(re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" "MMMCMXCIX")

(fn rrn [roman-numeral]
  (let [digits (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} (take 2 roman-numeral))]
    (cond (empty? digits) 0
          (and (= 2 (count digits))
               (< (first digits) (second digits))) (- (rrn (rest roman-numeral)) (first digits))
          :else (+ (rrn (rest roman-numeral)) (first digits)))))

;; #79 Triangle Minimal Path *********************************************************************

;; Write a function which calculates the sum of the minimal path through a triangle.
;; The triangle is represented as a collection of vectors. The path should start at
;; the top of the triangle and move to an adjacent number on the next row until the
;; bottom of the triangle is reached

(= 7 (B '([1]
          [2 4]
         [5 1 4]
        [2 3 4 5]))) ; 1->2->1->3

(= 20 (f '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4]))) ; 3->4->3->2->7->1

(defn B [triangulo]
  (apply min (reduce
               (fn [a b] (let [s (flatten (map (fn [x y] (map #(+ x %) y)) a (partition 2 1 b)))]
                           (map #(apply min %) (partition 2 (concat [(first s)] s [(last s)])))))
               triangulo)))

;; Otras soluciones
;; amcnamara es un puto genio
;; esta no la ent
(defn f [[[a] & b]]
      (+ a (if b (min (f (map rest b))
                      (f (map butlast b))) 0)))

;; las dos siguientes son el mismo principio. Uno usa infinity y el otro un numero muy grande
#(apply min
     (reduce
      (fn [a v] (map min
                     (map + (cons Double/POSITIVE_INFINITY a) v)
                     (map + (concat a [Double/POSITIVE_INFINITY]) v)))
      %))

(fn [x] (apply min
               (reduce (comp #(map min (concat % [100000]) (concat [100000] %)) (partial map +)) [0] x)))


;; también se puede hacer así, que es lo que yo he hecho arriba cuando he usado (first s) y (last s)
;; para garantizar que el mínimo en ese caso se compare consigo mismo.
(#(apply min
     (reduce
      (fn [a v] (map min
                     (map + (cons (first a) a) v)
                     (map + (concat a [(last a)]) v)))
      %)))

(map min (map + (cons 3 [3]) [2 4])
         (map + (concat [3] [3]) [2 4]))
;; (5 7)
;; (5 7)

(map min (map + (cons 5 '(5 7)) [1 9 3])
         (map + (concat '(5 7) [7]) [1 9 3]))
;; (5 5 7)     (5 7 7)
;; (1 9 3) +   (1 9 3) +
;; ---------   ---------
;; (6 14 10)   (6 16 10)

;; #96 Beauty is Symmetry *********************************************************************

;; Let us define a binary tree as "symmetric" if the left half of the tree is
;; the mirror image of the right half of the tree. Write a predicate to determine
;; whether or not a given binary tree is symmetric. (see To Tree, or not to Tree
;; for a reminder on the tree representation we're using).

(= (sim-tree? '(:a (:b nil nil) (:b nil nil))) true)
(= (sim-tree? '(:a (:b nil nil) nil)) false)
(= (sim-tree? '(:a (:b nil nil) (:c nil nil))) false)
(= (sim-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
(= (sim-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (sim-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)


(defn sim-tree? [[v izq der]]
  (= izq ((fn rev [[v i d]] [v (if (= 3 (count d)) (rev d) d) (if (= 3 (count i)) (rev i) i)]) der)))

;; #146 Trees into tables *********************************************************************

;; Because Clojure's for macro allows you to "walk" over multiple sequences
;; in a nested fashion, it is excellent for transforming all sorts of sequences.
;; If you don't want a sequence as your final output (say you want a map), you
;; are often still best-off using for, because you can produce a sequence and
;; feed it into a map, for example.

;; For this problem, your goal is to "flatten" a map of hashmaps. Each key
;; in your output map should be the "path"1 that you would have to take in the
;; original map to get to a value, so for example {1 {2 3}} should result in
;; {[1 2] 3}. You only need to flatten one level of maps: if one of the values
;; is a map, just leave it alone.

;; 1 That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])

(= (tree '{a {p 1, q 2}
         b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})

(= (tree '{[1] {a b c d}
         [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})

(= (tree '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})


(defn tree [m]
  (->>  (for [[k v] m
              [k2 v2] v]
          [[k k2] v2])
        (apply concat)
        (apply hash-map)))

;; Otras soluciones
(fn [m] (into {}
              (for [[k v] m
                    [k2 v2] v]
                [[k k2] v2])))

;; #108 Lazy Searching *********************************************************************

;; Given any number of sequences, each sorted from smallest to largest,
;; find the smallest single number which appears in all of the sequences.
;; The sequences may be infinite, so be careful to search lazily.

(= 3 (ls [3 4 5]))
(= 4 (ls [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 7 (ls (range) (range 0 100 7/6) [2 3 5 7 11 13]))
(= 64 (ls (map #(* % % %) (range)) ;; perfect cubes
          (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
          (iterate inc 20))) ;; at least as large as 20

(defn ls [s & ss]
  (letfn [(lazy-search [n coll] (if (>= n (first coll)) (if (= n (first coll)) n (recur n (next coll))) nil))]
    (if
      (every? #(= (first s) %) (map (partial lazy-search (first s)) ss))
      (first s)
      (recur (next s) ss))))

;; Otras soluciones

#(loop [c %&]
   (let [[a & b] (vec (apply sorted-set (map first c)))]
     (if b (recur (for [[i & j :as k] c] (if (= a i) j k))) a)))

(#(vec (apply sorted-set (map first %))) [[1 2 3 4 5 6 7] [0.5 1.5 2.5 3 4 19] [2 3 4 5 7 11 13]])

((fn [&colls]
  (let [c &colls]
    (for [[i & j :as k] c]
      i) )  ) [[1 2 3 4 5 6 7] [0.5 1.5 2.5 3 4 19] [2 3 4 5 7 11 13]])
(= [0.5 1 2] '(1 0.5 2))


;; IMPORTANTEEEEEEEEEEEE
;; cómo colapsar nested reductions

(def new-board [[nil nil nil][nil nil nil][nil nil nil]])
(def board [[1 2 3][4 5 6][7 8 9]])

(reduce
 (fn [new-board x]
   (reduce (fn [new-board y] (assoc-in new-board [x y] (if  (even? y) 2)))
           new-board [0 1 2]))
 board [0 1 2])

(defn populate [board]
  (reduce
   (fn [new-board [x y]]
     (assoc-in new-board [x y] (if (and (even? x)(even? y)) :e (get-in board [x y]))))
   board (for [x [0 1 2] y [0 1 2]] [x y])))

(populate board)

(for [x [0 1 2] y [0 1 2]] [x y])


;; #114 Global take-while *********************************************************************

;; take-while is great for filtering sequences, but it limited: you can only examine a single
;; item of the sequence at a time. What if you need to keep track of some state as you go over the sequence?
;; Write a function which accepts an integer n, a predicate p, and a sequence. It should
;; return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate

(= [2 3 5 7 11 13]
   (TW 4 #(= 2 (mod % 3))
         [2 3 5 7 11 13 17 19 23]))

(= ["this" "is" "a" "sentence"]
   (TW 3 #(some #{\i} %)
         ["this" "is" "a" "sentence" "i" "wrote"]))

(= ["this" "is"]
   (TW 1 #{"a"}
         ["this" "is" "a" "sentence" "i" "wrote"]))


(defn TW [n pred coll] (loop [c coll counter 0 result []]
                         (if (sequential? c)
                             (if (< counter n)
                                 (recur (next c) (if (pred (first c)) (inc counter) counter) (conj result (first c)))
                                  (pop result))
                               result)))

;; Esta es mi primera solución, aunque creo que no es lazy

;; #132 Insert between two items *********************************************************************
;; Write a function that takes a two-argument predicate, a value, and a collection;
;; and returns a new collection where the value is inserted between every two items that satisfy the predicate.

(= '(1 :less 6 :less 7 4 3) (insertvalue < :less [1 6 7 4 3]))
(= '(2) (insertvalue > :more [2]))
(= [0 1 :x 2 :x 3 :x 4]  (insertvalue #(and (pos? %) (< % %2)) :x (range 5)))
(empty? (insertvalue > :more ()))
(= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (insertvalue (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same))))

;; Primera solución que no sirve porque no es lazy y no cumple el último test
(defn insertvalue [pred val coll]
  (if (empty? coll)
    ()
    (reduce #(if (pred (last %) %2) (conj % val %2) (conj % %2)) [(first coll)] (rest coll)))
  )

;; Lo convierto en una función recursiva
(defn insertvalue [pred val coll]
     (lazy-seq
     (when-let [s (seq coll)]
         (if
         (if (second s) (pred (first s) (second s)) false) ;; para que pueda hacer la comprobación
                                                           ;; primero tengo que comprobar si no hemos
                                                           ;; llegado al último elemento y por lo tanto no hay second
         (cons (first s) (cons val (insertvalue pred val (rest s))))
         (cons (first s) (insertvalue pred val (rest s)))))))

(defn insertvalue [pred val coll]
     (lazy-seq
     (when-let [s (seq coll)]
         (if
         (if (second s) (pred (first s) (second s)) false)
         (cons (first s) (cons val (insertvalue pred val (rest s))))
         (cons (first s) (insertvalue pred val (rest s)))))))

;; #104 Write Roman Numerals *********************************************************************
;;This is the inverse of Problem 92, but much easier. Given an integer
;; smaller than 4000, return the corresponding roman numeral in uppercase, adhering to the subtractive principle.

(= "I" (romanos 1))
(= "XXX" (romanos 30))
(= "IV" (romanos 4))
(= "CXL" (romanos 140))
(= "DCCCXXVII" (romanos 827))
(= "MMMCMXCIX" (romanos 3999))
(= "XLVIII" (romanos 48))

(defn romanos [n]
  (let [romans {1 "I" 10 "X" 100 "C" 1000 "M"}
        roman-num (apply str
                         (map romans
                              (reduce #(concat % (repeat (mod (quot n %2) 10) %2))
                                      [] [1000 100 10 1])))]
    (println roman-num)
    (-> roman-num
        (clojure.string/replace "CCCCCCCCC" "CM")
        (clojure.string/replace "CCCCC" "D")
        (clojure.string/replace "CCCC" "CD")
        (clojure.string/replace "XXXXXXXXX" "XC")
        (clojure.string/replace "XXXXX" "L")
        (clojure.string/replace "XXXX" "XL")
        (clojure.string/replace "IIIIIIIII" "IX")
        (clojure.string/replace "IIIII" "V")
        (clojure.string/replace "IIII" "IV"))))

;; Otras soluciones
(#(clojure.pprint/cl-format nil "~@R" %) 1958)


(fn _
    ([n] (_ (reverse
              (map read-string
                   (map str
                        (str n))))
            "IVXLCDM.."))
    ([[d & e] [r s & t]]
     (if d
       (apply str
              (_ e t)
              (cond
                (< d 4) (repeat d r)
                (= d 4) [r s]
                (< d 9) (cons s (repeat (- d 5) r))
                1 [r (first t)]))
       )))











