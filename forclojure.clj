
(def a [[[a]]])
(def v [1 2 3 2 1])
(def w [1 2 2 2 3 4 4 4 4 4 2 5 7])
(def flat [1 [2 3] 4 [5 6]])
(def k [:a :b :c :d :e])
(def s #{1 2 3 4 5})
(def lista (list 1 2 3 4))
(def string "racecar")
(def mayus "Hellooooooooo Woooooooooooorld")



;INTERLEAVE sin usar interleave. *********************************************************************
;Esta es mi solución poco elegante
;mi tendencia todavía al imperative control flow

(defn interl [s t] (loop [result []
              s s
              t t]
(if (and (seq s) (seq t))
(recur (concat result [(first s) (first t)]) (rest s) (rest t))
  result)))


(interl v w)

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
(sequential? v)
(seq? v)

;Tengo varios ejemplos pero no sé cómo funcionan o por qué a veces no funcionan.
(tree-seq vector? seq flat)

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
((complement sequential?) v)

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
(quitarep mayus)

;mi poco elegante solución :S
(defn elimrep [s]
  (let [svec (vec s)]
   (loop [cnt 0
         result [(first svec)]]
    (if (< cnt (- (count svec) 1))
      (recur (inc cnt) (if (= (svec cnt)(svec (+ cnt 1))) result (concat [(svec (+ cnt 1))] result)))
      (reverse result)))))

(apply str (elimrep mayus))

;otras soluciones más elegantes!

(#(map first (partition-by identity %)) w)

(#(partition-by identity %) w) ;(partition-by f coll) Aplica f a cada valor en la colección, cortandola cada vez
                               ; que f devuelve un nuevo valor.

;otra solución
(reduce #(do (println %1 %2)(if (= (last %1) %2) %1 (conj %1 %2))) [] w)
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






;fibonacci SIN RESOLVER *********************************************************************


#_(take 10
      (iterate
       (fn [[x y]] (+ y x)) [1 1] ))


#_(take 5 (iterate (partial (fn [[x y]] (+ x y))) [1 1]))



(defn secret-modifier [id]
  (let [after (int (rand 10000))]
  (println after)
  (+ after 10))
  )

(secret-modifier 2)

((fn [[x y]] (+ y x)) [3 1] )



;FACTORIAL *********************************************************************

(defn factorial [x] (
     loop [cnt x acc 1]
     (if (zero? cnt)
       acc
       (recur (dec cnt) (* cnt acc))
       )))
(factorial 6)


; filtrado de mayúsculas SIN RESOLVER!!! *********************************************************************
; No tentiendo las Regex... pfffff

(re-find #"[A-Z]+" "StrIng")

(def a (re-matcher #"[A-Z]" "StrInG"))
(re-matcher #"[A-Z]" "StrInG")
(re-find a)

(re-groups a)


;MAX. Encontrar el máx de una serie de números. Esta es mi opción: *********************************************************************

((fn [& numeros] (last(sort(apply conj () numeros)))) 1 7 3)

; esta es más simple. Yo pensaba que sort sólo se podía aplicar a una colección
((fn [& numeros] (last (sort numeros))) 1 7 3)


;PALINDROME *********************************************************************
(= v (reverse v))

(reverse string)

(apply str (reverse string))

(#(cond
   (coll? %)  (= % (reverse %))
   (string? %) (= % (apply str (reverse %)))
   )
  string)

;otras coluciones. En vez de volver a convertir en string la secuencia para que coincida con el
; original, lo que hace es convertir primero el original en una secuencia. De esta forma sirve
; tanto para collecciones como para strings

(#(= (seq %) (reverse %)) string)



;SECUENCIA INVERTIDA. mi solución. sin usar rseq *********************************************************************
(#(seq (replace (vec %) (vec (take (count %) (iterate dec (- (count %) 1)))))) s)

;yo no había tenido en cuenta esta posibilidad. No sabía como intertir el orden
; de los valores que me da range: asignando al paso -1
(range (count v) -1 -1)


;soluciones más simples al ejemplo anterior
(into () v) ; () es una secuencia vacía. Con into metemos los elementos de v en la secuencia
            ; vacía, al hacerlo empieza por el último y acaba por el primero. por eso la
            ; secuencia aparece invertida
(into () s)
(into () lista) ; funciona también para listas

;conj me devuelve la colección con el o los nuevos argumentos añadidos.
;si lo hago sin reduce entiende que el único argumento que le estoy pasando es el v
; y lo mete tal cual en la lista vacía. Si añado reduce antes, hace eso pero con cada
; elemento del vector
(reduce conj () v )
(conj () v)

; la siguiente solución no es elegante pero es un ejemplo de cómo usar loop y recur
; REVISAR!
((fn [s]
  (loop [result []
         s s]
    (if (seq s)
      (recur (concat [(first s)] result) (rest s))
      result))) k)

; función que devuelve una secuencia sin los números pares a partir de una colección
; Si queremos iterar por los elementos de una colección --> for!!!!
; podemos usar :when para que haga algo sólo cuando se cumpla esa condición
(defn odd [x] (for [a x :when (odd? a)] a))
(odd v)

;hacen lo mismo que la de arriba. Usando remove quitamos los elementos que cumplen
; la condición de ser par
(remove even? v)
(remove even? s)

;otras opciones. Usando filter, filtramos los elementos que son impares
(filter odd? v)



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

; con reduce

(defn r [& c]
   (reduce #((first %2) %1 (second %2)) (first c) (partition 2 (rest c))))

(r 1 * 2 + 3 * 4)

; #120 Sum of square of digits *********************************************************************

(= 8 (__ (range 10)))

(range 20)


(defn _sumsqu [x]

(apply + (map #(Math/pow % 2) (map read-string (map str (into [] (str 15))))))

)



(defn binary-to-decimal [x] (int (reduce +
                                    (map #(* % (Math/pow 2 %2))
                                         (->> x (map str) (map #(Integer/parseInt %)) reverse)
                                         (range(count x))))))

(binary-to-decimal "10010101")

;no entiendo cómo funciona esta, debe ser una propiedad de read-string
#(read-string (str "2r" %))

(#(read-string (str "2r" %)) "10010101")
(str "2r" "10010101")
(read-string "1001")

(fn [s]
  (reduce + 0
    (map-indexed (fn [i x]
      (if (= \1 x)
        (Math/pow 2 i)
        0))
      (reverse s))))



