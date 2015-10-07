(ns Learning.A_bit_of_theory
  (:require [swiss.arrows :refer :all])
  )


;; ---------------------------------------------
;; The Weird and Wonderful Characters of Clojure
;; ---------------------------------------------

;; En: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/?utm_content=buffer88032&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

;; # -> The Dispatch macro
;; # después de un symbol sirve para generar automáticamente un nuevo symbol

;; # También se ve en los tagged literals como: #inst, #js

(java.util.Date.)

;; #{ Set macro

;; #_ Discard macro. La form que vaya detrás no se tiene en cuenta

;; #" Regular expression macro

;; #( Function macro
;; % not a macro. placeholder for use in the #( macro.

;; #' Var macro

;; @ Deref macro. Shorthand equivalent of the deref function

;; ^ Metadata. Metadata is a map of values. ^ es equivalente a with-meta
(def ^{ :debug true } five 5)
(meta #'five)

;; Cuando se trata de añadir un solo value cuyo valor es true, es puede hacer así
(def ^:debug six 6)
(meta #'six)

;; Another use of ^ is for type hints. These are used to tell the compiler what type the value
;; will be and allow it to perform type specific optimisations thus potentially making resultant code a bit faster.
(def ^Integer five 5)
(meta #'five)

;; ^Integer ha añadido la key :tag al mapa de metadata.
;; También se puede hacer shorthand
(def ^Integer ^:private seven 7)
(meta #'seven)

;;There are several metadata keys that have special interpretation:
;;  :private A boolean indicating the access control for the var.
;;  :doc A string containing short (1-3 line) documentation for the var contents
;;  :test A fn of no args that uses assert to check various operations.
;;  :tag A symbol naming a class or a Class object that indicates the Java type of the object
;;       in the var, or its return value if the object is a fn.

;; ' Quote macro. To quote forms and prevent their evaluation. = quote
;; : Keyword. Indicator for a Keyword: interned string that provides fast comparison and lower memory overhead.
(keyword "hola")

;; :: Qualified keyword. To fully qualify a keyword with the current namespace.

;; $ Inner class reference
;; BaseXClient$EventNotifier. EventNotifier is an inner interface of the BaseXClient class.

;;-> ->> some-> cond-> as-> etc. - Threading macros

;; --------
;; -> y ->>
;; --------

(-> 25 Math/sqrt int str Integer.) ;; En una línea
(-> 25                             ;; O multilínea. En ambos casos las funciones pueden ir
    (Math/sqrt)                    ;; con paréntesis o sin ellos. No depende de si es una línea
    (int)                          ;; o multilínea, sino de la propia funcion
    (str)
    (Integer.))

;; -> Inserts x as the second item in the first form and so on
;; Las comas (espacios en blanco en clojure) son para explicitar donde se inserta el
;; valor o el resultado de la anterior forma
(-> 10
    (+ ,,, 20)
    (/ ,,, 2)
    (- ,,, 5))

;; expands to
(- (/ (+ 10 20) 2) 5)

;; ->> Inserts x as the last item in the first form and so on
(->> 10
    (+ 20 ,,,)
    (/ 2 ,,,)
    (- 5 ,,,))

;; expands to
(- 5 (/ 2 (+ 20 10)))

(-> "a b c d"
    .toUpperCase
    (.replace "A" "X")
    (.split " ")
    first)

;; expands to
(first (.split (.replace (.toUpperCase "a b c d")  "A" "X" ) " "))

;; Dentro de clojure.core hay más arrow forms; some->, cond->

(some #{2} [1 2 3 2 4])
(some #{5} [1 2 3 2 4])
(-> {:a 1} :a inc)
(-> {:a 1} :b inc) ;; throw an exception
(some-> {:a 1} :b inc)

;; Hay también toda una librería sobre arrows: swiss.arrows
(-<> 0 [<> 1 2 3])

;; ~ Unquote macro.
(reduce + '(1 2))
(reduce + '(~1 2)) ;; throw an exception
;; En esto se basa la construcción de macros wich are functions that returns
;; blocks of syntax with parts evaluated in varying contexts.

;; ----------------------------
;; Protocols, records and types
;; ----------------------------


;; Universal Design Pattern (UDP) Steve Yegge
;; Basado en la idea de Hofstadter "There is generality in the specific"
;; UDP se construye sobre la idea de un objeto con forma de mapa
;; Cada mapa se crea en base a un mapa prototipo usado como "padre" del cual se
;; heredan campos

;; Se podrían usar simples mapas como Data Objects, pero no serían un type específico
;; Para ello Clojure tiene "defrecord"

;; ******defrecord*******
;; crea una nueva clase de Java con un constructor que toma un valor por
;; cada uno de los campos listados

(defrecord Persona [nombre apellido edad direccion])
(defrecord Direccion [calle numero])

;; Para crear una nueva instancia:

(def Espe (Persona. "Esperanza" "Moreno" 39 (Direccion. "Amapola" 3)))
Espe

(:nombre Espe)
(:calle (:direccion Espe))
(-> Espe :direccion :calle)

;; Record types are maps and implement everything maps should: assoc, dissoc, get...

(assoc Espe :apellido "Moreno Cruz")
(update-in Espe [:edad] inc)

;; Nota: Aplicando dissoc, el objeto resultante no es un record, sino un mapa
(dissoc Espe :edad)

;; Aclaración: defrecord vs deftypes
;; deftypes es parecido a defrecord pero a un nivel más bajo que no implementa nada
;; y por lo tanto para usar por ejemplo assoc, get... tendríamos que implementarlo
;; nosotros explícitamente.
;; Otra diferencia es que permite crear clases con campos volátiles y mutables, lo cual
;; es muy complicado y nada recomendable

;; defrecord, demás de un vector con los distintos campos, admite protocolos

;; ******defprotocol*******
;; Es la función usada para crear protocolos. Un protocolo es un set de funciones,
;; cada una de ellas con almenos un parámetro. (Como una interfaz en Java)
;; Cualquier clase que quiera implementar ese protocolo tiene que proveer una implementacióm
;; específica para cada una de las funciones del protocolo


;; Primero definimos un protocolo

(defprotocol Datos-personales
  (quien-es [_])
  (años [_])
  (donde-vive [_]))



(defrecord Usuario [nombre apellido edad direccion]
  Datos-personales
  (quien-es [_] (str "Me llamo " nombre " " apellido))
  (años [_] (str "Tengo " edad " años"))
  (donde-vive [_] (str "Vivo en la calle " direccion)))

(def pepe (Usuario. "Jose" "García" 40 "Feria"))
(quien-es pepe)
(años pepe)
(donde-vive pepe)

;;+++++++++++++++++++++++++++++++++++++++++++++

(defprotocol Fun-Time
  (drinky-drinky [_]))

(defrecord Someone [nick-name preffered-drink]
  Fun-Time
  (drinky-drinky [_] (str nick-name "(having " preffered-drink "): uuumm")))

(def dude (->Someone "belun" "daiquiri"))

(def tio (Someone. "pepe" "gintonic"))

 (drinky-drinky dude)
 (drinky-drinky tio)

;;+++++++++++++++++++++++++++++++++++++++++++++
;; Definición de un protocolo
(defprotocol Foo
  (bar [this])
  (baz [this st])
  (quux [this x y]))


;; Creación de un nuevo record

(defrecord FooRecord []
    Foo
    (bar [this] (str "hola"))
    (baz [this saludo] (str "hola" saludo))
    (quux [this x y] (str this " " (* x y))))


;; Aplicación
(def hola (FooRecord.))

(bar hola)
(baz hola "-¿qué tal?")
(quux hola 8 9)


;; +++++++++ Ejemplo basado en thin.ng geom +++++++++++

(defprotocol Area
  (area [_])
  (half-area [_]))

(defprotocol Perimetro
  (perimetro [_]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++
(defrecord Cuadrado [points])

(defn crea-cuadrado
  "Crea una entidad cuadrado a partir de las coordenadas de su esquina
  superior izquierda y la dimensión de su lado"
  ([x y l] (Cuadrado. [(vector x y)(vector x (+ y l))(vector (+ x l) (+ y l))(vector (+ x l) y)])))


(extend-type Cuadrado
  Area
  (area [_] (let [[[_ a][_ b][_ _][_ _]] (:points _)
                  l (- b a)]
              (* l l)))
  (half-area [_] (/ (area _) 2))

  Perimetro
  (perimetro [_] (let [[[_ a][_ b][_ _][_ _]] (:points _)
                       l (- b a)]
                (* l 4))))

(def micuadrado (crea-cuadrado 10 10 10))

(area micuadrado)

(half-area)

(perimetro micuadrado)

;;++++++++++++++++++++++++++++++++++++++++++++++++++

(defrecord Rectangulo [points])

(defn crea-rectangulo
  "Crea una entidad rectangulo a partir de las coordenadas de su esquina
  superior izquierda, su anchura y su altura"
  ([x y w h] (Rectangulo. [(vector x y)(vector x (+ y h))(vector (+ x w) (+ y h))(vector (+ x w) y)])))


(extend-type Rectangulo
  Area
  (area [_] (let [[[_ a][c b][d _][_ _]] (:points _)
                  h (- b a)
                  w (- d c)]
              (* h w)))
  Perimetro
  (perimetro [_] (let [[[_ a][c b][d _][_ _]] (:points _)
                       h (- b a)
                       w (- d c)]
                (+ (* h 2) (* w 2)))))


(def mirectangulo (crea-rectangulo 10 10 100 50))
(area mirectangulo)
(perimetro mirectangulo)


;; ******reify*******

;; Explicado a la ligera. Es un macro parecido a defrecord, pero en vez de crear una clase de
;; objetos que pueden ser instanciada repetidamente, crea un único objeto que implementa el
;; protocolo

(defprotocol Poo
  (aaa [this])
  (bbb [this st]))

(def athing
  (reify Poo
    (aaa [_] (str _))
    (bbb [_ st] (str st))))

(aaa athing)
(bbb athing "hola")

;; Podemos usar athing mientras nos sea útil y luego mandarlo al Garbage Collection (GC)








