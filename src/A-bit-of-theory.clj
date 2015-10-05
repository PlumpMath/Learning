(ns Learning.A_bit_of_theory
  )




;; --------
;; -> y ->>
;; --------


;; -> Inserts x as the second item in the first form and so on
(-> 10
    (+ 20)
    (/ 2)
    (- 5))

;; expands to
(- (/ (+ 10 20) 2) 5)

;; ->> Inserts x as the last item in the first form and so on
(->> 10
    (+ 20)
    (/ 2)
    (- 5))

;; expands to
(- 5 (/ 2 (+ 20 10)))

(-> "a b c d"
    .toUpperCase
    (.replace "A" "X")
    (.split " ")
    first)

;; expands to
(first (.split (.replace (.toUpperCase "a b c d")  "A" "X" ) " "))




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


;; +++++++++ Ejemplo thin.ng geom +++++++++++



(defn triangle2

  ([a b c] (Triangle2. [(vec a) (vec b) (vec c)])))


(extend-type Triangle2
  PArea
  (area [_] (apply (fn tri-area2 [a b c] (* 0.5 (norm-sign2 a b c))) (:points _)))

  PBoundary
  (contains-point?
   [_ p] (apply gu/point-in-triangle2? p (:points _)))

  )


(defprotocol Area
  (area [_]))

(defprotocol Altura
  (altura [_]))

(defrecord Cuadrado [points])

(defn crea-cuadrado
  "Crea una entidad cuadrado a partir de las coordenadas de su esquina
  superior izquierda y la dimensión de su lado"
  ([x y l] (Cuadrado. [(vector x y)(vector x (+ y l))(vector (+ x l) (+ y l))(vector (+ x l) y)])))

(extend-type Cuadrado
  Area
  (area [_] ((Math/pow (- (-> (:points _) first second)(-> (:points _) second second)) 2))))

(def micuadrado (crea-cuadrado 10 10 10))

(area micuadrado)















