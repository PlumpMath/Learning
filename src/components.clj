(ns Learning.components
  (:require [com.stuartsierra.component :as component]))

(defrecord DB [conn host])

(map->DB {:host "http://mihost.com"})

