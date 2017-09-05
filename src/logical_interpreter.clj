(ns logical-interpreter)

(defn esDefinicion
  [x]
  true
)

(defn esRegla
  [x]
  false
)

(defn filtrarDefinicion
  [x]
  (def string2 (clojure.string/split x #"\)."))
  (def string3 (clojure.string/join " " string2))

  ;;Obtengo un vector con dos posiciones, la primera es el nombre de la definicion y la segunda los argumentos
  (def vector1 (clojure.string/split string3 #"\("))
  ;(println (get vector1 0))
  ;(println (get vector1 1))
  
  ;Chequeo si la linea esta mal formada
  (cond
    (== (count vector1) 1) "Mal formada"
    :else   vector1)  
)

(defn filtrarLinea
  [x]
  ;(println "Split (")
  
  (cond
    (esDefinicion x) (filtrarDefinicion x)
    ;(esRegla x) "regla"
    :else   "Mal")
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  nil)

(defn Example []

  ;Obtengo cada linea del archivo como una cadena.
   (with-open [rdr (clojure.java.io/reader "Example.txt")]
   (def lineasArchivo (reduce conj [] (line-seq rdr))))
  (println lineasArchivo)
   (println "Resultado map")
  (def listaSentencias (map filtrarLinea lineasArchivo))
  (println listaSentencias)

)
