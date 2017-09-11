(ns logical-interpreter)

(defn esDefinicion
  "Devuelve true si un string es una definicion"
  [x]
  (def string1 (clojure.string/split x #"\:-"))
  
  (cond
    (= (count string1) 1) true
    :else false) 
)

(defn esRegla
  "Devuelve true si un string es una regla"
  [x]
  (def string1 (clojure.string/split x #"\:-"))
  (cond
    (= (count string1) 1) false
    :else true)
)

(defn filtrarDefinicion
  "Filtra un string y devuelve un formato de definicion interpretable"
  [x]
  (def string2 (clojure.string/split x #"\)"))
  (def string3 (clojure.string/join " " string2))

  ;;Obtengo un vector con dos posiciones, la primera es el nombre de la definicion y la segunda los argumentos
  (def vector1 (clojure.string/split string3 #"\("))
  
  ;Chequeo si la linea esta mal formada
  (cond
    (== (count vector1) 1) nil
    :else   vector1)  
)

(defn filtrarRegla
  "Filtra un string y devuelve un formato de regla interpretable"
  [x]
  (def string2 (clojure.string/split x #"\:-"))
  (println string2)
)

(defn existeDefinicionAux
  [mapaDefiniciones clave valor] 
  (def valores (get mapaDefiniciones clave))
  (def lista1 (map (fn [x] (= x valor)) valores))
  
  (def result (some true? lista1))

  (cond
    (= result nil) false
    (= result false) false
    :else true)
)

(defn existeDefinicion
  "Devuelve true si la definicion consultada existe en la base de datos"
  [mapaDefiniciones clave valor]
 (cond
   (= (contains? mapaDefiniciones clave) true) (existeDefinicionAux mapaDefiniciones clave valor)
   :else false)
)

(defn formarDefinicion
  "Recibe una definicion con parametros genericos y un mapa de parametros asociados a argumentos.
  Devuelve un string con los parametros a consultar en el formato adecuado."
  [argumentosGenericos mapaArg]
  (def cantidadArgu (count argumentosGenericos))
  (def stringaux (map (fn [x] (get mapaArg x)) argumentosGenericos))

  (def stringFormado (clojure.string/join ", " stringaux))
  (def primerElemento (first stringaux))
  
  (cond
    (= (count argumentosGenericos) 1) primerElemento 
    :else stringFormado)
)

(defn existeReglaAux
  [mapaReglas mapaDef clave valorConsulta]
  (def valores (get mapaReglas clave))
  (def parametros (get valores 0))
  (def definicionesAEvaluar (subvec valores 1))
  (def argumentos (clojure.string/split valorConsulta #"\, ") )
  
  ;;Creo un mapa para asociar parametros con los argumentos de la consulta.
  (def mapaArgumentos (apply merge (map (fn [x, y] (hash-map x y)) parametros argumentos)))
  
  ;Mapeo todas las definiciones que componen una regla para evaluar si esta ultima se cumple.
  (def resultDefiniciones (map (fn [x] (existeDefinicion mapaDef (get x 0) (formarDefinicion (subvec x 1) mapaArgumentos))) definicionesAEvaluar))
  (every? true? resultDefiniciones)
)

(defn existeRegla
  "Devuelve true si una regla existe en la base de datos."
  [mapaReglas mapaDef clave valorConsulta]

  (cond
   (= (contains? mapaReglas clave) true) (existeReglaAux mapaReglas mapaDef clave valorConsulta)
   :else false)
)

(defn extraerDefinicion
  [linea]
  (cond
    (= (esDefinicion linea) true) (filtrarDefinicion linea)
   :else nil)
)

(defn extraerReglas
  [linea]
  (cond
    (= (esRegla linea) true) (filtrarRegla linea)
   :else nil)
)

(defn evaluate-query2
  [database consulta]

  (def clave (get consulta 0))
  (def valor (get consulta 1)) 
  
  (def definiciones (hash-map "varon" ["juan" "pepe" "hector" "roberto" "alejandro"] "mujer" ["maria" "cecilia"] "padre" ["juan, pepe" "juan, pepa" "hector, maria" "roberto, alejandro" "roberto, cecilia"] "add" ["zero, zero, zero" "zero, one, one" "zero, two, two" "one, zero, one" "one, one, two" "one, two, zero" "two, zero, two" "two, one, zero" "two, two, one"]))
  (def reglas (hash-map "hijo" [["X" "Y"] ["varon" "X"] ["padre" "Y" "X"]] "hija" [["X" "Y"] ["mujer" "X"] ["padre" "Y" "X"]] "subtract" [["X" "Y" "Z"] ["add" "Y" "Z" "X"]]))
  
  
  ;Paso la base de datos a un formato interpretable.
  (def lineas (clojure.string/split database #"\."))
  
  ;Extraigo las lineas que son definiciones
  (def listaDefiniciones (map extraerDefinicion lineas))
  ;Remuevo los valores que no son definiciones.
  (def listaDefiniciones2 (remove nil? listaDefiniciones))
  ;Creo un hash-map de definiciones
  (def definicionesAux (apply merge (map (fn [x] (hash-map (get x 0) [(get x 1)])) listaDefiniciones2)))
 
  ;Extraigo las lineas que son reglas
  (def listaReglas (map extraerReglas lineas))
  ;Remuevo los valores que no son definiciones.
;  (println (remove nil? listaReglas)) 
  
    ;Primero busco si existe la consulta como definicion.
  (def resultadoDefinicion (existeDefinicion definiciones clave valor))
  
  (cond
    (= resultadoDefinicion true) true
    :else (existeRegla reglas definiciones clave valor))
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  
  ;Paso la consulta a un formato interpretable.
  (def consulta (filtrarDefinicion query))
  (cond
    (= consulta nil) nil
    :else (evaluate-query2 database consulta))
)
