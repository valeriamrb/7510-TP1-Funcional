(ns logical-interpreter)

(defn esDefinicionValida
  ;Devuelve true si el string es una definicion con un formato valido.
  [linea]
  (let [partes (clojure.string/split linea #"\(|\)")]
  (= (count partes) 2))
)

(defn esReglaValida
  ;Devuelve true si el string es una regla con un formato valido.
  [linea]
  (let [partes (clojure.string/split linea #"\:-")]
  (= (count partes) 2))
)

(defn validarBase
  ;;Valida si la base de datos esta completa. Devuelve nil en caso negativo.
  [datos]
  (let [listaDefValidas (filter (fn [x] (esDefinicionValida x)) datos)
        listaReglasValidas (filter (fn [x] (esReglaValida x)) datos)
        cantidadDefValidas (count listaDefValidas)
        cantidadReglasValidas (count listaReglasValidas)
        cantidadLineasValidas (+ cantidadDefValidas cantidadReglasValidas)
        cantidadLineasTotales (count datos)
        ]

      (if (= cantidadLineasTotales cantidadLineasValidas)
          true
          nil)
        )
)

(defn filtrarDefinicion
  "Filtra un string y devuelve un formato de definicion interpretable"
  [x]
  (let [string2 (clojure.string/split x #"\)")
        string3 (clojure.string/join " " string2)

        ;;Obtengo un vector con dos posiciones, la primera es el nombre de la definicion y la segunda los argumentos
        vector1 (clojure.string/split string3 #"\(")
      ]
  ;Chequeo si la linea esta mal formada
  (cond
    (== (count vector1) 1) nil
    :else   vector1))
)

(defn filtrarRegla
  "Filtra un string de regla y devuelve un vector con formato de regla interpretable.
  Ej: [[hijo] [X Y] [varon X] [padre Y X]]"
  [string]
  (let [ string1 (clojure.string/replace string #"\s" "")
         partes (clojure.string/split string1 #"\:-")
         primerTermino (get partes 0)
         segundoTermino (get partes 1)
         aux1 (clojure.string/split primerTermino #"\(")
        
         aux2 (clojure.string/split (get aux1 1) #"\)|,")
         aux3 (clojure.string/split segundoTermino #"\),")
         partes2 (map (fn [x] (clojure.string/split x #"\(|\)|,")) aux3)

        ]
    (vec (concat [(get aux1 0) aux2] partes2))
    )
)

(defn extraerDefinicion
  "Recibe una linea y de ser posible extrae una definicion en formato interpretable"
  [linea]
  (cond
    (= (esDefinicionValida linea) true) (filtrarDefinicion linea)
   :else nil)
)

(defn extraerReglas
  "Verifica si una linea es una definicion en formato valido y la filtra en modo interpretable"
  [linea]
  (cond
    (= (esReglaValida linea) true) (filtrarRegla linea)
   :else nil)
)

(defn existeDefinicionAux
  [mapaDefiniciones clave valor] 
  (let [valores (get mapaDefiniciones clave)
        lista1 (map (fn [x] (= x valor)) valores)
        result (some true? lista1)]
  (cond
    (= result nil) false
    (= result false) false
    :else true))
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
  (let [cantidadArgu (count argumentosGenericos)
        stringaux (map (fn [x] (get mapaArg x)) argumentosGenericos)
        stringFormado (clojure.string/join ", " stringaux)
        primerElemento (first stringaux)]
  
  (cond
    (= (count argumentosGenericos) 1) primerElemento 
    :else stringFormado))
)

(defn existeReglaAux
  [mapaReglas mapaDef clave valorConsulta]
  (let [valores (get mapaReglas clave)
        parametros (get valores 0)
        definicionesAEvaluar (subvec valores 1)
        argumentos (clojure.string/split valorConsulta #"\, ")
  
        ;Creo un mapa para asociar parametros con los argumentos de la consulta.
        mapaArgumentos (apply merge (map (fn [x, y] (hash-map x y)) parametros argumentos))
  
        ;Mapeo todas las definiciones que componen una regla para evaluar si esta ultima se cumple.
        resultDefiniciones (map (fn [x] (existeDefinicion mapaDef (get x 0) (formarDefinicion (subvec x 1) mapaArgumentos))) definicionesAEvaluar)]
  
  (every? true? resultDefiniciones))
)

(defn existeRegla
  "Devuelve true si una regla existe en la base de datos."
  [mapaReglas mapaDef clave valorConsulta]
  (cond
   (= (contains? mapaReglas clave) true) (existeReglaAux mapaReglas mapaDef clave valorConsulta)
   :else false)
)

(defn procesarConsulta
  "Procesa una consulta. Devuelve true si existe."
  [listaLineas consulta]
  ;Separo el nombre de la consulta y sus argumentos.
  (let [clave (get consulta 0)
        valor (get consulta 1)
        
        ;Extraigo las lineas que son consultas.
        listaDefiniciones (map extraerDefinicion listaLineas)
        ;Remuevo los valores que no son definiciones.
        listaDefiniciones2 (remove nil? listaDefiniciones)
        
        ;Armo un hash-map de definiciones.
        definicionesAux (merge (map (fn [x] (hash-map (get x 0) (get x 1))) listaDefiniciones2))
        hashDefinicionesAux (reduce (fn [x pair] (let [[[k v]] (seq pair)]
                 (assoc x k (cons v (x k))))) 
  {} 
  definicionesAux)
        clavesHash (keys hashDefinicionesAux)
        definiciones (apply merge (map (fn [clave] (hash-map clave (reduce conj [] (get hashDefinicionesAux clave)))) clavesHash))

        ;Extraigo las lineas que son reglas
        listaReglas (remove nil? (map extraerReglas listaLineas))
        ;Armo un hash-map de definiciones.
        reglas (apply merge (map (fn [x] (hash-map (get x 0) (subvec x 1))) listaReglas))

        ;Busco si existe la consulta como definicion.
        resultadoDefinicion (existeDefinicion definiciones clave valor)]
        ;Si no existe la definicion, verifico que la consulta sea una regla.
        (cond
          (= resultadoDefinicion true) true
          :else (existeRegla reglas definiciones clave valor)))
)

(defn evaluate-query2
  [database consulta]
  ;Elimino tabulaciones y saltos de linea de la base de datos para procesarla.
  (let [lineas (clojure.string/replace database #"\t|\n|\.$" "")
  
      ;Divido las lineas a partir del ultimo punto de cada una.
        lineas2(clojure.string/split lineas #"\.")]
  
  ;Valido que la base de datos sea valida
  (if (= (validarBase lineas2) nil)
      nil
      (procesarConsulta lineas2 consulta)))
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  
  ;Paso la consulta a un formato interpretable.
  (let [consulta (filtrarDefinicion query)]
  ;Si la consulta esta bien formada la proceso.
  (cond
    (= consulta nil) nil
    :else (evaluate-query2 database consulta)))
)
