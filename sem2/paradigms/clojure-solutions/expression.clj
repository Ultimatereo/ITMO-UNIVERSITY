(load-file "parser.clj")
(load-file "proto.clj")
;HW 10
(defn abstractBinary [op]
  (fn [& functions]
    (fn [args]
      (let
        [results (map (fn [expression] (expression args)) functions)]
        (apply op results)))))
(def subtract (abstractBinary -))
(def add (abstractBinary +))
(def multiply (abstractBinary *))
(defn divideImpl
  ([args] (if (== args 0.0) ##Inf (/ (double args))))
  ([x & args] (/ x (double (apply * args)))))
(def divide (abstractBinary divideImpl))
(def negate subtract)
(defn powImpl [a b] (Math/pow a b))
(def pow (abstractBinary powImpl))
(defn logImpl [a b] (/ (Math/log (Math/abs b)) (Math/log (Math/abs a))))
(def log (abstractBinary logImpl))
(defn constant [a] (constantly a))
(defn variable [name] (fn [map] (get map name)))
(def operations {'+ add '- subtract '* multiply '/ divide 'negate negate 'pow pow 'log log})
(defn parseImpl
  [operationImpl constantImpl variableImpl]
  (fn [string]
    (letfn [
            (parseFunction-iter [expression]
              (if (seq? expression)
                (apply (operationImpl (first expression))
                       (map parseFunction-iter (rest expression)))
                (if (number? expression) (constantImpl expression)
                                         (if (symbol? expression) (variableImpl (str expression)))))
              )]
      (parseFunction-iter (read-string string)))))
(def parseFunction (parseImpl operations constant variable))

;HW 11

(def evaluate (method :evaluate))
(def toString (method :toString))
(def toStringSuffix (method :toStringSuffix))
(def diff (method :diff))
(def _functions (field :functions))
(def _op (field :op))
(def _opString (field :opString))
(def _opDif (field :opDif))

(defn constructor
  "Defines constructor"
  [ctor prototype]
  (fn [& args] (apply ctor {:prototype prototype} args)))

(def AbstractOperationPrototype
  {:evaluate       (fn [this vars] (let
                                     [results (map (fn [expression] (evaluate expression vars)) (_functions this))]
                                     (apply (_op this) results)))
   :toStringSuffix (fn [this] (let
                                [results (mapv toStringSuffix (_functions this))]
                                (str "(" (clojure.string/join " " results)
                                     " " (_opString this)
                                     ")")))
   :toString       (fn [this] (let
                                [results (mapv toString (_functions this))]
                                (str "(" (_opString this) " "
                                     (clojure.string/join " " results)
                                     ")")))
   :diff           (fn [this variable] (let
                                         [results (map (fn [expression] (diff expression variable)) (_functions this))]
                                         ((_opDif this) (_functions this) results variable)))})
(defn abstractOpConstructor [this op opString opDif]
  (assoc this :op op :opString opString :opDif opDif))
(def AbstractOperation (constructor abstractOpConstructor AbstractOperationPrototype))
(defn createFunction [op opString opDif] (fn [& functions]
                                           (assoc (AbstractOperation op opString opDif) :functions functions)))
(def Add (createFunction + '+ (fn [_ results _] (apply Add results))))
(def Subtract (createFunction - '- (fn [_ results _] (apply Subtract results))))
(def Negate (createFunction - 'negate (fn [_ results _] (apply Negate results))))
(declare Multiply)
(declare Divide)
(declare Constant)
(defn multiplyDiffRule
  [fs dfs _]
  (second (reduce (fn [[f df] [g dg]]
                    [(Multiply f g)
                     (Add (Multiply f dg)
                          (Multiply df g))])
                  [(Constant 1) (Constant 0)]
                  (mapv vector fs dfs))))
(def Multiply (createFunction * '* multiplyDiffRule))
(defn divideDifRule [fs dfs _]
  (if (== (count fs) 1) (Negate (Divide (first dfs) (Multiply (first fs) (first fs))))
                        (second (reduce (fn [[f df] [g dg]]
                                          [(Divide f g)
                                           (Divide (Subtract (Multiply df g)
                                                             (Multiply f dg))
                                                   (Multiply g g))])
                                        (mapv vector fs dfs)))))
(def Divide (createFunction divideImpl '/ divideDifRule))
(declare Log)
(def ln (partial logImpl Math/E))
(def Ln (createFunction ln 'ln
                        (fn [fs dfs _] (Divide (first dfs) (first fs)))))
(defn logDifRule [fs dfs _]
  (let [g (first fs) f (second fs) dg (first dfs) df (second dfs)]
    (Divide (Subtract (Divide (Multiply df (Ln g)) f)
                      (Divide (Multiply dg (Ln f)) g))
            (Multiply (Ln g) (Ln g)))))
(def Log (createFunction logImpl 'log logDifRule))
(declare Pow)
(defn powDifRule [fs dfs _]
  (let [f (first fs) g (second fs) df (first dfs) dg (second dfs)]
    (Add (Multiply g (Pow f (Subtract g (Constant 1))) df)
         (Multiply (Pow f g) (Ln f) dg))))
(def Pow (createFunction powImpl 'pow powDifRule))
(def exp (partial powImpl Math/E))
(declare Exp)
(defn expDifRule [fs dfs _] (Multiply (first dfs) (Exp (first fs))))
(def Exp (createFunction exp 'exp
                         expDifRule))
(defn sumexp [& args] (apply + (map exp args)))
(defn sumexpDifRule [fs dfs _] (apply Add (mapv (fn [f df] (Multiply (Exp f) df)) fs dfs)))
(def Sumexp (createFunction sumexp
                            'sumexp
                            sumexpDifRule))
(def Softmax (createFunction (fn [& args] (/ (exp (first args)) (double (apply sumexp args))))
                             'softmax
                             (fn [fs dfs _] (let [derOfDenominator (sumexpDifRule fs dfs "")
                                                  derOfNumerator (expDifRule [(first fs)] [(first dfs)] "")]
                                              (divideDifRule [(Exp (first fs)) (apply Sumexp fs)] [derOfNumerator derOfDenominator] "")))))
(declare Mean)
(defn mean [& args] (/ (apply + args) (double (count args))))
(defn meanDifRule [_ dfs _] (apply Mean dfs))
(def Mean (createFunction mean 'mean meanDifRule))
(declare Varn)
(defn varn [& args]
  (let [mean2 (apply mean args)]
    (- (apply mean (mapv (fn [a] (* a a)) args))
       (* mean2 mean2))))
(defn varnDifRule [fs dfs var]
  (let
    [derOfMean (apply Mean dfs)
     justMean (apply Mean fs)
     mean2 (apply Mean (mapv (fn [f] (Multiply f f)) fs))
     derOfMean2 (diff mean2 var)]
    (Subtract derOfMean2
              (Multiply (Constant 2) derOfMean justMean))))
(def Varn (createFunction varn 'varn varnDifRule))
(def arg (field :arg))
(def ConstantPrototype
  {:evaluate       (fn [this _] (arg this))
   :toString       (fn [this] (format "%.1f" (double (arg this))))
   :toStringSuffix (fn [this] (format "%.1f" (double (arg this))))
   :diff           (fn [_ _] (Constant 0))
   })
(defn Constant [arg] (assoc ConstantPrototype :arg arg))
(def VariablePrototype
  {:evaluate       (fn [this vars] (let [arg' (str (Character/toLowerCase (get (arg this) 0)))] (double (vars arg'))))
   :toString       (fn [this] (str (arg this)))
   :toStringSuffix (fn [this] (str (arg this)))
   :diff           (fn [this variable]
                     (if (= variable (arg this))
                       (Constant 1) (Constant 0)))
   })
(defn Variable [arg] (assoc VariablePrototype :arg arg))
(defn dToL [a] (Double/doubleToLongBits a))
(defn lToD [a] (Double/longBitsToDouble a))
(defn createBitOperation [f] (fn [a b] (lToD (f (dToL a) (dToL b)))))
(def bitAnd (createBitOperation bit-and))
(def bitOr (createBitOperation bit-or))
(def bitXor (createBitOperation bit-xor))
(def BitAnd (createFunction bitAnd '& []))
(def BitOr (createFunction bitOr '| []))
(def BitXor (createFunction bitXor (symbol (str \^)) []))
(def Operations {'+    Add '- Subtract '* Multiply '/ Divide 'negate Negate
                 'pow  Pow 'log Log 'ln Ln 'exp Exp 'sumexp Sumexp 'softmax Softmax
                 'mean Mean 'varn Varn
                 '&    BitAnd '| BitOr (symbol (str \^)) BitXor})
(def parseObject (parseImpl Operations Constant Variable))

;HW 12
(def *all-chars (mapv char (range 0 128)))
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))
(def *digit (+char (apply str (filter #(Character/isDigit %) *all-chars))))
(def *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars))))
(def *ws (+ignore (+star *space)))
(def +ws (+ignore (+plus *space)))
(def *number (+map read-string (+str (+seq (+opt (+char "-"))
                                           (+str (+plus *digit))
                                           (+str (+opt (+seq (+char ".") (+str (+plus *digit)))))))))

(def *operation (+or (+char "&|+-*/") (+char (str \^))
                     (+str (+seq (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e")))
                     (+str (+seq (+char "e") (+char "x") (+char "p")))
                     (+str (+seq (+char "l") (+char "n")))
                     (+str (+seq (+char "p") (+char "o") (+char "w")))
                     (+str (+seq (+char "l") (+char "o") (+char "g")))
                     (+str (+seq (+char "s") (+char "u") (+char "m") (+char "e") (+char "x") (+char "p")))
                     (+str (+seq (+char "s") (+char "o") (+char "f") (+char "t") (+char "m") (+char "a") (+char "x")))
                     (+str (+seq (+char "m") (+char "e") (+char "a") (+char "n")))
                     (+str (+seq (+char "v") (+char "a") (+char "r") (+char "n")))
                     ))

(def *constant (+map Constant *number))
(def *variable (+map Variable (+str (+plus (+char "xyzXYZ")))))

(defn *seq [] (+seq *ws (+or *constant
                             *variable
                             (+seqf (fn [functions operation]
                                      (apply (Operations (symbol (str operation)))
                                             (map (fn [a] (first a)) functions)
                                             ))
                                    ;(+seqf (fn [functions operation] (str functions " " operation))
                                    (+ignore (+char "("))
                                    *ws
                                    (+plus (delay (*seq)))
                                    *ws
                                    *operation
                                    *ws
                                    (+ignore (+char ")"))
                                    )) *ws))
(defn parseObjectSuffix [expression] (first (-value ((*seq) (str expression)))))

