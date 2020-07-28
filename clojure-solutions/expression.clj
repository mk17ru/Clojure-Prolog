(def constant constantly)
(defn variable [name] (fn [args] (get args name)))
(defn abstract-operation [oper]
  (fn [& args]
    (fn [elems] (apply oper (mapv (fn [arg] (arg elems)) args)))))

(defn div
  ([a] (/ 1 (double a)))
  ([a & bs] (reduce (fn [a b] (/ (double a) (double b))) a bs)))
(defn aver
  [& args] (/ (apply + args) (count args)))
(defn pow [x y] (Math/pow x y))

(def add (abstract-operation +))
(defn neg [x] (- x))
(def subtract (abstract-operation -))
(def multiply (abstract-operation *))
(def divide (abstract-operation div))
(def negate (abstract-operation -))
(def avg (abstract-operation aver))
(def med (abstract-operation (fn [& args] (nth (sort args) (quot (count args) 2)))))



(def func-operations {'+ add '- subtract '* multiply '/ divide 'negate negate 'med med 'avg avg})

(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)))

(defn proto-call [this key & args] (apply (proto-get this key) this args))

(defn field [key] (fn [this] (proto-get this key)))
(defn method [key] (fn [this & args] (apply proto-call this key args)))
(def evaluate (method :evaluate))
(def _eval-impl (method :eval-impl))
(def toString (method :toString))
(def toStringInfix (method :toStringInfix))
(def diff (method :diff))

(defn constructor [prototype ctor]
  (fn [& args]
    (apply ctor {:prototype prototype} args)))

(declare TWO)
(declare ONE)
(declare ZERO)

(def Constant-proto
  (let [val (field :value)]
    {
     :toString      (fn [this] (format "%.1f" (double (val this))))
     :toStringInfix toString
     :evaluate      (fn [this _] (val this))
     :diff          (fn [_ _] ZERO)}))

(def Constant (constructor Constant-proto (fn [this value] (assoc this :value value))))

(def TWO (Constant 2))
(def ONE (Constant 1))
(def ZERO (Constant 0))

(def Variable-proto
  (let [_name (field :name)]
    {:evaluate      (fn [this args] (args (_name this)))
     :toString      (fn [this] (_name this))
     :toStringInfix toString
     :diff          (fn [this arg] (if (= arg (_name this)) ONE ZERO))
     }))

(def Variable (constructor Variable-proto (fn [this name] (assoc this :name name))))

(declare Add)

(def Operation-proto
  (let [_op (field :oper)
        _diffC (field :diffC)
        _name (field :name)
        _operands (field :operands)]
    {:toString      (fn [this] (str "(" (_name this) " " (clojure.string/join " " (mapv toString (_operands this))) ")"))
     :toStringInfix (fn [this] (if (= 1 (count (_operands this))) (str (_name this) "(" (toStringInfix (first (_operands this))) ")")
                                          (str "(" (toStringInfix (first (_operands this))) (str " " (_name this)) " "
                                               (clojure.string/join (str " " (_name this) " ") (map toStringInfix (rest (_operands this)))) ")")))

     :evaluate      (fn [this vals] (apply (_op this) (map #(evaluate % vals) (_operands this))))
     :diff          (fn [this variable]
                      ((_diffC this) (_operands this) (mapv #(diff % variable) (_operands this))))
     }))

(declare Multiply)
(declare Subtract)
(defn get-diff [name x] (diff x name))

(defn diff-sum [_ dxs] (apply Add dxs))

(defn diff-sub [_ dxs] (apply Subtract dxs))

(defn diff-multiply [os dos] (last (reduce (fn [[a da] [b db]]
                                                         [(Multiply a b) (Add (Multiply a db) (Multiply b da))])
                                                       (apply map vector [os dos]))))

(defn create-operation [op type diffC]
  (let [new-operation {:prototype Operation-proto
                       :oper      op
                       :diffC     diffC
                       :name      type}]
    (fn [& operands]
      {:prototype new-operation
       :operands  operands})))
; ren
(defn operation-bit [oper]
  (fn [& args]
    (Double/longBitsToDouble (apply oper (mapv #(Double/doubleToLongBits %) args)))))

(def Add (create-operation + "+" diff-sum))
(def Subtract (create-operation - "-" diff-sub))
(def Multiply (create-operation * "*" diff-multiply))
(def Negate (create-operation neg "negate" diff-sub))
(def Divide (create-operation div "/" (fn [[x & xs] [dx & dxs]]
                                        (let [dfm (diff-multiply xs dxs) fm (apply Multiply xs)] (Divide (Subtract
                                                                                         (Multiply dx fm) (Multiply x dfm)) (Multiply fm fm))))))
(def Sum (create-operation + "sum" diff-sum))
(def Avg (create-operation aver "avg" (fn [xs dxs] (Divide (apply Add dxs) (Constant (count xs))))))

(def And (create-operation (operation-bit bit-and) "&" nil))
(def Xor (create-operation (operation-bit bit-xor) "^" nil))
(def Or (create-operation (operation-bit bit-or) "|" nil))

(def Pow (create-operation pow "**" nil))

(def Log (create-operation  (fn [x y] (/ (Math/log (Math/abs y)) (Math/log (Math/abs x)))) "//" nil))

(def obj-operations {
                     '+            Add
                     '-            Subtract
                     '*            Multiply
                     '/            Divide
                     'negate       Negate
                     'sum          Sum
                     'avg          Avg
                     '&            And
                     '|            Or
                     (symbol "^")  Xor
                     (symbol "//") Log
                     (symbol "**") Pow
                     })


(defn common-parser [constant variable operations]
  (fn [input]
    (letfn [(parse [expr]
              (cond
                (seq? expr) (apply (get operations (first expr)) (mapv parse (rest expr)))
                (number? expr) (constant expr)
                :else (variable (str expr))))]
      (parse (read-string input)))))

(def parseFunction (common-parser constant variable func-operations))

(def parseObject (common-parser Constant Variable obj-operations))

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
                       "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" input (_show (parser input)))) inputs))

(defn _empty [value] (partial -return value))

(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))

(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
         ((force b) (-tail ar)))))))

(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn +char [chars] (_char (set chars)))

(defn +char-not [chars] (_char (comp not (set chars))))

(defn +map [f parser] (comp (_map f) parser))

(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce (partial _either) p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(def +parser _parser)

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

(defn +word [ss] (apply +seqf str (map (comp +char str) ss)))

(def spaces " \t\n\r")

(defn *operation [map-oper] (+map
               (comp map-oper symbol)
               (+str (apply +or (map (comp +word str) (keys map-oper))))))

(def *space (+char spaces))

(def *ws (+ignore (+star *space)))

(def *digit (+char "1234567890"))

(def *number (+map read-string (+str (+map flatten (+seq (+opt (+char "-"))
                                                         (+plus *digit) (+opt (+seqf cons (+char ".") (+plus *digit))))))))

(def *constant (+map Constant (+seqn 0 *ws *number)))

(def *all-chars (mapv char (range 32 128)))

(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))

;(defn *identifier [id real] (+map symbol (apply +seqf (constantly real) (map (fn [x] (+char x)) (clojure.string/split id #"")))))

;(def unary-syms (*identifier "negate" "negate"))

(def *variable (+map (comp Variable str) (+char "xyz")))

(declare *unary-oper)
(declare *lvl-sub-add)
(def *parse-brackets (+seqn 1 (+char "(") *ws (delay *lvl-sub-add) *ws (+char ")")))
(def *unary (delay (+or *unary-oper *variable *constant *parse-brackets)))

(def *unary-oper (+map (fn [[oper x]] (oper x)) (+seq (*operation {'negate Negate}) *ws *unary)))

(defn oper-obj [left]
  (fn [args] (let [nas (if left args (reverse args))]
                (reduce (fn [x [oper y]] (if left (oper x y) (oper y x))) (first nas) (partition 2 (rest nas))))))

(defn lvl-oper [l opers left]
  (+map (oper-obj left)
        (+seqf cons *ws l (+map (partial apply concat)
                             (+star (+seq *ws (*operation opers) *ws l))) *ws)))

(def *lvl-log-pow (lvl-oper *unary {(symbol "//") Log (symbol "**") Pow} false))
(def *lvl-div-mul (lvl-oper *lvl-log-pow {'/ Divide '* Multiply} true))
(def *lvl-sub-add (lvl-oper *lvl-div-mul {'- Subtract '+ Add} true))

(def parseObjectInfix
  (+parser (+seqn 0 *ws *lvl-sub-add *ws)))

;(println (toStringInfix (parseObjectInfix "x + y + x")))
;(println (toStringInfix (parseObjectInfix "x ** y + x")))

;(println (toStringInfix (parseObjectInfix "x // y + x")))
;(println (toStringInfix (parseObjectInfix "x // y ** x")))