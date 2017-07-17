
(ns my-peg-game.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn take-input []
  "Accept user input"
  (println "Enter a input")
  (let [inp (read-line)]
    (println inp)))

(defn make-map []
  "Static map of length 5"
  {1  {:pegged true, :connections {6 3, 4 2}},
   2  {:pegged true, :connections {9 5, 7 4}},
   3  {:pegged true, :connections {10 6, 8 5}},
   4  {:pegged true, :connections {13 8, 11 7, 6 5, 1 2}},
   5  {:pegged true, :connections {14 9, 12 8}},
   6  {:pegged true, :connections {15 10, 13 9, 4 5, 1 3}},
   7  {:pegged true, :connections {9 8, 2 4}},
   8  {:pegged true, :connections {10 9, 3 5}},
   9  {:pegged true, :connections {7 8, 2 5}},
   10 {:pegged true, :connections {8 9, 3 6}},
   11 {:pegged true, :connections {13 12, 4 7}},
   12 {:pegged true, :connections {14 13, 5 8}},
   13 {:pegged true, :connections {15 14, 11 12, 6 9, 4 8}},
   14 {:pegged true, :connections {12 13, 5 9}},
   15 {:pegged true, :connections {13 14, 6 10}}})

(defn get-peg-value [inp map out]
  "Returns 1 if pegged else returns - for the given input"
  (if (empty? inp)
    out
    (recur (rest inp) map (conj out (if (= (get-in map [(first inp) :pegged]))
                                      1
                                      -)))))


(defn get-valid-pegs [inp]
  "Gets the filled pegs"

  (keys  (filter (fn [[k v]] (:pegged v)) inp)))

(defn convert-to-alph
  "Coverts filled pegs numbers to their respective alphabets"
  ([inp]
   (convert-to-alph inp [] ))
  ([inp out]
   (if (empty? inp)
     out
     (recur (rest inp) (conj out (char (+ 96 (first inp))))))))

(defn convert-to-num
  "Converts user-accepted alphabet to respective number"
  ([inp]
   (convert-to-num inp []))
  ([inp out]
   (if (empty? inp)
     (filter pos? out)
     (recur (rest inp) (conj out (- (int (first inp)) 96))))))
(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn get-element [inp]
  "Gets the elements on the respective level"

  (let [drop-value (nth (conj (take 5 tri) 0) (dec inp))]
    (take inp (drop drop-value (range 1 16)))))

(defn convert-zip-format
  "Converts to vector of strings"
  ([inp]
   (convert-zip-format inp []))
  ([inp out]
   (if (empty? inp)
     out
     (recur (rest inp) (conj out (str (first inp)))))))

(defn comb-element-peg [inp map]
  (let [a (get-element inp)
        b (get-peg-value a map [])]
    (zipmap (convert-zip-format (convert-to-alph  a)) (convert-zip-format b))))

(defn print-map [map]
  (loop [m map
         cl 1
         ce 1
         ct 4]
    (when (<= cl 5)
      (println (str (apply str (repeat ct "\t")) (comb-element-peg ce map)))
      (recur m (inc cl) (inc ce) (dec ct) ))))
