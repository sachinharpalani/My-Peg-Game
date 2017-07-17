
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

(defn make-board []
  "Static map of length 5"
  {1  {:pegged false, :connections {6 3, 4 2}},
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

(defn get-peg-value [inp board out]
  "Returns 1 if pegged else returns - for the given input"
  (if (empty? inp)
    out
    (recur (rest inp) board (conj out (if (= (get-in board [(first inp) :pegged]) true)
                                      1
                                      0)))))


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

#_(defn convert-zip-format-1
  "Converts to vector of strings EG: '(4 5 6) -> [\"4\" \"5\" \"6\"] "
  ([inp]
   (convert-zip-format inp []))
  ([inp out]
   (if (empty? inp)
     out
     (recur (rest inp) (conj out (str (first inp)))))))

(defn convert-zip-format [inp]
  (vec inp))

(defn comb-element-peg [inp board]
  ""
  (let [a (get-element inp)
        b (get-peg-value a board [])]
    (zipmap (convert-zip-format (convert-to-alph  a)) (convert-zip-format b))))

(defn print-map [board]
  "Prints the board"
  (loop [cl 1
         ce 1
         ct 4]
    (when (<= cl 5)
      (println (str (apply str (repeat ct "\t")) (comb-element-peg ce board)))
      (recur (inc cl) (inc ce) (dec ct) ))))


;; RADHA CODE

(defn char-as-str [x] (filter (comp not empty?) (clojure.string/split x #" ")))

(defn convert-int [str] (- (int (let [gb (.getBytes str)] (char (first gb)))) 96))

(take-input (make-map))                         ;taking move
;(char-as-str "a   b");is peg

(defn peg? [board pos] (if (get-in board [pos :pegged])
                        true
                        false));valid move;remove peg

(defn remove-peg [board pos]
 (assoc-in board
           [pos :pegged]
           false))
                                        ;add peg

(defn add-peg [board pos]
 (assoc-in board
           [pos :pegged]
           true))
                                        ;move peg

(defn neigh-peg [board pos dest]
  (second (first (filter #(= dest (first %))
                         (get-in board
                                 [pos :connections])))))


(defn move-peg [board pos dest]
 (let [neigh (neigh-peg board pos dest)]
   ;(println neigh)
   (add-peg (remove-peg (remove-peg board
                                neigh)
                    pos)
            dest)))

                                        ;is valid

(defn valid-move? [board pos dest]
 (let [neigh (neigh-peg board pos dest)]
   (and (peg? board pos)
        (peg? board neigh)
        (not (peg? board dest)))))

(defn take-input [board]
  (println "enter the source and destination points")
  (let [lis (map convert-int  (char-as-str (read-line)))
        fir (first lis)
        sec (second lis)]
    (if (valid-move? board fir sec)
      (move-peg board fir sec)
      (do println "invalid move "
          board))))


;;HItesh COde
(comment
  (defn checkres [board]
    (let [truemap (filter #(true? (:pegged (second %))) (vec board))]
      (if (= 1 (count truemap))
        (println "WINNER")

        (if (movava truemap)
          (println "U LOST")
          #_(println "U LOST"))) ))

  (defn ispeg [board pos]
    (:pegged (second (first (filter #(= pos (first %)) board)))))

  (defn movava [board]
    #_(let [newboard (filter #(true? (:pegged (second %))) (vec board))])
    (and
     (false? (ispeg board (first (first (:connections (second (first board)))))))
     (true? (ispeg board (second (first (:connections (second (first board))))))))))
