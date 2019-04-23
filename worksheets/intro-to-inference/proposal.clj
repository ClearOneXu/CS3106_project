(ns arithmetic-functions-induction
   (:require [gorilla-plot.core :as plot]
             [clojure.java.io :as io]
             [clojure.repl :as repl]
             [anglican.runtime :refer [sample* observe* flip uniform-discrete discrete normal log exp abs]]))

(def log-prob observe*)
(defn safe-div [x y] (if (zero? y) 0 (/ x y)))
(defn get-int-constant [] 
  (sample* (uniform-discrete 0 10)))
(def operations ['+ '- '* 'safe-div])
(defn sample-operation [] 
  (get operations (sample* (uniform-discrete 0 (count operations)))))
(defn sample-symbol []
  (if (sample* (flip 0.6)) (get-int-constant) 'x))
(defn sample-simple []
  (list (sample-operation) (sample-symbol) (sample-symbol)))
(defn make-fn [body-expr]
  (list 'fn ['x] (:value body-expr)))
(defn uniform-propose-symbol []
  (let [i (sample* (uniform-discrete 0 11))]
    (if (= i 10) 'x i)))
(defn sample-expr []
  (let [expression-type (sample* (discrete [0.3 0.4 0.3]))]
    (cond (= expression-type 0) {:value 'x :length 1}
          (= expression-type 1) {:value (get-int-constant) :length 1}
          :else 
          (let [operation (get operations (sample* (uniform-discrete 0 (count operations))))
                left (sample-expr)
                right (sample-expr)
                left-expr (:value left)
                right-expr (:value right)
                left-length (if (list? left-expr) (first (:length left)) 1)
                right-length (if (list? right-expr) (first (:length right)) 1)
                length (+ 1 left-length right-length)]
            {:value (list operation left-expr right-expr) :length (list length (:length left) (:length right))}))))

(defn sample-function []
  (let[left (sample-expr)
       right (sample-expr)
       left-expr (:value left)
       right-expr (:value right)
       left-length (if (list? left-expr) (first (:length left)) 1)
       right-length (if (list? right-expr) (first (:length right)) 1)
       length (+ 1 left-length right-length)] 
  {:value (list (sample-operation) left-expr right-expr) :length (list length (:length left) (:length right))}))


(defn get-sigmas [values]
  (mapv (fn [v] (max 0.001 (abs (* 0.05 v)))) values))

(defn sample-likelihood [inputs]
  (mapv (fn [mu sigma] (sample* (normal mu sigma))) inputs (get-sigmas inputs)))

(defn score-likelihood [inputs outputs]
  (mapv (fn [in out sigma] (log-prob (normal in sigma) out)) inputs outputs (get-sigmas inputs)))

(defn plot-joint-samples [f]
  (let [domain (range -10 11 0.15)
        ef (eval f)]
        ;noisy-function (fn [x] (sample-likelihood (ef x)))]
    (println f)
    (plot/list-plot (map list domain (sample-likelihood (mapv ef domain))) :joined true :x-title (str f))))

(defn score-operation 
  "each of the operations has the same (fixed) probability, independent of the operation"
  [op]
  (log (/ 1. (count operations))))

(defn score-symbol 
  "the symbol 'x has probability 0.4; each of the 10 integer constants has 
   equal probability 0.6 * 0.1 = 0.06"
  [sym]
  (log (if (number? sym) 0.06 0.4)))


(defn score-expr [expr]
  (cond (number? expr) 0.04
        (= 'x expr) 0.3
        :else (let [[op expr1 expr2] expr
                    expr-score (+ (score-operation op) (score-expr expr1) (score-expr expr2))]
                (+ 0.3 expr-score))))
(defn score-function [body]
  (let [[op sym1 sym2] (:value body)]
    (+ (score-operation op) (score-expr sym1) (score-expr sym2))))

(defn plot-function [f]
  (let [domain (range -10 11 0.15)
        ef (eval f)]
    (println f)
    (plot/list-plot (map list domain (mapv ef domain)) :joined true :x-title (str f))))

 

    (defn sample-simple-proposal [prev-expr root]
  (let [[op symL symR] (:value prev-expr)
        [len-op len-left len-right] (:length prev-expr)
        lenL (if (list? symL) (first len-left) 1)
        lenR (if (list? symR) (first len-right) 1)
        pro1 (/ 1 len-op)
        pro2 (/ lenL len-op)
        pro3 (/ lenR len-op)
        which-to-change (sample* (discrete [pro1 pro2 pro3]))]
      ;change operation
    (cond (= which-to-change 0) 
            (let [op-to-change (if (and (= lenL 1) (= lenR 1)) (sample* (discrete [0.5 0.5])) 0)] 
                  ;operation --> new operation
              (cond (= op-to-change 0) 
                    (let[]{:value (list (sample-operation) symL symR) :length (:length prev-expr)})
                    ;operation --> contract
                    (= op-to-change 1) 
                      (if (= root 1) 
                          (let[] {:value (list (sample-operation) symL symR) :length (:length prev-expr)} )
                          (let[]{:value (uniform-propose-symbol) :length 1}))
              )
            )
          ;change symL
          (= which-to-change 1) 
            (let [symL-to-change (if (= lenL 1) (sample* (discrete [0.5 0.5])) 2)]
                    ;terminal --> terminal
              (cond (= symL-to-change 0) 
                    (let[] {:value (list op (uniform-propose-symbol) symR) :length (list len-op len-left len-right)})
                    ;terminal --> expression
                    (= symL-to-change 1) 
                    (let[] {:value (list op (sample-simple) symR) :length (list (+ len-op 2) '(3 1 1) len-right)})
                    ;deep find
                    (= symL-to-change 2) 
                      (let [expL {:value symL :length len-left}
                              new-left (sample-simple-proposal expL 0)
                              new-left-value (:value new-left)
                              new-left-len (if (list? new-left-value) (first (:length new-left)) 1)]
                                           {:value (list op new-left-value symR) :length (list (+ new-left-len lenR 1) (:length new-left) len-right)})
              )
            )
          ;change symR
          (= which-to-change 2) 
            (let [symR-to-change (if (= lenR 1) (sample* (discrete [0.5 0.5])) 2)] 
                    ;terminal --> terminal
              (cond (= symR-to-change 0) 
                    (let[]  {:value (list op symL (uniform-propose-symbol)) :length (list len-op len-left len-right)})
                    ;terminal --> expression
                    (= symR-to-change 1) 
                    (let[] 
                      {:value (list op symL (sample-simple)) :length (list (+ len-op 2) len-left (list 3 1 1))})
                    ;deep find
                    (= symR-to-change 2) 
                    (let [expR {:value symR :length len-right}
                              new-right (sample-simple-proposal expR 0)
                              new-right-value (:value new-right)
                              new-right-len (if (list? new-right-value) (first (:length new-right)) 1)]
                                           {:value (list op symL new-right-value) :length (list (+ new-right-len lenL 1) len-left (:length new-right) )})
              )
            )
    )
  )
)



(defn mh-dependent-proposals [prev-expr inputs outputs]
  (let [; compute p(f, y)
        prev-fn-vals (mapv (eval (make-fn prev-expr)) inputs)
        ;_ (println (score-likelihood prev-fn-vals outputs))
        prev-score (+ (score-function prev-expr) (reduce + (score-likelihood prev-fn-vals outputs)))
        ; propose new f' from q(f' | f)
        next-expr (sample-simple-proposal prev-expr 1)
        ; compute p(f', y)
        next-fn-vals (mapv (eval (make-fn next-expr)) inputs)
    next-score (+ (score-function next-expr) (reduce + (score-likelihood next-fn-vals outputs)))
        accept-ratio (exp (- next-score prev-score))]
    (if (sample* (flip accept-ratio))
      [next-expr next-score]
      [prev-expr prev-score])))

(defn run-mh-sampler [mh-transition inputs outputs num-samples]
  (let [init-expr (sample-function)]
    (loop [samples []
           scores []
           prev-expr init-expr]
      (if (> (count samples) num-samples)
        {:samples samples :scores scores}
        (let [[next-expr next-score] (mh-transition prev-expr inputs outputs)]
          (recur (conj samples next-expr)
                 (conj scores next-score)
                 next-expr))))))
(def inputs-exp (range -10 10 1))
(def outputs-exp (mapv exp inputs-exp))



(time (def posterior-dependent-proposals 
        (run-mh-sampler mh-dependent-proposals inputs-exp outputs-exp 1000)))

(def approximations-to-exp (into #{} (drop 800 (:samples posterior-dependent-proposals))))
;(clojure.pprint/pprint approximations-to-exp)

(defn rms [expr inputs outputs]
  (let[
       value (mapv (eval expr) inputs)
       ]
  (reduce + (score-likelihood value outputs))))

 (defn mean-rms [appro-to-exp]
(/ (reduce + (mapv #(rms % inputs-exp outputs-exp) (mapv make-fn appro-to-exp))) 
   (count appro-to-exp)))
 


 ;Choose drop number
 (def a1 (drop 1990 (:samples posterior-dependent-proposals)))
 (mean-rms a1)


;Plot 
;(for [func (take 3 approximations-to-exp)]
 ; (do
  ;  (plot/compose
   ;   (plot-function (make-fn func))
    ;  (plot/list-plot (map list inputs-exp outputs-exp)))))
