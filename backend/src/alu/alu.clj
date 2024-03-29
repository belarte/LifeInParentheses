(ns alu.alu
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [alu.layout :as layout :refer [=>]]
            [life.coords :as coords]
            [life.life :as life]
            [life.patterns :as patterns]))

(s/def :alu/origin :coords/coords)
(s/def :alu/width (s/and int? pos?))
(s/def :alu/height (s/and int? pos?))
(s/def :alu/dimensions (s/keys :req [:alu/origin :alu/width :alu/height]))

(s/def :alu/position :coords/coords)
(s/def :alu/direction #{:bottom-left :bottom-right})
(s/def :alu/output (s/keys :req [:alu/position :alu/direction]))

(s/def :alu/steps (s/and int? (s/or :pos pos? :zero zero?)))
(s/def :alu/pattern :patterns/pattern)

; Expression
(s/def :alu/expression (s/keys :req [:alu/dimensions
                                     :alu/output
                                     :alu/steps]
                               :opt [:alu/pattern]))

(s/def :byte/value (s/and int? #(>= % 0) #(< % 256)))

; Argument
(s/def :byte/argument (s/or :unary-argument   :byte/value
                            :binary-arguments (s/coll-of :byte/argument :kind vector? :count 2)))

(s/def :alu/result :byte/value)
(s/def :alu/iterations (s/coll-of :alu/pattern))

;Output
(s/def :alu/read-output (s/keys :req [:alu/result
                                      :alu/width
                                      :alu/height
                                      :alu/iterations]))

(def bit>
  "Generator for a single bit."
  {:alu/dimensions {:alu/origin [0 0]
                    :alu/width 5
                    :alu/height 5}
   :alu/output {:alu/position [3 3]
                :alu/direction :bottom-right}
   :alu/steps 0
   :alu/generator (fn [n]
                    {:pre [(or (= 1 n) (= 0 n))]}
                    (if (zero? n) #{} (patterns/offset patterns/glider [1 1])))})

(defn evaluate [expression]
  (let [{:keys [alu/dimensions alu/steps alu/pattern]} expression
        board (life/create-board (dimensions :alu/width) (dimensions :alu/height) [pattern])]
    (life/simulate board steps)))

(defn- print-e
  "Prints all steps generated."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]}
  (run! #(do (println %) (println (life/draw-board %) (Thread/sleep 100)))
        (evaluate expression)))

(defn- get-intersection [left right]
  (let [[x0 y0] (-> left :alu/output :alu/position)
        [x1 __] (-> right :alu/output :alu/position)
        diff    (int (/ (- x1 x0) 2))]
      [(+ x0 diff) (+ y0 diff) diff]))

(defn not>
  "Negates a single bit. Expects the input to be facing bottom right, if not the input will be flipped."
  [expression]
  {:pre  [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [[l r]       (layout/make-intersect> expression bit>)
        [x y diff]  (get-intersection l r)
        height-diff (+ diff 3 1)
        height      (+ (-> l :alu/dimensions :alu/height) height-diff)
        steps-diff  (* 4 (+ diff 4))
        steps       (+ (r :alu/steps) steps-diff)
        output-pos  (coords/add [x y] [(- 4) 3])
        generator   (fn [args]
                      (let [fl (l :alu/generator)
                            fr (r :alu/generator)]
                        (set/union (fl args) (fr 1))))]
    (-> (layout/merge-expressions> l r)
        (assoc-in [:alu/dimensions :alu/height] height)
        (assoc-in [:alu/output :alu/direction] :bottom-left)
        (assoc-in [:alu/output :alu/position] output-pos)
        (assoc :alu/steps steps)
        (assoc :alu/generator generator))))

(defn and>
  "Combine left and right expressions to form an 'and' statement."
  [left right]
  {:pre  [(s/valid? :alu/expression left) (layout/within-bounds? left)
          (s/valid? :alu/expression right) (layout/within-bounds? right)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [[l r]       (layout/make-parallel> left right)
        [_ n]       (layout/make-intersect> r bit>)
        [x y diff]  (get-intersection l n)
        height-diff (+ diff 6 1)
        height      (+ (-> l :alu/dimensions :alu/height) height-diff)
        steps-diff  (* 4 (+ diff 6))
        steps       (+ (n :alu/steps) steps-diff)
        output-pos  (coords/add [x y] [6 6])
        eater-pos   (coords/add [x y] [-6 3])
        eater       (patterns/offset (patterns/flip-x patterns/eater) eater-pos)
        generator   (fn [[left-args right-args]]
                      (let [lf (l :alu/generator)
                            rf (r :alu/generator)
                            nf (n :alu/generator)]
                        (set/union (lf left-args) (rf right-args) (nf 1) eater)))]
    (-> (layout/merge-expressions> l r n)
        (assoc-in [:alu/dimensions :alu/height] height)
        (assoc-in [:alu/output :alu/direction] :bottom-right)
        (assoc-in [:alu/output :alu/position] output-pos)
        (assoc :alu/steps steps)
        (assoc :alu/generator generator))))

(defn or>
  "Combine left and right expressions to form an 'or' statement."
  [left right]
  {:pre [(s/valid? :alu/expression left) (layout/within-bounds? left)
         (s/valid? :alu/expression right) (layout/within-bounds? right)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (not> (and> (not> left) (not> right))))

(defn- to-base-2 [n]
  (loop [n n
         acc []]
    (if (zero? n)
      (take 8 (concat acc (repeat 0)))
      (recur (int (/ n 2)) (conj acc (mod n 2))))))

(defn- from-base-2 [n]
  (->> n
       (map vector [1 2 4 8 16 32 64 128])
       (map #(apply * %))
       (reduce +)))

(defmulti convert class)

(defmethod convert java.lang.Number [arg]
  (to-base-2 arg))

(defmethod convert clojure.lang.PersistentVector [args]
  (->> (map convert args)
       (apply map vector)))

(def byte>
  "Represents byte as a sequence of expressions. Little-endian representation."
  bit>)

(defn- board->int [board outputs]
  (-> (map #(if (contains? (board :alive-cells) %) 1 0) outputs)
      (from-base-2)))

(defn read>
  "Computes an expression. Returns the result as well as each steps."
  [expression args]
  {:pre  [(s/valid? :byte/argument args)]
   :post [(s/valid? :alu/read-output %)]}
  (let [expressions (->> (layout/align-with-origin> expression)
                         (repeat)
                         (take 8)
                         (apply layout/spread-x>))
        outputs (map #(get-in % [:alu/output :alu/position]) expressions)
        steps (-> (apply layout/merge-expressions> expressions)
                  (=> (convert args))
                  (evaluate))]
    {:alu/result     (board->int (last steps) outputs)
     :alu/width      ((first steps) :width)
     :alu/height     ((first steps) :height)
     :alu/iterations (map #(get % :alive-cells) steps)}))

(comment
  (let [x {:alu/result 42
           :alu/width 16
           :alu/height 9
           :alu/iterations [#{[1 2] [2 3]}
                            #{[1 3]}
                            #{}
                            #{[1 2] [4 5] [6 7]}]}]
    (s/conform :alu/read-output x))
  (s/explain :byte/argument [[0 [255 8]] [4 5]])
  (convert [5 [7 63]])
  (=> (layout/shift> (layout/flip-x> bit>) [1 2]) 1)
  (=> (layout/merge-expressions> (layout/flip-x> bit>) (layout/shift> bit> [3 3]) bit>) [1 1 1])
  (read> byte> 42)
  (print-e (=> (not> (layout/wire> bit> 3)) 1))
  (print-e (=> (layout/align-with-origin> (and> bit> bit>)) [1 1]))
  (let [exp   (=> (layout/align-with-origin> (and> bit> (and> bit> bit>))) [1 [1 1]])
        board (life/create-board ((exp :alu/dimensions) :alu/width) ((exp :alu/dimensions) :alu/height) [(exp :alu/pattern)])]
    (println exp)
    (println (life/draw-board board))
    (print-e exp)))
