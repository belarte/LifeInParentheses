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

(s/def :alu/expression (s/keys :req [:alu/dimensions
                                     :alu/output
                                     :alu/steps]
                               :opt [:alu/pattern]))

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

(defn bit
  "Represents a single bit as an input to an expression."
  [n]
  {:post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [e bit>
        f (e :alu/generator)]
    (-> e
        (assoc :alu/pattern (f n))
        (dissoc :alu/generator))))

(def one (=> bit> 1))

(def zero (=> bit> 0))

(defn- evaluate [expression]
  (let [{:keys [alu/dimensions alu/steps alu/pattern]} expression
        board (life/create-board (dimensions :alu/width) (dimensions :alu/height) [pattern])]
    (life/simulate board steps)))

(defn read>
  "Reads a single bit as the output of an expression."
  [expression args]
  (let [exp       (=> expression args)
        output    (-> exp :alu/output :alu/position)
        last-iter (last (evaluate exp))]
    (if (contains? (last-iter :alive-cells) output) 1 0)))

(defn read-bit
  "Reads a single bit as the output of an expression."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]}
  (let [output (get-in expression [:alu/output :alu/position])
        last-iteration (last (evaluate expression))]
    (if (contains? (last-iteration :alive-cells) output) 1 0)))

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

(defn not-bit
  "Negates a single bit. Expects the input to be facing bottom right, if not the input will be flipped."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [[l r]       (layout/make-intersect expression one)
        [x y diff]  (get-intersection l r)
        height-diff (+ diff 3 1)
        height      (+ (-> l :alu/dimensions :alu/height) height-diff)
        steps-diff  (* 4 (+ diff 4))
        steps       (+ (r :alu/steps) steps-diff)
        output-pos  (coords/add [x y] [(- 4) 3])]
    (-> (layout/merge-expressions l r)
        (assoc-in [:alu/dimensions :alu/height] height)
        (assoc-in [:alu/output :alu/direction] :bottom-left)
        (assoc-in [:alu/output :alu/position] output-pos)
        (assoc :alu/steps steps))))

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

(defn and-bit
  "Combine left and right expressions to form an 'and' statement."
  [left right]
  {:pre  [(s/valid? :alu/expression left) (layout/within-bounds? left)
          (s/valid? :alu/expression right) (layout/within-bounds? right)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [[l r]       (layout/make-parallel left right)
        [_ n]       (layout/make-intersect r one)
        [x y diff]  (get-intersection l n)
        height-diff (+ diff 6 1)
        height      (+ (-> l :alu/dimensions :alu/height) height-diff)
        steps-diff  (* 4 (+ diff 6))
        steps       (+ (n :alu/steps) steps-diff)
        output-pos  (coords/add [x y] [6 6])
        eater-pos   (coords/add [x y] [-6 3])
        eater       (patterns/offset (patterns/flip-x patterns/eater) eater-pos)]
      (-> (layout/merge-expressions l r n)
          (assoc-in [:alu/dimensions :alu/height] height)
          (assoc-in [:alu/output :alu/direction] :bottom-right)
          (assoc-in [:alu/output :alu/position] output-pos)
          (assoc :alu/steps steps)
          (update :alu/pattern set/union eater))))

(defn or-bit
  "Combine left and right expressions to form an 'or' statement."
  [left right]
  {:pre [(s/valid? :alu/expression left) (layout/within-bounds? left)
         (s/valid? :alu/expression right) (layout/within-bounds? right)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (not-bit (and-bit (not-bit left) (not-bit right))))

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

(defn write-byte
  "Represents byte as a sequence of expressions. Little-endian representation."
  [n]
  {:pre [(>= n 0) (< n 256)]}
  (->> (to-base-2 n)
       (map bit)
       layout/spread-x))

(defn read-byte
  "Reads a bit as the output of a  sequence of expressions."
  [expressions]
  {:pre [(= 8 (count expressions))]}
  (->> expressions
       (map read-bit)
       from-base-2))

(comment
  (=> (layout/shift> (layout/flip-x> bit>) [1 2]) 1)
  (=> (layout/merge-expressions> (layout/flip-x> bit>) (layout/shift> bit> [3 3]) bit>) [1 1 1])
  (read-byte (write-byte 12))
  (s/explain :alu/expression one)
  (not-bit one)
  (print-e (not-bit (layout/wire one 3)))
  (print-e (layout/align-with-origin (and-bit one one)))
  (let [exp (layout/align-with-origin (and-bit one (and-bit one one)))
        board (life/create-board ((exp :alu/dimensions) :alu/width) ((exp :alu/dimensions) :alu/height) [(exp :alu/pattern)])]
    (println exp)
    (println (life/draw-board board))
    (print-e exp))
  (read-bit one)
  (layout/within-bounds? one))
