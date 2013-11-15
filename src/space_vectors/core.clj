(ns space-vectors.core
  (:gen-class)
  (:require [instaparse.core :as insta]
            [numeric.expresso.core :as e]))

;;;; Parsing

(def transform-options
  {:number (fn [& ns] (read-string (apply str ns)))
   :vector (fn [& es] {:type :vector :comps (apply vector es)})
   :line (fn [op r] {:type :line :op op :r r})
   :plane (fn [a b c d] {:type :plane :n {:type :vector :comps [a b c]} :d d})
   :pplane (fn [op r1 r2] {:type :pplane :op op :r1 r1 :r2 r2})
   :func (fn [f & xs] (apply (ns-resolve 'space-vectors.core (symbol f)) (sort-by :type xs)))
   :S identity})

(def parser
  (insta/parser
   "S = func

    <elm> = (vector | line | plane | pplane) | expr

    func = vecfunc1 (vector | expr)
         | vecfunc2 (vector | expr) <c> (vector | expr)
         | 'lwith' (line | expr) <c> number
         | planefunc (plane | pplane | expr)
         | 'pwith' (pplane | expr) <c> number number
         | 'line' (vector | expr) <c> (vector | expr)
         | 'plane' (vector | expr) <c> (vector | expr) [<c> (vector | expr)]
         | genfunc elm <c> elm

    <expr> = <lpar> func <rpar>

    <vecfunc1> = 'length' | 'normalize'
    <vecfunc2> = 'dotp' | 'cross' | 'area' | 'between'

    <planefunc> = 'param' | 'three-points' | 'normal'

    <genfunc> = 'angle' | 'parallel?' | 'perpendicular?' | 'distance' | 'on?' | 'intersection' | 'projection' | 'skewed?'

    vector = [space] [lparen] [space] number space number space number [space] [rparen] [space]

    line = [space] vector [space] [<plus>] [space] [<word>] [space] [<mult>] vector

    plane = [space] number [[space] [<mult>] [space] <'x'>] [space]
            number [[space] [<mult>] [space] <'y'>] [space]
            number [[space] [<mult>] [space] <'z'>] [space]
            number [[space]  <'='>   [space] <'0'>]

    pplane = [space] vector   [space] [<plus>] [space]
             [<word>] [space] [<mult>] [space]
             vector   [space] [<plus>] [space]
             [<word>] [space] [<mult>] [space]
             vector

    <word> = #'[a-z]'+

    <plus> = '+'
    <mult> = '*'
    <lpar> = [space] '(' [space]
    <rpar> = [space] ')' [space]
    <lparen> = <'('> | <'<'> | <'['>
    <rparen> = <')'> | <'>'> | <']'>
    <c> = [space] ',' [space]
    number = ('+' | '-' | '') [space] #'[0-9]+' ['.' #'[0-9]+']
    <space> = (<#'[ ]+'> | <','>)+"))

(defn parse
  "Parse the input and execute the functions."
  [input]
  (->> (parser input)
       (insta/transform transform-options)))

;;;; Vectors

(defn length
  "Length of the vector."
  [{v :comps}]
  (->> v (map #(Math/pow % 2)) (reduce +) Math/sqrt))

(defn normalize
  "Normalize the vector to a unit-vector."
  [v]
  {:type :vector :comps (mapv #(/ % (length v)) (:comps v))})

(defn dotp
  "Return the scalar product of two vectors."
  [{a :comps} {b :comps}]
  (reduce + (map * a b)))

(defn cross
  "Cross two vectors into a third."
  [{a :comps} {b :comps}]
  (let [[ax ay az] a, [bx by bz] b
         r [(- (* ay bz) (* az by))
            (- (* az bx) (* ax bz))
            (- (* ax by) (* ay bx))]]
    {:type :vector :comps r}))

(defn area
  "Return the area expanded by the two vectors."
  [a b]
  (length (cross a b)))

(defn between
  "Return vector between two points"
  [{a :comps} {b :comps}]
  {:type :vector :comps (mapv - b a)})

;;; Lines

(defn lwith
  "Find a point by specifying the parameter of the line."
  [t {{op :comps} :op {r :comps} :r}]
  {:type :vector :comps (mapv + op (map (partial * t) r))})

;;; Planes & PPlanes

(defn three-points
  "Return three points on a normal plane."
  [{{n :comps} :n d :d}]
  (let [[x y z] (map #(/ (- d) %) n)]
    [[x 0 0]
     [0 y 0]
     [0 0 z]]))

(defn param
  "Return a normal plane in parameter-form"
  [{:keys [n d] :as a}]
  (let [opq (three-points a)
        [o p q] (map (fn [v] {:type :vector :comps v}) opq)
        r1 (between o p)
        r2 (between o q)]
    {:type :pplane :op o :r1 r1 :r2 r2}))

(defn normal
  "Return the same plane as a normal vector and a point."
  [{:keys [op r1 r2]}]
  (let [n (cross r1 r2)
        d (reduce - 0 (map * (:comps op) (:comps n)))]
    {:type :plane :n n :d d}))

(defn pwith
  "Return a point on the plane by specifying the two parameters."
  [s t {:keys [op r1 r2]}]
  {:type :vector :comps (mapv + (:comps op)
                              (map (partial * s) (:comps r1))
                              (map (partial * t) (:comps r2)))})

;;;; Helpers

(defn line
  "Takes two points and returns a line through them."
  [a b]
  {:type :line :op a :r (between a b)})

(defn plane
  "Returns a normal-plane from a normal vector and a point
   or
   a plane in parameter form from three points."
  ([n p]
     {:type :plane :n n :d (reduce - 0 (map * (:comps p) (:comps n)))})
  ([A B C]
     (let [op A
           r1 (between A B)
           r2 (between A C)]
       {:type :pplane :op op :r1 r1 :r2 r2})))

;;;; Multimethods

;;; Mechanics

(defn types
  "Returns a seq of `:type`s to for methods to match on."
  [& xs]
  (map :type xs))

(defn unknown-input
  [args]
  (str "No method found for input types: "
       (apply str (interpose ", " (map name (apply types args))))))

;;; Angle

(defmulti angle
  "Returns the angle between two elements."
  types)

(defmethod angle [:vector :vector]
  [a b]
  (let [[a' b'] (map normalize [a b])
        v (Math/toDegrees (Math/acos (/ (dotp a' b') (* (length a') (length b')))))]
    (if (Double/isNaN v) 0.0 v)))

(defmethod angle [:line :vector]
  [{r :r} a]
  (angle r a))

(defmethod angle [:line :line]
  [{rl :r} {rm :r}]
  (angle rl rm))

(defmethod angle [:plane :vector]
  [{n :n} r]
  (let [v (Math/toDegrees (Math/acos (/ (dotp n r) (* (length n) (length r)))))]
    ((comp first filter) pos? [(- 90 v) (- v 90)])))

(defmethod angle [:plane :plane]
  [{na :n} {nb :n}]
  (let [v (Math/acos (/ (dotp na nb) (* (length na) (length nb))))]
    (Math/toDegrees (min v (- 180 v)))))

(defmethod angle [:line :plane]
  [{r :r} a]
  (angle a r))

(defmethod angle :default
  [& args]
  (unknown-input args))

;;; Parallel

(defn parallel?
  "Returns whether two elements are parallel."
  [& xs]
  (let [v (apply angle xs)]
    (or (= v 0.0) (= v 180.0))))

;;; Perpendicular

(defn perpendicular?
  "Returns whether two elements are perpendicular."
  [& xs]
  (let [v (apply angle xs)]
    (or (= v 90.0) (= v 270.0))))

;;; Distance

(defmulti distance
  "Returns the distance between two elements."
  types)

(defmethod distance [:vector :vector]
  [p q]
  (->> (:comps (between p q))
       (map #(Math/pow % 2))
       (reduce +)
       Math/sqrt))

(defmethod distance [:line :vector]
  [{:keys [r op]} p]
  (/ (length (cross r (between op p)))
     (length r)))

(defmethod distance [:line :line]
  [{rl :r op :op} {rm :r oq :op}]
  (let [n (cross rl rm)
        pq (between op oq)]
    (/ (Math/abs (dotp n pq))
       (length n))))

(defmethod distance [:plane :vector]
  [{n :n d :d} {p :comps}]
  (/ (Math/abs (reduce + d (map * (:comps n) p)))
     (length n)))

(defmethod distance [:line :plane]
  [{op :op :as l} a]
  (if (parallel? l a)
    (distance a op)
    0))

(defmethod distance [:plane :plane]
  [{{[a b c] :comps} :n d :d :as alpha} beta]
  (if (parallel? alpha beta)
    (let [z (/ (- d) c)
          p [0 0 z]]
      (distance beta {:type :vector :comps p}))
    0))

(defmethod distance :default
  [& args]
  (unknown-input args))

;;; On
(def on?
  "Returns whether the element are touching."
  #(= 0.0 (apply distance %&)))

;;; Intersection

(defmulti intersection
  "Returns the intersection point or line between two elements."
  types)

(defn- line=line
  "Equal two lines yielding their [x y z] equations."
  [{{ap :comps} :op {ar :comps} :r} {{bp :comps} :op {br :comps} :r}]
  (let [[apx apy apz] ap [arx ary arz] ar
        [bpx bpy bpz] bp [brx bry brz] br]
    [(e/ex' (= (+ apx (* arx 't)) (+ bpx (* brx 's))))
     (e/ex' (= (+ apy (* ary 't)) (+ bpy (* bry 's))))
     (e/ex' (= (+ apz (* arz 't)) (+ bpz (* brz 's))))]))

(defmethod intersection [:line :line]
  [l m]
  (let [sol (first (apply e/solve '[t s] (line=line l m)))]
    (when (seq sol)
     (lwith ('t sol) l))))

(defmethod intersection [:plane :plane]
  [{{[a1 b1 c1] :comps} :n d1 :d} {{[a2 b2 c2] :comps} :n d2 :d}]
  (let [op [0
            (/ (- (* c1 d2) (* c2 d1)) (- (* b1 c2) (* b2 c1)))
            (/ (- (* b2 d1) (* b1 d2)) (- (* b1 c2) (* b2 c1)))]
        r [1
           (/ (- (* a2 c1) (* a1 c2)) (- (* b1 c2) (* b2 c1)))
           (/ (- (* b2 a1) (* b1 a2)) (- (* b1 c2) (* b2 c1)))]]
    {:type :line :op {:type :vector :comps op} :r {:type :vector :comps r}}))

(defmethod intersection [:line :plane]
  [{{[opx opy opz] :comps} :op {[rx ry rz] :comps} :r :as l}
   {{[a b c] :comps} :n d :d}]
  (let [texp (e/ex' (= 0 (+ (* a (+ opx (* rx 't)))
                            (* b (+ opy (* ry 't)))
                            (* c (+ opz (* rz 't)))
                            d)))
        t' (first (e/solve 't texp))]
    (when t'
      (lwith t' l))))

(defmethod intersection :default
  [& args]
  (unknown-input args))

;;; Projection

(defmulti projection
  "Returns the projection of one element onto the other."
  types)

(defmethod projection [:vector :vector]
  [a b]
  {:type :vector :comps (mapv (partial * (/ (dotp a b) (Math/pow (length b) 2))) (:comps b))})

(defmethod projection [:line :plane]
  [{r :r :as l} {n :n :as a}]
  (let [pi (intersection l a)
        rm (mapv (partial - (/ (dotp r n) (Math/pow (length n) 2))) (:comps r))]
    {:type :line :op pi :r {:type :vector :comps rm}}))

(defmethod projection :default
  [& args]
  (unknown-input args))

;;;; Skewed lines

(defn skewed?
  "Returns whether two lines are skewed,
   ie. neither parallel nor touching."
  [l m]
  (not (or (parallel? l m) (on? l m))))

;;;; Printing

(defn- sign
  [n]
  (if (neg? n)
    (str n)
    (str "+" n)))

(defmulti mathy
  "Protocol for pretty-printing math."
  :type)

(defmethod mathy :vector
  [{v :comps}]
  (str "(" (apply str (interpose "," v)) ")"))

(defmethod mathy :line
  [{:keys [op r]}]
  (str (mathy op) " + t * " (mathy r)))

(defmethod mathy :plane
  [{{[a b c] :comps} :n d :d}]
  (str a "x" (sign b) "y" (sign c) "z" (sign d) "=0"))

(defmethod mathy :pplane
  [{:keys [op r1 r2]}]
  (str (mathy op) " + s *" (mathy r1) " + t * " (mathy r2)))

(defmethod mathy :default [x] x)

;;;; Repl

(defn -main
  [& args]
  (loop []
    (print "> ") (flush)
    (let [input (read-line)
          output (try (parse input)
                      (catch Exception e
                        (str "Wrong input: " (.getMessage e))))]
      (println (mathy output))
      (when-not (contains? #{"quit" "exit"} input) (recur)))))
