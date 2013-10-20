(ns space-vectors.core
  (:gen-class)
  (:require [instaparse.core :as insta]
            [numeric.expresso.core :as e]
            [clojure.repl :refer :all]))

;;;; Protocols

(defprotocol Vectors
  "Protocol for operations on vectors in space."
  (length [this] "Length of the vector.")
  (normalize [this] "Normalize the vector to a unit-vector.")
  (dotp [a b] "Return the scalar product of two vectors.")
  (cross [a b] "Cross two vectors into a third.")
  (area [a b] "Return the area expanded by the two vectors.")
  (between [a b] "Return vector between two points."))

(defprotocol Lines
  "Protocol for operating on lines of the form op+t*r."
  (lwith [this t] "Find a point by specifying the parameter."))

(defprotocol Planes
  "Protocol for planes in space."
  (param [this] "Return the same plane in parameter-form")
  (three-points [this] "Return three points on the plane.")
  (normal [this] "Return the plane as parameters.")
  (pwith [this s t] "Get point by specifying variables."))

(defprotocol Printing
  "Print the element."
  (mathy [this] "Print in mathematical notation."))

;;;; Records

(defrecord Line [op r])
(defrecord Plane [n d])
(defrecord PPlane [op r1 r2])

;;;; Parsing

(def transform-options
  {:number (fn [& ns] (read-string (apply str ns)))
   :vector (fn [& es] (apply vector es))
   :line (fn [op r] (Line. op r))
   :plane (fn [a b c d] (Plane. [a b c] d))
   :pplane (fn [op r1 r2] (PPlane. op r1 r2))
   :S identity})

(def parser
  (insta/parser
   "S = vector | line | plane | pplane
    plane = number [[space] [<mult>] [space] <'x'>] [space]
            number [[space] [<mult>] [space] <'y'>] [space]
            number [[space] [<mult>] [space] <'z'>] [space]
            number [[space]  <'='>   [space] <'0'>]

    pplane = vector   [space] [<plus>] [space]
             [<word>] [space] [<mult>] [space]
             vector   [space] [<plus>] [space]
             [<word>] [space] [<mult>] [space]
             vector

    line = vector [space] [<plus>] [space] [<word>] [space] [<mult>] vector

    <word> = #'[a-z]'+

    vector = [space] [lparen] [space] number space number space number [space] [rparen] [space]

    <plus> = '+'
    <mult> = '*'
    <lparen> = <'('> | <'<'> | <'['>
    <rparen> = <')'> | <'>'> | <']'>
    number = ('+' | '-' | [space]) #'[0-9]'+ ['.' #'[0-9]'+]
    <space> = (<#'[ ]+'> | <','>)+"))

(defn parse
  "Parses the input into Clojure data, but only if it is a string."
  [input]
  (if (string? input)
    (->> (parser input)
         (insta/transform transform-options))
    input))

;;;; Implementations
;;; Vector

(extend-protocol Vectors
  clojure.lang.PersistentVector
  (length [this]
    (->> this (map #(Math/pow % 2)) (reduce +) Math/sqrt))
  (normalize [this]
    (mapv #(/ % (length this)) this))
  (dotp [a b]
    (reduce + (map * a b)))
  (cross [a b]
    (let [[ax ay az] a, [bx by bz] b]
      [(- (* ay bz) (* az by))
       (- (* az bx) (* ax bz))
       (- (* ax by) (* ay bx))]))
  (area [a b]
    (length (cross a b)))
  (between [a b]
    (mapv - b a))

  java.lang.String
  (length [this] (length (parse this)))
  (normalize [this] (normalize (parse this)))
  (dotp [a b] (dotp (parse a) (parse b)))
  (cross [a b] (cross (parse a) (parse b)))
  (area [a b] (area (parse a) (parse b)))
  (between [a b] (between (parse a) (parse b))))

;;; Line

(extend-protocol Lines
  Line
  (lwith [{:keys [op r]} t]
    (mapv + op (map (partial * t) r)))

  java.lang.String
  (lwith [this t] (lwith (parse this) t)))

;;; Plane & PPlane

(extend-protocol Planes
  Plane
  (normal [this] this)
  (param [{:keys [n d] :as a}]
    (let [[o p q] (three-points a)
          r1 (between o p)
          r2 (between o q)]
      (PPlane. o r1 r2)))
  (three-points [{n :n d :d}]
    (let [[x y z] (map #(/ (- d) %) n)]
      [[x 0 0]
       [0 y 0]
       [0 0 z]]))

  PPlane
  (normal [{:keys [op r1 r2]}]
    (let [n (cross r1 r2)
          d (reduce - 0 (map * op n))]
      (Plane. n d)))
  (param [this] this)
  (three-points [this]
    (three-points (normal this)))
  (pwith [{:keys [op r1 r2]} s t]
    (mapv + op (map (partial * s) r1) (map (partial * t) r2)))

  java.lang.String
  (normal [this] (normal (parse this)))
  (param [this] (param (parse this)))
  (three-points [this] (three-points (parse this)))
  (pwith [this & vars] (apply pwith (parse this) vars)))

;;;; Printing

(defn- sign
  [n]
  (if (neg? n)
    (str n)
    (str "+" n)))

(extend-protocol Printing
  clojure.lang.PersistentVector
  (mathy [this]
    (str "(" (apply str (interpose "," this)) ")"))
  Line
  (mathy [{:keys [op r]}]
    (str (mathy op) " + t * " (mathy r)))
  Plane
  (mathy [{[a b c] :n d :d}]
    (str a "x" (sign b) "y" (sign c) "z" (sign d) "=0"))
  PPlane
  (mathy [{:keys [op r1 r2]}]
    (str (mathy op) " + s *" (mathy r1) " + t * " (mathy r2))))

;;;; Helpers

(defn line
  "Takes two points and returns a line through them."
  [a b]
  (Line. (parse a) (between (parse a) (parse b))))

(defn plane
  "Returns a normal-plane from a normal vector and a point
   or
   a plane in parameter form from three points."
  ([n p]
     (Plane. (parse n) (reduce - 0 (map * (parse p) (parse n)))))
  ([A B C]
     (let [[A B C] (map parse [A B C])]
       (let [op A
             r1 (between A B)
             r2 (between A C)]
         (PPlane. op r1 r2)))))

;;;; Multimethods

;;; Mechanics

(defn type->keyword
  "Converts an element to a keyword representing its type."
  [x]
  (condp = (type x)
    space_vectors.core.Line :line
    space_vectors.core.Plane :plane
    space_vectors.core.PPlane :pplane
    clojure.lang.PersistentVector :vector
    :unknown))

(def types #(map type->keyword %&))

(defn parse-and-sort
  "Parses and sort a collection of arguments."
  [args]
  (->> args
       (map parse)
       (map #(if (= :pplane (type->keyword %)) (normal %) %))
       (sort-by type->keyword)))

(defn recurse-parsed
  "Applies `f` to a sorted and parsed version of `args`.
   Prints an error if the input is invalid."
  [f & args]
  (let [xs (parse-and-sort args)]
    (if (some #{:unknown} (map type->keyword xs))
      (println "Error: " xs)
      (apply f xs))))

;;; Angle

(defmulti angle
  "Returns the angle between two elements."
  types)

(defmethod angle [:vector :vector]
  [a b]
  (let [[a b] (map normalize [a b])
        v (Math/toDegrees (Math/acos (/ (dotp a b) (* (length a) (length b)))))]
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
  (apply recurse-parsed angle args))

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
  (->> (between p q)
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
  [{n :n d :d} p]
  (/ (Math/abs (reduce + d (map * n p)))
     (length n)))

(defmethod distance [:line :plane]
  [{op :op :as l} a]
  (if (parallel? l a)
    (distance a op)
    0))

(defmethod distance [:plane :plane]
  [{[a b c] :n d :d :as alpha} beta]
  (if (parallel? alpha beta)
    (let [z (/ (- d) c)
          p [0 0 z]]
      p)
    0))

(defmethod distance :default
  [& args]
  (apply recurse-parsed distance args))

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
  [{ap :op ar :r} {bp :op br :r}]
  (let [[apx apy apz] ap [arx ary arz] ar
        [bpx bpy bpz] bp [brx bry brz] br]
    [(e/ex' (= (+ apx (* arx 't)) (+ bpx (* brx 's))))
     (e/ex' (= (+ apy (* ary 't)) (+ bpy (* bry 's))))
     (e/ex' (= (+ apz (* arz 't)) (+ bpz (* brz 's))))]))

(defmethod intersection [:line :line]
  [l m]
  (let [sol (first (apply e/solve '[t s] (line=line l m)))]
    (when (seq sol)
     (lwith l ('t sol)))))

(defmethod intersection [:plane :plane]
  [{[a1 b1 c1] :n d1 :d} {[a2 b2 c2] :n d2 :d}]
  (let [op [0
            (/ (- (* c1 d2) (* c2 d1)) (- (* b1 c2) (* b2 c1)))
            (/ (- (* b2 d1) (* b1 d2)) (- (* b1 c2) (* b2 c1)))]
        r [1
           (/ (- (* a2 c1) (* a1 c2)) (- (* b1 c2) (* b2 c1)))
           (/ (- (* b2 a1) (* b1 a2)) (- (* b1 c2) (* b2 c1)))]]
    (Line. op r)))

(defmethod intersection [:line :plane]
  [{[opx opy opz] :op [rx ry rz] :r :as l}
   {[a b c] :n d :d}]
  (let [texp (e/ex' (= 0 (+ (* a (+ opx (* rx 't)))
                            (* b (+ opy (* ry 't)))
                            (* c (+ opz (* rz 't)))
                            d)))
        t' (first (e/solve 't texp))]
    (when t'
      (lwith l t'))))

(defmethod intersection :default
  [& args]
  (apply recurse-parsed intersection args))

;;; Projection

(defmulti projection
  "Returns the projection of one element onto the other."
  types)

(defmethod projection [:vector :vector]
  [a b]
  (mapv (partial * (/ (dotp a b) (Math/pow (length b) 2))) b))

(defmethod projection [:line :plane]
  [{r :r :as l} {n :n :as a}]
  (let [pi (intersection l a)
        rm (mapv (partial - (/ (dotp r n) (Math/pow (length n) 2))) r)]
    (Line. pi rm)))

(defmethod projection :default
  [& args]
  (apply recurse-parsed projection args))

;;;; Skewed lines

(defn skewed?
  "Returns whether two lines are skewed,
   ie. neither parallel or touching."
  [l m]
  (not (or (parallel? l m) (on? l m))))

;;;; Repl

(defn -main
  "The main entry point, delegates to REPL-y."
  [& args]
  (require 'reply.main)
  ((ns-resolve 'reply.main 'launch)
   {:custom-eval '(do (println "\n\nWelcome to the Space Vectors application.\n\nStart by running \"(ns space-vectors.core)\"!.\n\nExample command: (distance \"(1,2,3)\" \"2x-4y+1z-5=0\")\nDocumentation is available through doc: (doc dotp)"))}))
