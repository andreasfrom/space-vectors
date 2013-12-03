(ns space-vectors.core
  (:require [instaparse.core :as insta]
            [instaparse.failure :refer [pprint-failure]]
            [clojure.repl :refer [doc]])
  (:import (java.text NumberFormat)
           (java.util Locale)))

;;;; Types
(defn space-type
  "If `x` has a :type key, return its value.
   Else if it's a vector return :vector.
   Else return its (type)."
  [x]
  (get x :type (condp = (type x)
                 clojure.lang.PersistentVector :vector
                 clojure.lang.Keyword x
                 java.lang.Long :number-long
                 java.lang.Double :number-double
                 clojure.lang.Ratio :number-ratio
                 (type x))))

(defn as-line [op r] {:type :line :op op :r r})
(defn as-plane [n d] {:type :plane :n n :d d})
(defn as-pplane [op r1 r2] {:type :pplane :op op :r1 r1 :r2 r2})

;;;; Printing

(defn- sign
  [n]
  (if (neg? (read-string n))
    n
    (str "+" n)))

(defmulti mathy
  "Pretty-print math."
  space-type)

(defmethod mathy :number-double
  [n]
  (.format (NumberFormat/getInstance (Locale/US)) n))

(defmethod mathy :number-ratio
  [n]
  (mathy (double n)))

(defmethod mathy :vector
  [v]
  (str "(" (apply str (interpose ", " (map mathy v))) ")"))

(defmethod mathy :line
  [{:keys [op r]}]
  (str (mathy op) " + t * " (mathy r)))

(defmethod mathy :plane
  [{[a b c] :n d :d}]
  (let [[b c d] (map (comp sign mathy) [b c d])]
    (str (mathy a) "x" b "y" c "z" d "=0")))

(defmethod mathy :pplane
  [{:keys [op r1 r2]}]
  (str (mathy op) " + s * " (mathy r1) " + t * " (mathy r2)))

(defmethod mathy :default [x] (str x))


;;;; User variables
(def user-vars (atom {}))

;;;; Parsing

(declare normal)
(defn pplane->plane [a] (if (= (:type a) :pplane) (normal a) a))

(def transform-basics
  {:number (fn [n & ns] (read-string (apply str n ns)))
   :vector (fn [& es] (apply vector es))
   :line (fn [op r] (as-line op r))
   :plane (fn [a b c d] (as-plane [a b c] d))
   :pplane (fn [op r1 r2] (as-pplane op r1 r2))
   :word (partial apply str)
   :sign (fn ([] 1) ([sign] (if (= sign "+") 1 -1)))
   :one (constantly 1)
   :nested identity
   :S identity})

(def transform-func
  (merge transform-basics
         {:function (fn [f & xs] (try (apply (ns-resolve 'space-vectors.core (symbol f))
                                         (->> xs (map pplane->plane) (sort-by space-type)))
                                  (catch Exception e (.getMessage e))))
          :def (fn [name elm] (do (swap! user-vars assoc name elm) (mathy elm)))
          :var (fn [name] (if (contains? @user-vars name)
                            (get @user-vars name)
                            (throw (Exception. (str name " not found.\nDid you mean any of: "
                                                    (apply str (interpose ", " (keys @user-vars))))))))}))

(declare mathy)

(def transform-print
  (merge transform-basics
         {:function (fn [f & xs] (apply str f " " (interpose ", " (map mathy xs))))
          :nested (fn [f] (str "(" f ")"))
          :def (fn [name elm] (str name " = " (mathy elm)))
          :var (fn [name] (str name))
          :S mathy}))

(def parser
  (insta/parser
   "S = var | def | elm | function

    var = word
    def = word [space] <'='> [space] (elm | function | number)
    <elm> = vector | line | plane | pplane
    <elmarg> = elm | nested | var

    function = (('length' | 'normalize') space vecarg)
             | (('dotp' | 'cross' | 'area' | 'between') space vecarg c vecarg)
             | (('param' space planearg) | ('normal' space pplanearg) | ('three-points' space (planearg | pplanearg)))
             | (('line' space vecarg c vecarg) | ('plane' space vecarg c vecarg [c vecarg]))
             | (('lwith' space linearg c numarg) | ('pwith' space pplanearg c numarg c number))
             | (('angle' | 'parallel?' | 'perpendicular?' | 'distance' | 'on?' | 'intersection' [space elmarg c] | 'projection' | 'skewed?') space elmarg c elmarg)

    nested = <lparen> function <rparen>

    vector = [lparen] numarg (c | space) numarg (c | space) numarg [rparen]
    <vecarg> = vector | nested | var
    line = vector [param] vector
    <linearg> = line | nested | var
    plane = (([sign] | one) <'x'> | number | number [<mult>] <'x'>) [space]
            (([sign] | one) <'y'> | number | number [<mult>] <'y'>) [space]
            (([sign] | one) <'z'> | number | number [<mult>] <'z'>) [space] number [<'='> [space] <'0'>]
    one = ''
    <planearg> = plane | nested | var
    sign = '+' | '-'
    pplane = vector [param] vector [param] vector
    <pplanearg> = pplane | nested | var

    word = #'[a-zA-Z]'+

    <param> = <word> | (<plus> <word> <mult>)
    <plus> = [space] '+' [space]
    <mult> = [space] '*' [space]
    <lparen> = [space] <'('> [space]
    <rparen> = [space] <')'> [space]
    <c> = [space] <','> [space]
    number = [space] ('+' | '-' | '') [space] (num [('.' | '/') num]) [space]
    <num> = #'[0-9]+'
    <numarg> = number | var
    <space> = <#'[ ]+'>"))

(defn parse
  "Parse the input and execute the functions."
  [input]
  (let [parsed (parser input)]
    (if (insta/failure? parsed)
      {:result (with-out-str (pprint-failure parsed))}
      {:result (try (mathy (insta/transform transform-func parsed))
                    (catch Exception e (.getMessage e)))
       :input  (insta/transform transform-print parsed)})))

;;;; Vectors

(defn length
  "Length of the vector."
  [v]
  (->> v (map #(Math/pow % 2)) (reduce +) Math/sqrt))

(defn normalize
  "Normalize the vector to a unit-vector."
  [v]
  (mapv #(/ % (length v)) v))

(defn dotp
  "Return the scalar product of two vectors."
  [a b]
  (reduce + (map * a b)))

(defn cross
  "Cross two vectors into a third."
  [a b]
  (let [[ax ay az] a, [bx by bz] b]
    [(- (* ay bz) (* az by))
     (- (* az bx) (* ax bz))
     (- (* ax by) (* ay bx))]))

(defn area
  "Return the area expanded by the two vectors."
  [a b]
  (length (cross a b)))

(defn between
  "Return vector between two points"
  [a b]
  (mapv - b a))

;;; Lines

(defn lwith
  "Find a point by specifying the parameter of the line."
  [{:keys [op r]} t]
  (mapv + op (map (partial * t) r)))

;;; Planes & PPlanes

(defn three-points
  "Return three points on a normal plane."
  [{:keys [n d]}]
  (let [[x y z] (map #(/ (- d) %) n)]
    [[x 0 0]
     [0 y 0]
     [0 0 z]]))

(defn param
  "Return a normal plane in parameter-form"
  [{:keys [n d] :as a}]
  (let [[o p q] (three-points a)
        r1 (between o p)
        r2 (between o q)]
    (as-pplane o r1 r2)))

(defn normal
  "Return the same plane as a normal vector and a point."
  [{:keys [op r1 r2 type] :as a}]
  (if (= type :pplane)
    (let [n (cross r1 r2)
          d (reduce - 0 (map * op n))]
      (as-plane n d))
    a))

(defn pwith
  "Return a point on the plane by specifying the two parameters.
   Takes a normal-plane."
  [s t a]
  (let [{:keys [op r1 r2]} (param a)]
    (mapv + op (map (partial * s) r1) (map (partial * t) r2))))

;;;; Helpers

(defn line
  "Takes two points and returns a line through them."
  [a b]
  (as-line a (between a b)))

(defn plane
  "Returns a normal-plane from a normal vector and a point
   or
   a plane in parameter form from three points."
  ([n p]
     (as-plane n (reduce - 0 (map * p n))))
  ([A B C]
     (let [op A
           r1 (between A B)
           r2 (between A C)]
       (as-pplane op r1 r2))))

;;;; Multimethods

;;; Mechanics

(defn types
  "Returns a seq of space-types for methods to match on."
  [& xs]
  (map space-type xs))

(defn unknown-input
  [args]
  (str "No method found for (sorted) input types: "
       (apply str (interpose ", " (apply types args)))))

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
  (let [v (Math/toDegrees (Math/acos (/ (dotp na nb) (* (length na) (length nb)))))]
    (min v (- 180 v))))

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
        pq (between op oq)
        res (/ (Math/abs (double (dotp n pq)))
           (length n))]
    (if (Double/isNaN res) 0 res)))

(defmethod distance [:plane :vector]
  [{n :n d :d} p]
  (/ (Math/abs (double (reduce + d (map * n p))))
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
      (distance beta p))
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

(defmethod intersection [:line :line]
  [{ap :op ar :r :as a} {bp :op br :r}]
  (let [lv3 (between ap bp)
        cab (cross ar br)
        c3b (cross lv3 br)
        planar-factor (dotp lv3 cab)]
    (if (= 0 planar-factor)
      (let [s (/ (dotp c3b cab) (reduce + (map #(* % %) cab)))]
        (lwith a s))
      (throw (Exception. "No intersection")))))

(defmethod intersection [:line :plane]
  [{[opx opy opz] :op [rx ry rz] :r :as l}
   {[a b c] :n d :d :as alpha}]
  (if-not (parallel? l alpha)
    (let [t (/ (- (+ (* a opx) (* b opy) (* c opz) d))
               (+ (* a rx) (* b ry) (* c rz)))]
      (lwith l t))
    (throw (Exception. "No intersection"))))

(defmethod intersection [:plane :plane]
  [{[a1 b1 c1] :n d1 :d :as alpha} {[a2 b2 c2] :n d2 :d :as beta}]
  (if-not (parallel? alpha beta)
      (let [op [0
                (/ (- (* c1 d2) (* c2 d1)) (- (* b1 c2) (* b2 c1)))
                (/ (- (* b2 d1) (* b1 d2)) (- (* b1 c2) (* b2 c1)))]
            r [1
               (/ (- (* a2 c1) (* a1 c2)) (- (* b1 c2) (* b2 c1)))
               (/ (- (* b2 a1) (* b1 a2)) (- (* b1 c2) (* b2 c1)))]]
        (as-line op r))
      (throw (Exception. "No intersection"))))

(defmethod intersection [:plane :plane :plane]
  [a b c]
  (intersection (intersection a b) c))

(defmethod intersection :default
  [& args]
  (unknown-input args))

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
    (as-line pi rm)))

(defmethod projection :default
  [& args]
  (unknown-input args))

;;;; Skewed lines

(defn skewed?
  "Returns whether two lines are skewed,
   ie. neither parallel nor touching."
  [l m]
  (not (or (parallel? l m) (on? l m))))
