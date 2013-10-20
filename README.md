# Space Vectors

A collection of functions and a parser bundled with a repl to
enable faster calculations on vectors, lines and planes in space.

## Usage
The following protocols are implemented:

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

As well as the following functions between elements:

* Angle (and parallel? and perpendicular?)
* Distance (and on?)
* Intersection
* Projection
* Skewed lines

Strings in regular mathematical notation are automatically parsed so you
can do things like: `(distance "(1,2,3)" "(2,-4,5)")` as well as `(distance [1 2 3] [2 -4 5])`.

You can also mix the two: `(angle [1 2 3] "(0,0,-2) + t * (4,2,1)")` and the syntax is very
forgiving so this is legal: `(intersection "1 2 3 4 5 6" "0 2 -3 1")` for the intersection of
a line and a plane.

You can pretty-print with `mathy` as in `(mathy [1 2 3]) => "(1,2,3)"`.

As the REPL is just a standard Clojure REPL you can do things like saving an element for reuse: `(def l (line "(0,0,0)" "(1,2,3)"))`. Now `l` is a line that goes through (0,0,0) and has the direction (1,2,3).
Then one can do: `(distance l "3 -1 2")` and so on.
You can also do further calculations on the returned result: `(angle "1 2 3" (intersection "2x-3y-1z+4=0" "4 4 1 2"))`.

## Thanks
Thanks to [Instaparse](https://github.com/Engelberg/instaparse) and [Expresso](https://github.com/clojure-numerics/expresso) for making it easy to parse user-input and solve equations respectively. And for extensive documentation!

Thanks to [REPL-y](http://github.com/trptcolin/reply) for providing a drop-in Clojure repl.

## License

Copyright © 2013 Andreas From

Distributed under the Eclipse Public License, the same as Clojure.
