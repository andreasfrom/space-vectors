# Space Vectors

A collection of functions and a parser bundled with a repl to
enable faster calculations on vectors, lines and planes in space.

## Sample session
[Using the Space Vectors command line](spacevectorssample.png)

## Supported functions
### Vectors
* Length `length a`
* Normalize to a unit vector `normalize a`
* Dot/scalar product `dotp a, b`
* Cross product `cross a, b`
* Area `area a, b`
* Vector between two points (position vectors) `between a, b`

### Lines
* Specify the parameter to get a point on the line `line l, t`

### Planes
* Turn a plane of the form `ax+by+cz+d=0` into `(x,y,z) = op+t*r1+s*r2`: `param a`
* Go the other way: `normal a`
* Return three points on a normal-plane `three-points a` 
* Get a point by specifying two parameters `pwith a, t, s`

### Functions between elements
* Angle: `angle x, y`, `parallel? x, y` and `perpendicular? x, y`
* Distance: `distance x, y` and `on? x, y`
* Intersection: `intersection x, y`
* Projection: `projection x, y`
* Are two lines skewed? `skewed? l, m`

## Syntax
Braces, "(), "<>" and "[]" are optional and comma is considered whitespace except for in between function arguments.

### Vectors
All of the following is legal for a vector `(-2,1,3)`
* `(-2,1,3)`
* `(-2, 1, 3)`
* `(-2 1 3)`
* `<-2, 1, 3>`
* `[-2 1 3]`
* `-2 1 3`
* `-2,1 3`

If there are three numbers, optionally surrounded by "()", "<>" or "[]", it's interpreted as a vector.

### Lines
Lines consist of a positional vector, an optional parameter (it's ignored) and a directional vector.
* `(-2,1,3) + t * (1,2,3)`
* `<-2 1 3> 1 2 3`
* `-2 1 3 1 2 3`

The syntax is very forgiving.

### Planes
* `1x+2y+3z-4=0`
* `1 2 3 -4`

### PPlanes
* `(0,0,0) + t * (1,2,3) + s * <-2 9 17>`
* `0 0 0 1 2 3 -2 9 17`

### Commands
Commands consist of one of the above functions and then the arguments separated by comma.
One can nest functions by wrapping them in parenthesis: `dotp 1 2 3, (cross 1 2 -4, 0 4 1)`.

### Example commands

* Distance between two points: `distance (1,2,3), (0,-2,3)`
* Dot product of a point and the cross product of two other: `dotp 0 -2 4, (cross 0 4 1, [1 2 4])`
* Intersection of two planes: `intersection -2x+2y-3z+0=0, 4 -2 1 4`

## Binaries
It's a java jar file so you need to run it from the command prompt like so: `java -jar space-vectors.jar` (this is a nightmare on Windows, google it).

[space-vectors.jar](https://www.dropbox.com/s/mv1838sht00cs0f/space-vectors-0.2.0-standalone.jar)

I recommend wrapping the binary in [rlwrap](http://utopia.knoware.nl/~hlub/rlwrap/) on Mac and Linux to get history in the REPL.
I will be developing a browser-frontend later for easier use though.

## Thanks
Thanks to [Instaparse](https://github.com/Engelberg/instaparse) for the extensive documentation and the excellent library for parsing user input.

## License

Copyright © 2013 Andreas From

Distributed under the Eclipse Public License, the same as Clojure.
