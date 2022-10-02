# Computing Tangles Using a SAT Solver

---

Code accompanying the practical course report of the same name by Vasil Alistarov, 
supervised by Prof. Dr. Alexander Wolff and Johannes Zink, MSc.

## Prerequisites

This project is written in Scala 3. Required are therefore the following packages:

* [`sbt`](https://www.scala-sbt.org/1.x/docs/Setup.html) v1.5.5 or above, the Scala build tools
* [JDK](https://www.oracle.com/java/technologies/downloads/) v11.0 or above

Eventually, one may also want to install the Scala compiler `scalac` v3.1.1 or above.

## Building the project

This is a normal `sbt` project. You can compile code with `sbt compile`, run it with `sbt run`, 
and `sbt console` will start a Scala 3 REPL. The required libraries are provided in the `build.sbt`
file and are automatically downloaded upon compilation.

## Usage

All methods serving as an interface for the user, along with a main method, are provided in the file
`Main.scala`.

* `generateDonkeyInstance(k: Int)` will create a swap list representing donkey's construction for 2^k
many wires and multiplicity of 2.
* `iterateAllExamples` creates swap lists for all examples from the paper of Olszowski et al. about the
visualisation of chaotic attractors.
* `process` will produce, from a swap list, a DIMACS file in src/main/scala/examples with the provided 
file name. The "offset" argument indicates the offset from _m_; the DIMACS file will represent a query
of whether or not a realisation with height at most _m-offset_ can be found. Only if the offset is 0 may
the "deciding" parameter be set to `true`, doing so will produce a DIMACS file regarding a realisation
of a tangle with height _exactly m_. The DIMACS file can be used as input for any SAT solver.
* `testFeasibility` checks whether the given swap list is feasible at all through the Sat4j solver. In
particular, it attemtps to find a realisation with height exactly _m_ and, if such is found, draws it
as a SVG file in src/main/scala/diagrams folder.
* `minimiseHeight` computes the minimal height of any realisation of the given swap list and also draws
that realisation. If the swap list is not feasible, -1 is returned.
* `minimiseHeightBinary` serves the same purpose as the `minimiseHeight` method, but uses binary search
on the realisation's height instead of an iterative approach.
* `solve` applies the Sat4j solver on a swap list and its representation as a DIMACS file.

Some of those, such as `iterateAllExamples`, require the [Chaotic Attractors repository](https://github.com/PhKindermann/chaotic-attractors). By default it is expected that it is cloned next to the praktikum-alistarov repository; however, a user can change that by modifying the path constants set in the beginning of `Main.scala`.
