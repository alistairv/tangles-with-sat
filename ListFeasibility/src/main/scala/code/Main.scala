package code

import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.{DimacsReader, ParseFormatException}
import org.sat4j.specs.{ContradictionException, IProblem, ISolver, TimeoutException}
import os.{Path, isFile, pwd, up, write}

import java.io.{BufferedReader, File, IOException, InputStreamReader, PrintStream}
import scala.annotation.tailrec

val pathToChaoticAttractorsRepo: Path = os.pwd/os.up/os.up/os.up/"chaotic-attractors"
val pathToGDPaperExamples:       Path = pathToChaoticAttractorsRepo/"examples"
val pathToPythonDrawingTool:     Path = pathToChaoticAttractorsRepo

val pathToScala:                 Path = os.pwd/"src"/"main"/"scala"
val pathToDiagramsFolder:        Path = pathToScala/"diagrams"
val pathToExamplesFolder:        Path = pathToScala/"examples"
val pathToOutputFolder:          Path = pathToScala/"output"


@main def main(): Unit =
  if !os.exists(pathToOutputFolder)   then os.makeDir(pathToOutputFolder)
  if !os.exists(pathToExamplesFolder) then os.makeDir(pathToExamplesFolder)
  if !os.exists(pathToDiagramsFolder) then os.makeDir(pathToDiagramsFolder)

  // L3 dummy to avoid initial performance drop for actual testing
  val dummy = generateDonkeyInstance(3)
  minimiseHeight(dummy, "dummy", dummy.m)



/**
 * Solves TangleHeightMinimisation iteratively. Begins by searching for a realisation
 * within height m. If feasible, checks the number of steps h' that were actually
 * used (i.e. removes empty steps) and continues by checking feasibility within
 * height h' - 1.
 * @param list swap list to check
 * @param name name of file
 * @param currOptHeight current minimal height
 * @return -1 if the list is unfeasible, or the minimal height otherwise
 */
@tailrec
def minimiseHeight(list: SwapList, name: String, currOptHeight: Int): Int =
  if currOptHeight == 0 then
    // either there are no swaps or they are all disjunct
    return math.min(1, list.m)

  if currOptHeight < list.mostSwapsPerWire() then
    return currOptHeight + 1

  // needed by testExplicitly only
  val start = System.currentTimeMillis()

  process(list, name, list.m - currOptHeight, false)
  val heightNeeded = solve(list, name, false)

  // needed by testExplicitly only
  val end = System.currentTimeMillis()
  os.write.append(pathToOutputFolder/s"output_$name.txt", s"Target height: $currOptHeight. Achieved height: $heightNeeded. Time needed: ${end-start}ms.\n")

  // unfeasible in height currOptHeight
  if heightNeeded == -1 then
    // either not realisable at all
    if currOptHeight == list.m then -1
    // or this height is simply not sufficient
    else currOptHeight + 1
  else
    minimiseHeight(list, name, heightNeeded - 1)

/**
 * Solves TangleHeightMinimisation by performing a binary search on the
 * height needed to perform all swaps. Range is 0 to m.
 * @param list swap list to check
 * @param name name of file
 * @param lower lower bound of range to search
 * @param upper upper bound of range to search
 * @param currOptHeight current minimal height
 * @return -1 if the list is unfeasible, or the minimal height otherwise
 */
@tailrec
def minimiseHeightBinary(list: SwapList, name: String, lower: Int, upper: Int, currOptHeight: Int): Int =
  if lower > upper then
    return currOptHeight

  val k = math.floor((lower + upper) / 2).toInt
  if k == 0 then
    // upper is either 0 or 1. If m == upper, then the minimum height is obviously that value:
    // we can always fit 0 swaps within height 0 and 1 swap within height 1.
    // Otherwise (m >= upper) this call is preceded by one with k' == upper + 1 and the swap list
    // is realisable within that k'.
    return math.min(upper + 1, list.m)

  // needed by testExplicitly only
  val start = System.currentTimeMillis()

  process(list, name, list.m - k, false)
  val heightNeeded = solve(list, name, false)

  // needed by testExplicitly only
  val end = System.currentTimeMillis()
  os.write.append(pathToOutputFolder/s"output_$name.txt", s"Target height: $k. Achieved height: $heightNeeded. Time needed: ${end-start}ms.\n")

  if heightNeeded == -1 then
    minimiseHeightBinary(list, name, k+1, upper, currOptHeight)
  else
    minimiseHeightBinary(list, name, lower, k-1, heightNeeded)

/**
 * Solves ListFeasibility by searching for a solution with exactly 1 swap per step. Formulates
 * problem as a SAT instance and applies the Sat4J solver. If feasible, also draws a swapping
 * diagram.
 * @param list swap list to check
 * @param name name of file
 * @return true if the list is feasible, and false otherwise
 */
def testFeasibility(list: SwapList, name: String): Boolean =
  process(list, name, 0, true)
  solve(list, name, true) != -1

/**
 * Generates the Gk swap list, defined as follows:
 * - wires: 0 until 2&#94;k
 * - if i | j != j then 2 swaps between i and j
 * - otherwise 0 swaps between i and j
 * @param k size parameter
 * @return a swap list for the instance
 */
def generateDonkeyInstance(k: Int): SwapList =
  val matrix =
    for
      i <- 0 until Math.pow(2,k).toInt
    yield for
      j <- 0 until Math.pow(2,k).toInt
    yield if i == j || List(i,j).contains(i | j) then 0 else 2

  SwapList(matrix)

/**
 * Computes an approximation of the performance variance of the iterative and the
 * binary search approaches by executing them on the same example JSON file
 * multiple times. Writes results to the output/output_variance_name.txt file.
 * @param repetitions size of the final data set on which the variance is computed
 * @param example an example from the GD paper repository used in the computation
 */
def computeVariance(repetitions: Int, example: String): Unit =
  val list = SwapList(Reader(pathToGDPaperExamples/(example + ".json")).read())
  computeVariance(repetitions, example, list)

/**
 * Overload of the method above with the same name and functionality. However, this
 * method is not restricted to examples from the GD paper: one can pass any swap list
 * and name to it, however those were generated.
 * @param repetitions size of the final data set on which the variance is computed
 * @param name name of the instance used in the computation. Can be chosen arbitrarily
 * @param swapList swap list representing the instance used in the computation
 */
def computeVariance(repetitions: Int, name: String, swapList: SwapList): Unit =
  os.write.over(pathToOutputFolder/s"output_variance_$name.txt", s"Computing variance of $name through $repetitions repetitions...\n" , createFolders = true)
  val times: Seq[(Long, Long)] = (0 until repetitions)
    .map { _ =>
      val start  = System.currentTimeMillis()
      val height = minimiseHeight(swapList, name, swapList.m)
      val end    = System.currentTimeMillis()

      val startBin  = System.currentTimeMillis()
      val heightBin = minimiseHeightBinary(swapList, name, swapList.mostSwapsPerWire(), swapList.m, -1)
      val endBin    = System.currentTimeMillis()

      (end-start, endBin-startBin)
    }
  val varianceIter = variance(times.map(_._1.toDouble))
  val varianceBin  = variance(times.map(_._2.toDouble))

  os.write.append(pathToOutputFolder/s"output_variance_$name.txt",
    s"Variance of the iterative approach was $varianceIter.\n" +
      s"The times were as follows: ${times.map(_._1.toString).mkString(", ")}\n" +
      s"Variance of the binary search approach was $varianceBin.\n" +
      s"The times were as follows: ${times.map(_._2.toString).mkString(", ")}\n"
  )

/**
 * Performs testing with more exact measurements on a list of examples.
 * In particular, tracks the target heights for each solve() call as well
 * as the time needed. Writes the results in output/output_example.txt file.
 */
def testExplicitly(): Unit =
  // add names of examples to be tested here
  Seq("extra_5x5_30-3", "extra_5x5_31-5")
    .sorted
    .foreach { example =>
      val path = pathToGDPaperExamples/(example + ".json")
      val swapList = SwapList(Reader(path).read())
      os.write.over(pathToOutputFolder/s"output_$example.txt", s"Testing $example iteratively...\n" , createFolders = true)
      minimiseHeight(swapList, example, swapList.m)
      os.write.append(pathToOutputFolder/s"output_$example.txt", s"Testing $example through binary search...\n")
      minimiseHeightBinary(swapList, example, swapList.mostSwapsPerWire(), swapList.m, -1)
    }

/**
 * Iterates all examples from the GD paper of Olszewski et al. and minimises their height.
 * Writes to a CSV document located in src/main/scala/output a table with the name, number
 * of swaps, minimal height and computation time for each example. If an example is not
 * feasible, -1 is written instead in the "Height" column. Does *not* draw the swapping
 * diagrams as only performance should be tested.
 */
def iterateAllExamples(): Unit =
  val startIndex = pathToGDPaperExamples.toString.length + 1

   val examples: Seq[(String, SwapList)] = os.walk(pathToGDPaperExamples)
     .filter  { e => os.isFile(e)}
     .map     { e =>
       val name = e.toString
       ( name.slice(startIndex, name.length - 5).replaceAll("/","_"), SwapList(Reader(e).read()) )
     }

   Seq("5x5", "6x6", "7x7", "extra_5x5", "extra_6x6", "extra_7x7", "too_slow").foreach { prefix =>
     println(s"Starting group $prefix...")
     os.write.over(pathToOutputFolder/s"output_$prefix.csv", "Instance,Swaps,Height,TimeIter,TimeBin\n" , createFolders = true)
     examples.filter { (name,list) => name.startsWith(prefix) }
       .sorted(Ordering.by[(String, SwapList), String](_._1))
       .foreach { (name,list) =>

         println(s"Testing $name iteratively...")
         val start  = System.currentTimeMillis()
         val height = minimiseHeight(list, name, list.m)
         val end    = System.currentTimeMillis()

         println(s"Testing $name through binary search...")
         val startBin  = System.currentTimeMillis()
         val heightBin = minimiseHeightBinary(list, name, list.mostSwapsPerWire(), list.m, -1)
         val endBin    = System.currentTimeMillis()

         if height != heightBin then println(s"Discrepancy found! Iterative height was $height while binary height was $heightBin")
         os.write.append(pathToOutputFolder/s"output_$prefix.csv", s"$name,${list.m},$height,${end-start},${endBin-startBin}\n")
       }
   }

/**
 * Takes a swap list and produces the SAT formulation as a DIMACS file. The created DIMACS file can
 * be passed to any SAT solver accepting that format.
 * @param list swap list to process
 * @param name name of the file where the formulation should be written to. File does not need to exist beforehand
 * @param offset offset for maximum number of steps. Produced formulation should be solvable within height m - offset
 * @param deciding true only if the exactly-m-steps formulation should be used (offset = 0). False if the at-most-m-steps formulation should be used (offset >= 0)
 */
def process(list: SwapList, name: String, offset: Int, deciding: Boolean): Unit =
  println(s"Creating SAT instance for height ${list.m - offset}...")
  val C: Combinator = Combinator(list, offset)
  val W: Writer = if deciding then FeasibilityWriter(C) else MinimisationWriter(C)
  os.write.over(pathToExamplesFolder/s"$name.dimacs", W.mkString, createFolders = true)

/**
   * Uses the Sat4J library tp check the satisfiability of a swap list processed as a
   * SAT instance (a DIMACS file). Optionally draws the resulting swapping diagram.
   * @param list an input swap list
   * @param name name of the DIMACS file produced by process()
   * @param draw if true, a realisation of the given swap list will be drown if any is found.
   * @return -1 if the instance is not satisfiable/parsable, or the height needed otherwise
   */
def solve(list: SwapList, name: String, draw: Boolean): Int =
  val solver: ISolver = SolverFactory.newDefault
  solver.setTimeout(86400)
  val reader: org.sat4j.reader.Reader = new org.sat4j.reader.DimacsReader(solver)

  try
    val pathOfProblem = pathToExamplesFolder/s"$name.dimacs"
    val problem: IProblem = reader.parseInstance(pathOfProblem.toString)
    println("Solving...")

    if problem.isSatisfiable() then

      val mod = problem.model().toList
      val swapIndices = mod
        .filter  { z => z > 0}
        .map     { z => Variable.fromRepr(z, list.n, list.m) }
        .collect { case YVariable(swapIndex, r) => YVariable(swapIndex, r) }
        .groupBy { _.r }
        .map     { case (r, ly) => (r, ly.map { y => list.asSeq(y.swapIndex) }) }
        // Uncomment the following lines to avoid drawings with empty steps:
        // .toSeq
        // .sortBy  { _._1 }
        // .map     { _._2 }
        // .zipWithIndex
        // .map     { case (a,b) => (b,a) }
        // .toMap

      println(s"Satisfiable! Found a realisation within height ${swapIndices.size}.")

      if draw then drawSwappingDiagram(
        swapIndices,
        list,
        pathToPythonDrawingTool.toString,
        s"${name}_vis.svg")
      return swapIndices.size

    else
      println("Unsatisfiable!")
      return -1
  catch
    case e: ParseFormatException   => println("Format could not be parsed!")
    case e: IOException            => println("Could not find file!")
    case e: ContradictionException => println("Unsatisfiable (trivial)!")
    case e: TimeoutException       => println("Timeout, sorry!")
    -1

/**
 * Draws a swapping diagram using svgExporter.py.
 * @param swaps map containing the swaps in each step, e.g., 1 -> [(1,2),(3,4)], 2 -> [(1,4)]
 * @param swapList a swap list whose realisation was computed
 * @param pathToPython path to svgExporter.py
 * @param name name of the SVG file where the diagram should be written to
 */
def drawSwappingDiagram(swaps: Map[Int,List[(Int,Int)]], swapList: SwapList, pathToPython: String, name: String): Unit =
  val pathToWrite = pathToDiagramsFolder / name
  val max = swaps.keys.max + 1

  val targetFile = new File(pathToWrite.toString)
  targetFile.getParentFile.mkdirs()
  if !targetFile.exists() then
    targetFile.createNewFile()

  val LPythonParam =
    "[" +
    swapList.matrix
      .map { inner => "[" + inner.mkString(",") + "]" }
      .mkString(",") +
    "]"

  val solutionPythonParam =
    s"{'h':$max,'s':[" +
      (0 until max)
        .map { i => s"[${ if swaps contains i then swaps(i).map { s => s"[${s._1},${s._2}]" }.mkString(",") else "" }]" }
        .mkString(",") +
      s"],'p':[${ (0 until swapList.n).mkString(",") }]}"

  val p = Runtime.getRuntime.exec(s"python " +
    s"svgExporter.py " +
    s"$pathToWrite " +
    s"${swapList.n} " +
    s"$max " +
    s"$LPythonParam " +
    s"$solutionPythonParam",
    null,
    new File(s"$pathToPython${File.separator}"))
  p.waitFor()

  val error = new BufferedReader(new InputStreamReader(p.getErrorStream))
  error.lines().forEach(line => println(line))
  error.close()

  val input = new BufferedReader(new InputStreamReader(p.getInputStream))
  input.lines().forEach(line => println(line))
  input.close()

  val outputStream = p.getOutputStream
  val printStream = new PrintStream(outputStream)
  printStream.println()
  printStream.flush()
  printStream.close()
  println("Diagram created.")

/**
 * Computes the variance of a list of values. It is recommended (but not necessary)
 * to cast the values to Double (.map(_.toDouble)) before calling.
 * @param values data set
 * @return the variance of the data set
 */
def variance(values: Seq[Double]): Double =
  val n    = values.size
  val mean = values.sum / n
  values.map { v => math.pow(v - mean, 2) }.sum / n - 1