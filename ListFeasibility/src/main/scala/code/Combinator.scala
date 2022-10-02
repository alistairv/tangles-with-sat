package code

/**
 * This class creates DIMACS clauses representing the passed swap list. There
 * is a dedicated method for each of the equations presented in the practical
 * course report. The results of those methods are combined by the combine()
 * function.
 * 
 * The result is a 2D sequence of integers. Each inner sequence corresponds to
 * one clause in DIMACS format. The number of clauses and variables created can
 * be queried through nbclauses() and nbvar(), respectively. The latter is not
 * necessarily exact but is still a tight upper bound, as discussed in our report.
 * @param L the swap list that must be represented as a SAT instance
 */
class Combinator(L: SwapList, offset: Int = 0):
  // Seq[Seq] since each inner Seq is a single clause
  // each Equation returns a Seq of the clauses it has produced

  // maybe only one iteration due to transitivity?
  def init: Seq[Seq[Int]] =
    for
      i <-   0 until L.n
      j <- i+1 until L.n
    yield Seq(XVariable(i,j,0).toRepr(L.n,L.m))

  def eq2: Seq[Seq[Int]] =
    for
      r <- 0 until L.m+1 - offset
      i <- 0 until L.n
      j <- 0 until L.n
      k <- 0 until L.n
      if i != j && i != k && j != k
    yield Seq(-XVariable(i,j,r).toRepr(L.n,L.m), -XVariable(j,k,r).toRepr(L.n,L.m), XVariable(i,k,r).toRepr(L.n,L.m))

  def eq3: Seq[Seq[Int]] =
    for
      r <- 0 until L.m+1 - offset
      i <- 0 until L.n
      j <- 0 until L.n
      if i != j
      e <- Seq(
        Seq( XVariable(i,j,r).toRepr(L.n,L.m),  XVariable(j,i,r).toRepr(L.n,L.m)),
        Seq(-XVariable(i,j,r).toRepr(L.n,L.m), -XVariable(j,i,r).toRepr(L.n,L.m))
      )
    yield e

  def eq6: Seq[Seq[Int]] =
    for
      si <- L.asSeq.indices
      r1 <-    0 until L.m  - offset
      r2 <- r1+1 until L.m  - offset
    yield Seq(-YVariable(si,r1).toRepr(L.n,L.m), -YVariable(si,r2).toRepr(L.n,L.m))

  def eq7: Seq[Seq[Int]] =
    for
      si <- L.asSeq.indices
    yield for
      r  <- 0 until L.m  - offset
    yield YVariable(si, r).toRepr(L.n, L.m)

  def eq8: Seq[Seq[Int]] =
    for
      r  <- 0 until L.m  - offset
    yield for
      si <- L.asSeq.indices
    yield YVariable(si, r).toRepr(L.n, L.m)

  def eq9: Seq[Seq[Int]] =
    for
      (s,i) <- L.asSeq.zipWithIndex
      r     <- 0 until L.m  - offset
      e     <- Seq(
        Seq(-YVariable(i,r).toRepr(L.n,L.m), -XVariable(s._1,s._2,r).toRepr(L.n,L.m), -XVariable(s._1,s._2,r+1).toRepr(L.n,L.m)),
        Seq(-YVariable(i,r).toRepr(L.n,L.m),  XVariable(s._1,s._2,r).toRepr(L.n,L.m),  XVariable(s._1,s._2,r+1).toRepr(L.n,L.m))
      )
    yield e

  def eq10: Seq[Seq[Int]] =
    val seq = L.asSeq.zipWithIndex
    for
      r <- 0 until L.m  - offset
      i <- 0 until L.n
      j <- i+1 until L.n
      swaps <- Seq(
        seq
          .filter { (s,si) => Set(s._1,s._2) == Set(i,j) }
          .map    { (s,si) => YVariable(si,r).toRepr(L.n,L.m) }
      )
      e <- Seq(
        swaps prepended  XVariable(i,j,r+1).toRepr(L.n,L.m) prepended -XVariable(i,j,r).toRepr(L.n,L.m),
        swaps prepended -XVariable(i,j,r+1).toRepr(L.n,L.m) prepended  XVariable(i,j,r).toRepr(L.n,L.m)
      )
    yield e

  def eq13: Seq[Seq[Int]] =
    for
      r       <- 0 until L.m  - offset
      (s1,i1) <- L.asSeq.zipWithIndex
      (_,i2)  <- L.asSeq.zipWithIndex.filter { (s2,i2) => s2._1 == s1._1 || s2._1 == s1._2 || s2._2 == s1._1 || s2._2 == s1._2 }
      if i1 != i2
    yield Seq(-YVariable(i1,r).toRepr(L.n,L.m), -YVariable(i2,r).toRepr(L.n,L.m))

  def combineForFeasibility: Seq[Seq[Int]] =
    (init ++ eq2 ++ eq3 ++ eq6 ++ eq7 ++ eq8 ++ eq9 ++ eq10).distinct.filter { c => c.nonEmpty }

  def combineForMinimisation: Seq[Seq[Int]] =
    (init ++ eq2 ++ eq3 ++ eq6 ++ eq7 ++        eq9 ++ eq10 ++ eq13).distinct.filter { c => c.nonEmpty }

  def nbvar: Int =
    val m: Int = L.m
    val n: Int = L.n
    (m+1)*n*n + m*m

object Combinator:
  def apply(L: SwapList, offset: Int = 0): Combinator = new Combinator(L, offset)
