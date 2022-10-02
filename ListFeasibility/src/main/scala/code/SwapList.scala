package code

import scala.language.postfixOps

/**
 * A case class (similar to a record) that holds a swap list along with the number of wires
 * and its length for fast access. Can be conveniently created by passing a square (n x n)
 * 2D sequence with the entries, in a row-major order. Negative entries are set to their
 * absolute value. The created representation is guaranteed to have zeroed diagonals.
 * 
 * The swap list can be queried in a sequence representation through the asSeq() function.
 * It returns a sequence of tuples, with a tuple holding the indices of the two wires for 
 * each swap. The returned result is lexicographically sorted by the first and then the
 * second wire index. Note that this function implicitly assumes a symmetric swap list.
 * @param matrix a 2D sequence representing the swap list
 * @param n the number of wires. Must be equivalent to the number of rows and columns of the matrix
 * @param m the total number of swaps
 */
case class SwapList(matrix: Seq[Seq[Int]], n: Int, m: Int):
  def asSeq: Seq[(Int, Int)] =
    for
      i <- 0   until n
      j <- i+1 until n
      _ <- 0   until matrix(i)(j)
    yield (i, j)

  def mostSwapsPerWire(): Int = matrix.map { _.sum }.max

object SwapList:
  def apply(matrix: Seq[Seq[Int]]): SwapList =
    val modified =
      for
        outer <- matrix.zipWithIndex
      yield for
        inner <- outer._1.zipWithIndex
      yield if outer._2 == inner._2 then 0 else inner._1.abs

    val n: Int = modified.length

    val m: Int = modified.foldLeft[Int](0) { (acc, el) => acc + el.sum } / 2

    new SwapList(modified, n, m)
