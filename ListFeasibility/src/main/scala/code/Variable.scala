package code

sealed trait Variable:
  def toRepr(n: Int, m: Int): Int

object Variable:
  def fromRepr(z: Int, n: Int, m: Int): Variable =
    val z2 = z.abs - 1
    if z2 >= (m+1)*n*n then
      val rest = z2 - (m+1)*n*n
      YVariable(rest % m, rest / m)
    else
      val r: Int     = z2 / (n*n)
      val rest2: Int = z2 % (n*n)
      val j: Int     = rest2 / n
      val rest1: Int = rest2 % n
      val i: Int = rest1
      XVariable(i, j, r)

final case class XVariable(i: Int, j: Int, r: Int) extends Variable:
  override def toRepr(n: Int, m: Int): Int = 1 + i + j*n + r*n*n

  override def toString: String = s"x_{$i,$j}^{$r}"

object XVariable:
  def apply(i: Int, j: Int, r: Int): XVariable = new XVariable(i,j,r)


final case class YVariable(swapIndex: Int, r: Int) extends Variable:
  override def toRepr(n: Int, m: Int): Int = (m+1)*n*n + 1 + swapIndex + r*m

  override def toString: String = s"y_{$swapIndex}^{$r}"

object YVariable:
  def apply(swapIndex: Int, r: Int): YVariable = new YVariable(swapIndex,r)
