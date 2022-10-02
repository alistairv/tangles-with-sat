package code

/**
 * This class serves the purpose of writing the results produced in a 
 * Combinator instance to a string in the DIMACS format. The produced
 * string can then be written on/appended to any file.
 * @param c a combinator instance
 */
sealed trait Writer(c: Combinator):
  def mkString: String
  
  def compose(clauses: Seq[Seq[Int]]): String =
    val clausesStr: Seq[String] =
      for
        clause <- clauses
      yield clause.appended(0).mkString(" ")
    s"p cnf ${c.nbvar} ${clauses.length}\n" + clausesStr.mkString("\n")

final case class FeasibilityWriter(c: Combinator) extends Writer(c):
  def mkString: String = compose(c.combineForFeasibility)

object FeasibilityWriter:
  def apply(c: Combinator): FeasibilityWriter = new FeasibilityWriter(c)

final case class MinimisationWriter(c: Combinator) extends Writer(c):
  def mkString: String = compose(c.combineForMinimisation)

object MinimisationWriter:
  def apply(c: Combinator): MinimisationWriter = new MinimisationWriter(c)