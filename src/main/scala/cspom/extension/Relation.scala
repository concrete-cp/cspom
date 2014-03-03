package cspom.extension

trait Relation extends Iterable[Seq[Int]] {

  /**
   * This method returns a copy of this extension with permuted tuples. New
   * order of tuples is given as an argument.
   *
   * <p>
   * For example, a ternary extension 1 2 3|1 3 4|2 4 5 will be reversed to 1
   * 3 2|1 4 3|2 5 4 by a call to reverse(0, 2, 1).
   * </p>
   *
   * @param newOrder
   *            new order of the extension.
   * @return a reversed copy of the extension.
   */
  //def permute(newOrder: Seq[Int]) = HashTrie(toList map { t => newOrder.map(t(_)).toArray }: _*)

  def tupleString = map { _.mkString(" ") } mkString "|"

  def contains(t: Seq[Int]): Boolean

  def arity: Int

  def filter(f: (Int, Int) => Boolean): Relation
  //def close()
  
  def project(c: Seq[Int]): Relation

}