abstract class IntSet {
  def contains(x: Int): Boolean

  def add(x: Int): IntSet

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def add(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left add x, right)
    else if (x > elem) new NonEmpty(elem, left, right add x)
    else this
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) add elem // this seems like magic to me
  }

  override def toString = "{" + left + elem + right + "}"
}

val a = new NonEmpty(3, Empty, Empty) add 3 add 1 add 5
val b = new NonEmpty(4, Empty, Empty) add 4 add 2 add 6
a union b

def get[T](n: Int, list: List[T]): T = {
  if (n < 0 || n > list.length) throw new IndexOutOfBoundsException
  if (n == 0) list.head
  else get(n - 1, list.tail)
}

get(1, List("a", "b"))
get(0, List("a", "b"))
get(3, List("a", "b"))
