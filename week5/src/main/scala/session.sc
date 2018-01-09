def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error
  case List(_) => Nil
  case y :: ys => y :: init(ys)
}

init(List(1, 2, 3, 4, 5))

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

concat(List(1, 2, 3), List(4, 5, 6))

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => Nil
  case y :: ys => reverse(ys) ++ List(y)
}

reverse(List(1, 2, 3, 4))

def removeAt[T](n: Int, xs: List[T]) = xs.take(n) ++ xs.drop(n + 1)

removeAt(1, List(1, 2, 3, 4))

def mergeSort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) if (x < y) => x :: merge(xs1, ys)
      case (x :: xs1, y :: ys1) => y :: merge(xs, ys1)
    }

    val (first, second) = xs splitAt(n)
    merge(mergeSort(first), mergeSort(second))
  }
}

mergeSort(List(5, 2, 8, -1))

def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x * x)

squareList(List(2, 4))

squareListMap((List(2, 4)))

val data = List("a", "a", "a", "b", "c", "c", "a");

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => xs.takeWhile(y => x == y) :: pack(xs.dropWhile(y => x == y))
//  case x :: xs1 =>
//    val (first, rest) = xs span(y => x == y)
//    first :: pack(rest)
}

pack(data)

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(a => (a.head, a.length))

encode(data)

// ReduceLeft
def sum1(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
def product1(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

// FoldLeft
def sum2(xs: List[Int]) = (xs foldLeft 0) (_ + _)
def product2(xs: List[Int]) = (xs foldLeft 1) (_ * _)


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, xs) => f(x) :: xs )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_, count) => count + 1 )

mapFun(data, (x: String) => x.length)

lengthFun(data)

