val a = List(List(1, 2, 3), List(3, 4), List(5, 6))

val groupByLength = a.groupBy(x => x.length)

groupByLength.getOrElse(2, List())

"aaccd".groupBy(x => x)

val xs = Array(1, 2, 3, 4)
xs.map(x => x * 2)

val s = "Hello World"
s.filter(c => c.isUpper)
s.exists(c => c.isUpper)
s.forall(c => c.isUpper)

val pairs = List(1, 2, 3).zip(s)
pairs.unzip

xs.sum
xs.max

def isPrime(n: Int): Boolean = (2 until n).forall(x => n % x != 0)

isPrime(53)
isPrime(7)
isPrime(45)

def findPairs(n: Int): Any = (1 until n).flatMap(i => (1 until i).map(j => (i, j)))
  .filter({ case (i, j) => isPrime(i + j) }).toList

def findPairsFor(n: Int): Any = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

findPairs(7)
findPairsFor(7)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).product

scalarProduct(List(1, 2), List(1, 2))

