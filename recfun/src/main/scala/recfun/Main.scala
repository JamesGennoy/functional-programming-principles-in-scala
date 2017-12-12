package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def pascalItr(c: Int, r: Int): Int = {
        if (r == 0 || c == 0 || c == r) {
          1
        } else {
          pascalItr(c - 1, r - 1) + pascalItr(c, r - 1)
        }
      }

      pascalItr(c, r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def leftChar(char: Char, count: Int) = if (char == '(') count + 1 else count
      def rightChar(char: Char, count: Int) = if (char == ')') count + 1 else count

      def balanceItr(chars: List[Char], left: Int, right: Int): Boolean = {
        if (chars.isEmpty) {
          left == right
        }
        else if (right > left) {
          false
        }
        else {
          balanceItr(chars.tail, leftChar(chars.head, left), rightChar(chars.head, right))
        }
      }

      balanceItr(chars, 0, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def changeItr(money: Int, coins: List[Int]): Int = {
        if (money < 0 || coins.isEmpty) {
          0
        }
        else if (money == 0)
          1
        else {
          changeItr(money, coins.tail) + changeItr(money - coins.head, coins)
        }
      }

      changeItr(money, coins)
    }
  }
