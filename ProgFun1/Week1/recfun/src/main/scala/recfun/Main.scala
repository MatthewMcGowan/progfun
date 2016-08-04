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

    def factorial(n: Int): Int = {
      def loop(acc: Int, n: Int): Int = {
        if (n == 0) acc else loop(acc * n, n - 1)
      }
      loop(1, n)
    }

    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def isBalanced(chars: List[Char], openParens: Int): Boolean = {
      if (openParens < 0) false
      else if (chars.isEmpty) openParens == 0
      else if (chars.head == ')') isBalanced(chars.tail, openParens - 1)
      else if (chars.head == '(') isBalanced(chars.tail, openParens + 1)
      else isBalanced(chars.tail, openParens)
    }

    isBalanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0)  0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
