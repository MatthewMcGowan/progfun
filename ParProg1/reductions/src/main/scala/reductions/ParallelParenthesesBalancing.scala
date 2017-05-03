package reductions

import scala.annotation.tailrec
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceAccum(chars: Array[Char], openParens: Int): Boolean =
    {
      if (openParens < 0) false
      else if (chars.isEmpty) openParens == 0
      else if (chars.head == ')') balanceAccum(chars.tail, openParens - 1)
      else if (chars.head == '(') balanceAccum(chars.tail, openParens + 1)
      else balanceAccum(chars.tail, openParens)
    }

    balanceAccum(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  //TODO: Clear up this mess...
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec
    def traverse(idx: Int, until: Int, openParens: Int, closeParens: Int): (Int, Int) /*: ???*/ = {
      if (idx == until) (openParens, closeParens)
      else if (chars(idx) == ')') {
        if (openParens > 0) traverse(idx + 1, until, openParens - 1, closeParens)
        else traverse(idx + 1, until, openParens, closeParens + 1)
      }
      else if (chars(idx) == '(') traverse(idx + 1, until, openParens + 1, closeParens)
      else traverse(idx + 1, until, openParens, closeParens)
    }

    def reduce(from: Int, until: Int): (Int, Int)/*: ???*/ = {
      val interval = until - from
      val halfInterval = interval / 2

      def parrallelise(interval: Int): (Int, Int) = {
        val (a, b) = parallel(
          reduce(from, from + halfInterval),
          reduce(until - halfInterval, until))

        val cancellingParens = math.min(a._1, b._2)
        val open = b._1 + (a._1 - cancellingParens)
        val closed = a._2 + (b._2 - cancellingParens)

        (open, closed)
      }

      if (interval <= threshold) traverse(from, until, 0, 0)
      else parrallelise(interval)
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
