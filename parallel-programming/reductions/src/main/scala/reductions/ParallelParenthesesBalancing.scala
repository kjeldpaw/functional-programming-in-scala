package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

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
    def balanceIter(c: Int, chars: Array[Char]): Boolean = {
      if (chars.isEmpty) {
        c == 0
      } else if (c < 0) {
        false
      } else if (chars.head == '(') {
        balanceIter(c + 1, chars.tail)
      } else if (chars.head == ')') {
        balanceIter(c - 1, chars.tail)
      } else {
        balanceIter(c, chars.tail)
      }
    }

    balanceIter(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      def traverseIter(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
        if(idx == until) (arg1, arg2)
        else {
          if (chars(idx) == '(')  {
            traverseIter(idx + 1, until, arg1 + 1, arg2)
          } else if (chars(idx) == ')') {
            if(arg1 > 0) traverseIter(idx + 1, until, arg1 - 1, arg2)
            else traverseIter(idx + 1, until, arg1, arg2 + 1)
          } else {
            traverseIter(idx + 1, until, arg1, arg2)
          }
        }
      }
      traverseIter(idx, until, 0, 0)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val middle = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, middle), reduce(middle, until))
        (left._1 + right._1 - Math.min(left._1, right._2), left._2 + right._2 - Math.min(left._1, right._2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
