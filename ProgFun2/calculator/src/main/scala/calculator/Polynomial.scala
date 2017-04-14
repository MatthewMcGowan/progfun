package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val delta = computeDelta(a, b, c)

    def solutions(a: Signal[Double], b: Signal[Double], delta: Signal[Double]) = {
      val add = (x: Double, y:Double) => x + y
      val sub = (x: Double, y:Double) => x - y

      def sol(fn: (Double, Double) => Double) = fn(-b(), math.sqrt(delta())) / (2 * a())

      if(delta() < 0) Set[Double]()
      else Set(sol(add), sol(sub))
    }

    Signal(solutions(a, b, delta))
  }
}
