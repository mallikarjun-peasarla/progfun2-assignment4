package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b.apply()*b.apply() - 4*a.apply()*c.apply())
  }

  // -b + rootOf delta / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val dval = delta.apply()
    val aval = a.apply()
    val bval = b.apply()

    val solutions =
      if(dval < 0) Set[Double]()
      else Set((-bval+sqrt(dval))/2*aval, (-bval-sqrt(dval))/2*aval)
    Signal(solutions)
  }
}
