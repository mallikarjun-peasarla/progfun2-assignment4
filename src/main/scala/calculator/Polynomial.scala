package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  // -b + rootOf delta / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val dval = delta()
    val aval = a()
    val bval = b()

    Signal(
      if(dval < 0) Set[Double]()
      else Set((-bval+sqrt(dval))/2*aval, (-bval-sqrt(dval))/2*aval)
    )
  }
}
