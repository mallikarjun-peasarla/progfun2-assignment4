package calculator

import scala.math

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  // -b + rootOf delta / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      if(delta() < 0.0) Set[Double]()
      else Set((-b()+math.sqrt(delta()))/(2*a()), (-b()-math.sqrt(delta()))/(2*a()))
    )
  }
}
