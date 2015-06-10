package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal(b.apply*b.apply - 4*a.apply*c.apply)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta.apply < 0 ) new Signal(Set(0.0))
    else new Signal(Set((-b.apply+Math.sqrt(delta.apply))/(2*a.apply), (-b.apply-Math.sqrt(delta.apply))/(2*a.apply)))
  }
}
