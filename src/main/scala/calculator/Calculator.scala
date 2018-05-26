package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case(name, sig) => (name, Signal(eval(sig(), namedExpressions)))}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case expr:Ref =>
        val nexpr = references.find { case (name, _) => name == expr.name }
          .map { case (_, expSig) => expSig() }.getOrElse(Literal(Double.NaN))
        eval(nexpr, references)
      case expr: Plus => eval(expr.a, references) + eval(expr.b, references)
      case expr: Minus => eval(expr.a, references) - eval(expr.b, references)
      case expr: Times => eval(expr.a, references) * eval(expr.b, references)
      case expr: Divide => eval(expr.a, references) / eval(expr.b, references)
      case expr: Literal => expr.v
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
