package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    
    val evaluation = (name:String) => eval(getReferenceExpr(name, namedExpressions), namedExpressions)
    
    val mapper = (name: String, expr: Signal[Expr]) => (name, Signal(evaluation(name)))
    
    namedExpressions map {
      		case (name: String, expr: Signal[Expr])  => if (!cyclicDependencies(expr(), namedExpressions, name)) mapper(name, expr)
      																								else (name, Signal(Double.NaN))
      }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v:Double) => v
    case Ref(name: String) => eval(getReferenceExpr(name,references), references)
    case Plus(a: Expr, b: Expr) => eval(a, references) + eval(b, references)
    case Minus(a: Expr, b: Expr) => eval(a, references) - eval(b, references)
    case Times(a: Expr, b: Expr) => eval(a, references) * eval(b, references)
    case Divide(a: Expr, b: Expr) => eval(a, references) / eval(b, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
  
  def cyclicDependencies(expr: Expr, references: Map[String, Signal[Expr]], origName:String): Boolean = { 
    def recurse (expr: Expr, references: Map[String, Signal[Expr]], origName:String): Boolean = expr match {
	    case Literal(v:Double) => false
	    case Ref(name: String) => if (name==origName) true else false
	    case Plus(a: Expr, b: Expr) => recurse(a, references, origName) || recurse(b, references, origName)
	    case Minus(a: Expr, b: Expr) => recurse(a, references, origName) || recurse(b, references, origName)
	    case Times(a: Expr, b: Expr) => recurse(a, references, origName) || recurse(b, references, origName)
	    case Divide(a: Expr, b: Expr) => recurse(a, references, origName) || recurse(b, references, origName)
	  }
    recurse(expr, references, origName)
  }
    
}
