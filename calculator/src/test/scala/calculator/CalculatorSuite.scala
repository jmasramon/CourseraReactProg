package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }
  
  test("cyclicDependencies") {
    var namedExpressions: Map[String, Signal[Expr]] = Map(("a",Signal(Literal(3))), ("b",Signal(Literal(2))), ("c",Signal(Plus(Ref("a"),Ref("b")))))
    assert(Calculator.cyclicDependencies(Plus(Ref("a"),Ref("b")), namedExpressions, "c") == false)
    
    namedExpressions = Map(("a",Signal(Ref("a"))))
    assert(Calculator.cyclicDependencies(Ref("a"), namedExpressions, "a") == true)
    
    namedExpressions = Map(("a",Signal(Literal(3))), ("b",Signal(Literal(2))), ("c",Signal(Plus(Ref("a"),Ref("b")))), ("d", Signal(Plus(Literal(1), Ref("d"))) ))
    assert(Calculator.cyclicDependencies(Plus(Literal(1), Ref("d")), namedExpressions, "d") == true)
    
  }
  
  test("if cliclycDependencies, no evaluation should be performed") {
    var namedExpressions: Map[String, Signal[Expr]] =  Map(("a",Signal(Ref("a"))))
    val res: Double = Calculator.computeValues(namedExpressions).get("a").fold(0.0){value => value()}
    assert(res.isNaN)
  }
  
  test("if cyclicDependencies in some exp it should return NaN") {
    var namedExpressions: Map[String, Signal[Expr]]  = Map(("a",Signal(Literal(3))), ("b",Signal(Literal(2))), ("c",Signal(Plus(Ref("a"),Ref("b")))), ("d", Signal(Plus(Literal(1), Ref("d"))) ))
    assert(Calculator.cyclicDependencies(Plus(Literal(1), Ref("d")), namedExpressions, "d") == true)
    val res: Map[String, Signal[Double]] = Calculator.computeValues(namedExpressions)
    
    assert(res.get("a").fold(0.0){value => value()} == 3.0)
    assert(res.get("b").fold(0.0){value => value()} == 2.0)
    assert(res.get("c").fold(0.0){value => value()} == 5.0)
    assert(res.get("d").fold(0.0){value => value()}.isNaN)
  }
    

}
