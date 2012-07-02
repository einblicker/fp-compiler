package spjlang

import org.scalatest.FunSuite

object ParserTest extends FunSuite {
  import Parser._

  def test = {
    println(parse(program, """f x = x + (negate x)"""))
    println(parse(program, """x y = let z = 123 in z; a = foo"""))
    println(parse(program, """isRed c = case c of
      <1> -> True ;
      <2> -> False ;
      <3> -> False"""))
    val prelude = """
I x = x ;
K x y = x ;
K1 x y = y ;
S f g x = f x (g x) ;
compose f g x = f (g x) ;
twice f = compose f f
"""
    println(parse(program, prelude))
  }
}
