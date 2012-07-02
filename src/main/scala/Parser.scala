package spjlang
import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  import Ast._

  lazy val program: Parser[CoreProgram] =
    repsep(sc, ";") ^^ (_.toStream)

  lazy val sc: Parser[CoreScDefn] =
    (var0 ~ rep(var0) <~ "=") ~ expr ^^ {
      case v~vars~exp => (v, vars.toStream, exp)
    }

  private def mkRightAssc[T](
    p: => Parser[Expr[T]],
    opName: String,
    op: Op
  ) = {
    p ~ rep(opName ~> p) ^^ {
      case exp~Stream() => exp
      case exp~exps =>
        exps.foldRight(exp)((x, y) => EBinOp(op, y, x))
    }
  }

  lazy val expr: Parser[CoreExpr] =
    "letrec" ~> (defns <~ "in") ~ expr ^^ {
      case defs~exp => ELet(true, defs, exp)
    } |
    "let" ~> (defns <~ "in") ~ expr ^^ {
      case defs~exp => ELet(false, defs, exp)
    } |
    "case" ~> (expr <~ "of") ~ alts ^^ {
      case exp ~ alts => ECase(exp, alts)
    } |
    ("\\" ~> rep(var0) <~ ".") ~ expr ^^ {
      case vars~exp => ELam(vars.toStream, exp)
    } |
    expr5

  lazy val expr5 =
    mkRightAssc(expr4, "|", "|")
    
  lazy val expr4: Parser[CoreExpr] =
    mkRightAssc(expr3, "&", "&")

  lazy val expr3: Parser[CoreExpr] =
    expr2 ~ opt(("==" | "~=" | ">" | ">=" | "<" | "<=") ~ expr2) ^^ {
      case exp~None => exp
      case exp~Some(op~exp1) =>
        EBinOp(op, exp, exp1)
    }

  lazy val expr2: Parser[CoreExpr] =
    expr1 ~ rep(("+" | "-") ~ expr1) ^^ {
      case exp~Stream() => exp
      case exp~exps =>
        exps.foldLeft(exp){ case (exp, op~exp1) => EBinOp(op, exp, exp1) }
    }

  lazy val expr1: Parser[CoreExpr] =
    expr0 ~ rep(("*" | "/") ~ expr0) ^^ {
      case exp~Stream() => exp
      case exp~exps =>
        exps.foldLeft(exp){ case (exp, op~exp1) => EBinOp(op, exp, exp1) }
    }
  
  lazy val expr0 =
    aexpr ~ rep(aexpr) ^^ {
      case exp~Stream() => exp
      case exp~exps => exps.foldLeft(exp)(EAp.apply)
    }
  
  lazy val aexpr =
    "Pack" ~> ("{" ~> num <~ ",") ~ num <~ "}" ^^ {
      case n1 ~ n2 => EConstr(n1, n2)
    } |
    var0.flatMap{
      case "in" => failure("`in` is a keyword.")
      case "of" => failure("`of` is a keyword.")
      case v => success(EVar(v))
    } |
    num ^^ ENum.apply |
    ("(" ~> expr <~ ")")
  
  lazy val defns: Parser[Stream[(Name, Expr[Name])]] =
    repsep(defn, ";") ^^ (_.toStream)

  lazy val defn: Parser[(Name, Expr[Name])] =
    (var0 <~ "=") ~ expr ^^ {
      case v~exp => (v, exp)
    }

  lazy val alts: Parser[Stream[CoreAlt]] =
    repsep(alt, ";") ^^ (_.toStream)

  lazy val alt: Parser[CoreAlt] =
    (("<" ~> num <~ ">") ~ rep(var0) <~ "->") ~ expr ^^ {
      case num~vars~exp => (num, vars.toStream, exp)
    }

  lazy val var0: Parser[String] =
    """[a-zA-Z][a-zA-Z_0-9]*""".r

  lazy val num: Parser[Int] =
    """[0-9]+""".r ^^ (_.toInt)
}
