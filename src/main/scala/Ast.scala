package spjlang

object Ast {
  type CoreExpr = Expr[Name]
  type Name = String
  type IsRec = Boolean
  type Alter[T] = (Int, Stream[T], Expr[T])
  type CoreAlt = Alter[Name]
  type Program[T] = Stream[ScDefn[T]]
  type CoreProgram = Program[Name]
  type ScDefn[T] = (Name, Stream[T], Expr[T])
  type CoreScDefn = ScDefn[Name]

  type Op = String
  
  sealed abstract class Expr[+T]
  case class EVar(name: Name) extends Expr[Nothing]
  case class ENum(num: Int) extends Expr[Nothing]
  case class EConstr(x: Int, y: Int) extends Expr[Nothing]
  case class EBinOp[T](op: Op, x: Expr[T], y: Expr[T]) extends Expr[T]
  case class EAp[T](x: Expr[T], y: Expr[T]) extends Expr[T]
  case class ELet[T](isRec: IsRec, xs: Stream[(T, Expr[T])], body:Expr[T]) extends Expr[T]
  case class ECase[T](x: Expr[T], alts: Stream[Alter[T]]) extends Expr[T]
  case class ELam[T](xs: Stream[T], x: Expr[T]) extends Expr[T]
}