package spjlang
object PrettyPrinter {
  import Ast._
  def pprExpr(ast: CoreExpr): String = {
    ast match {
      case EVar(v) => v
      case ENum(n) => n.toString
      case EAp(e1, e2) => pprExpr(e1) + " " + pprExpr(e2)
      case EBinOp(op, e1, e2) => pprExpr(e1) + " " + op + " " + pprExpr(e2)
      case EConstr(n1, n2) => "Pack{" + n1 + ", " + n2 + "}"
      case ELet(isRec, xs, body) =>
        (if (isRec) "letrec " else "let ") +
        xs.map{ case (name, exp) => name + " = " + pprExpr(exp) }.mkString("; ") +
        pprExpr(body)
      case ECase(x, alts) =>
        "case " + pprExpr(x) + " of " +
        alts.map{ case (num, vars, body) =>
          "<" + num + ">" + vars.mkString(" ") + " -> " + pprExpr(body) }.mkString("; ")
      case ELam(xs, x) =>
        "\\" + xs.mkString(" ") + " . " + pprExpr(x)
    }
  }
}
