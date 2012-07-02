package spjlang
object MultMachine {
  type MultState = (Int, Int, Int, Int)

  def evalMult(state: MultState): Stream[MultState] = {
    if (multFinal(state)) {
      Stream(state)
    } else {
      state #:: evalMult(stepMult(state))
    }
  }

  def stepMult(state: MultState) = {
    state match {
      case (n, m, d, t) if d > 0 => (n, m, d-1, t+1)
      case (n, m, d, t) if d == 0 => (n, m-1, n, t)
    }
  }

  def multFinal(state: MultState) = {
    state._2 == 0 && state._3 == 0
  }

  def test = evalMult((2, 3, 0, 0)).last
}