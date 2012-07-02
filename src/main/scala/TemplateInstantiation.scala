package spjlang

object TemplateInstantiation {
  import Ast._
  import Parser._
  
  type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
  type TiStack = Stream[Addr]
  
  type ASSOC[A, B] = Map[A, B]
  
  type Addr = Int
  type Heap[A] = (Addr, Map[Addr, A])
  val hInitial = (0, Map[Addr, Node]())
  sealed abstract class TiDump
  case object DummyTiDump extends TiDump
  val initialTiDump = DummyTiDump
  type TiHeap = Heap[Node]
  
  sealed abstract class Node
  case class NAp(
    addr1: Addr,
    addr2: Addr
  ) extends Node
  case class NSupercomb(
    name: Name,
    names: Stream[Name],
    expr: CoreExpr
  ) extends Node
  case class NNum(
    num: Int
  ) extends Node
  case class NInd(
    addr: Addr
  ) extends Node
  case class NPrim(
    name: Name,
    prim: Prim
  ) extends Node
  case class NData(
    tag: Int,
    addrs: Seq[Addr]
  ) extends Node

  type Prim = Primitive.Value
  object Primitive extends Enumeration {
    val Neg, Add, Sub, Mul, Div = Value
  }

  val primitives = Map(
    ("negate", Neg),
    ("+", Add),
    ("-", Sub),
    ("*", Mul),
    ("/", Div)
  )

  def primStep(state: TiState, prim: Prim): TiState = {
    def primNeg(state: TiState) = {
    }
    prim match {
      case Neg => primNeg(state)
      case Add => primArith(state, _ + _)
      case Sub => primArith(state, _ - _)
      case Mul => primArith(state, _ * _)
      case Div => primArith(state, _ / _)
    }
  }

  type TiGlobals = ASSOC[Name, Addr]

  type TiStats = Int
  val tiStatInitial : TiStats = 0
  def tiStatIncSteps(stats: TiStats): TiStats = stats + 1
  def tiStatGetSteps(stats: TiStats): Int = stats

  def applyToStats(
    stats_fun: TiStats => TiStats,
    state: TiState
  ) = state match {
    case (stack, dump, heap, sc_defs, stats) =>
      (stack, dump, heap, sc_defs, stats_fun(stats))
  }
  
  val prelude = """
I x = x ;
K x y = x ;
K1 x y = y ;
S f g x = f x (g x) ;
compose f g x = f (g x) ;
twice f = compose f f
"""
  val preludeDefs = parse(program, prelude).get
  val extraPreludeDefs = Stream()

  def compile(prog: CoreProgram): TiState = {
    val sc_defs = prog ++ preludeDefs ++ extraPreludeDefs
    val (initial_heap, globals) = buildInitialHeap(sc_defs)
    val address_of_main = globals("main")
    val initial_stack = Stream(address_of_main)
    (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
  }

  def buildInitialHeap(sc_defs: CoreProgram): (TiHeap, TiGlobals) = {
    var heap = hInitial
    var env = Map[Name, Addr]()
    for {
      sc_def <- sc_defs
    } {
      val (new_heap, (name, addr)) = allocateSc(heap, sc_def)
      heap = new_heap
      env = env.updated(name, addr)
    }
    (heap, env)
  }

  def allocateSc(heap: TiHeap, sc_def: CoreScDefn): (TiHeap, (Name, Addr)) = {
    val (name, args, body) = sc_def
    val (new_heap, addr) = hAlloc(heap, NSupercomb(name, args, body))
    (new_heap, (name, addr))
  }

  def hLookup(heap: TiHeap, addr: Addr): Node = heap._2(addr)
  def hAlloc(heap: TiHeap, comb: Node): (TiHeap, Addr) = {
    val new_addr = heap._1 + 1
    ((new_addr, heap._2.updated(new_addr, comb)), new_addr)
  }
  
  def eval(state: TiState): Stream[TiState] = {
    def doAdmin(state: TiState): TiState =
      applyToStats(tiStatIncSteps, state)
    
    def tiFinal(state: TiState) =
      state match {
        case (Stream(sole_addr), dump, heap, globals, stats) =>
          isDataNode(hLookup(heap, sole_addr))
        case (Stream(), dump, heap, globals, stats) =>
          sys.error("Empty stack!")
        case state => false
      }
    
    def isDataNode(node: Node) =
      node match {
        case NNum(_) => true
        case node => false
      }
    
    state #:: (
      if (tiFinal(state)) Stream()
      else eval(doAdmin(step(state)))
    )
  }
  
  def step(state: TiState) = {
    def dispatch(node: Node) = {
      node match {
        case NNum(n) =>
          numStep(state, n)
        case NAp(a1, a2) =>
          apStep(state, a1, a2)
        case NSupercomb(sc, args, body) =>
          scStep(state, sc, args, body)
      }
    }
    
    def numStep(state: TiState, n: Int) =
      sys.error("Number applied as a function!")
    
    def apStep(state: TiState, a1: Addr, a2: Addr) = 
      state match {
        case (stack, dump, heap, globals, stats) =>
         (a1 #:: stack, dump, heap, globals, stats)
      }

    def scStep(
      state: TiState,
      sc_name: Name,
      arg_names: Stream[Name],
      body: CoreExpr
    ) = 
      state match {
        case (stack, dump, heap, globals, stats) =>
          val arg_bindings = arg_names.zip(getArgs(heap, stack)).toMap
          val env = arg_bindings ++ globals
          val (new_heap, result_addr) =
            instantiate(body, heap, env)
          val new_stack = result_addr #:: stack.drop(arg_names.length+1)
          (new_stack, dump, new_heap, globals, stats)
      }
    
    val (stack, dump, heap, globals, stats) = state
    dispatch(hLookup(heap, stack.head))
  }

  def getArgs(heap: TiHeap, stack: TiStack) =
    stack.tail.map{ addr => 
      val NAp(_, arg) = hLookup(heap, addr)
      arg
    }
  
  def instantiate(
    expr: CoreExpr,
    heap: TiHeap,
    env: ASSOC[Name, Addr]
  ): (TiHeap, Addr) = {
    expr match {
      case ENum(n) => hAlloc(heap, NNum(n))
      case EAp(e1, e2) =>
        val (heap1, a1) = instantiate(e1, heap, env)
        val (heap2, a2) = instantiate(e2, heap1, env)
        hAlloc(heap2, NAp(a1, a2))
      case EVar(v) =>
        (heap, env(v))
      case ELet(false, defs, exp) =>
        val defs_names = defs.map(_._1)
        val defs_rhs = defs.map(_._2)
        val (new_heap, acc) =
          defs_rhs.foldLeft((heap, Stream[Addr]())) {
            case ((heap, acc), define) =>
              val (hp, ad) = instantiate(define, heap, env)
              (hp, ad #:: acc)
          }
        instantiate(exp, new_heap, defs_names.zip(acc.reverse).toMap ++ env)
      case ELet(true, defs, exp) =>
	sys.error("yet implemented.")
      case EConstr(tag, arity) =>
        sys.error("yet implemented.")
    }
  }
  
  def showResults(states: Stream[TiState]): String = {
    states.toString()
  }

  def test(s: String) = {
    val (x, _, (_, e), _, _) = eval(compile(parse(program, s).get)).last
    e(x.head)
  }
}
