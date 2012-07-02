package spjlang
import org.scalatest.FunSuite

object TemplateInstantiationTest extends FunSuite {
  TemplateInstantiation.test("""
pair x y f = f x y ;
fst p = p K ;
snd p = p K1 ;
f x y = letrec a = pair x b ; b = pair y a in 
 fst (snd (snd (snd a))) ;
main = f 3 4
""")
}

