import org.scalatest._
import flatspec._
import matchers._
import scala.language.implicitConversions

import skanren._
import skanren.sexp._

def nullo(x:SExp) = x=:=list()
def caro(x:SExp,a:SExp) = exists(d=>x=:=cons(a,d))
def cdro(x:SExp,d:SExp) = exists(a=>x=:=cons(a,d))
def conso(a:SExp,d:SExp,x:SExp) = x=:=cons(a,d)

def membero(x:SExp,l:SExp):Goal = conde(
  caro(l,x),
  exists(d=>begin(
    cdro(l,d),
    membero(x,d)
  ))
)

/*
def lookupo(env:SExp,id:SExp,value:SExp):Goal = conde(
  caro(env, cons(id, value)),
  exists(d=>begin(!caro(env, cons(id, value)),cdro(env,d),lookupo(d,id,value)))
)
def evalo(env:SExp,code:SExp,result:SExp):Goal = conde(
  lookupo(env,code,result),
  ???
)
*/

class ExampleSpec extends AnyFlatSpec with should.Matchers {
  it should "work" in {
    // some are ported from https://github.com/mbillingr/miniKANREN/blob/master/R7RS/test.scm
    (t=:=f).runAll should be (List())
    printAll(q=>q=:=t) should be (Set(t))
    printAll(q=>q=:=t||q=:=f) should be (Set(t,f))
    printAll(q=>caro(list(Sym("a"),Sym("b")),q)) should be (Set(Sym("a")))
    printAll(q=>conde(caro(list(Sym("q"),Sym("o"),q),Sym("a")))) should be (Set())
    printAll(q=>exists(d0=>begin(cdro(list(Sym("q"),Sym("o"),q),d0),conde(caro(d0,Sym("a")))))) should be (Set())
    printAll(q=>conde(caro(list(Sym("q"),Sym("o"),q),Sym("a")),exists(d0=>begin(cdro(list(Sym("q"),Sym("o"),q),d0),conde(caro(d0,Sym("a"))))))) should be (Set())
    printAll(q=>membero(Sym("a"),list(Sym("q"),Sym("o"),q))) should be (Set(Sym("a")))
    printAll(q=>membero(q,list(Sym("q"),Sym("o")))) should be (Set(Sym("q"),Sym("o")))
    printAll(q=>exists(d=>begin(q=/=d,membero(q,list(d,Sym("o")))))) should be (Set(Sym("o")))
    //printAll(q=>begin(!membero(q,list(Sym("b"),Sym("o"),Sym("a"))),membero(q,list(Sym("q"),Sym("o"))))) should be (Set(Sym("q")))
    //print1(q=>begin(!membero(q,list(Sym("b"),Sym("o"),Sym("a"))),membero(q,list(Sym("q"),Sym("o"))))) should be (Some(Sym("q")))
  }
}
