import org.scalatest._
import flatspec._
import matchers._
import scala.language.implicitConversions

class ExampleSpec extends AnyFlatSpec with should.Matchers {
  import skanren._
import skanren.sexp._
  it should "work" in {
    // ported from https://github.com/mbillingr/miniKANREN/blob/master/R7RS/test.scm
    (t===f).runAll should be (List())
    printAll(q=>q===t) should be (List(t))
  }
}
