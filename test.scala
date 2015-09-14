import CRP._
import CRP.util._

object Test {
  def main(args: Array[String]) {
    def iter {
      val t = readText
      if (t == null) return
      val p = readPattern
      val b = readLine toBoolean;
      if (b != preceq(t, p)) {
        println(t)
        println(p)
        println("expected: " + b)
      }
      iter
    }
    iter
  }
}
