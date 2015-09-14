import CRP._
import CRP.util._

object Test {
  def main(args: Array[String]) {
    var ok = true
    def iter {
      val t = readText
      if (t == null) return
      val p = readPattern
      if (p == null) return
      val b = readLine toBoolean;
      if (b != preceq(t, p)) {
        ok = false
        println(t)
        println(p)
        println("expected: " + b)
      }
      iter
    }
    iter
    if (ok) {
      println("\u001b[43mall passed\u001b[0m")
    }
  }
}
