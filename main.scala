import CRP._
import CRP.util._
import scala.collection.{mutable => mut}
import scala.collection.immutable._

object Main {

  def main(args: Array[String]) {
    var docs = readAllText(Array[Text]())
    var k = docs.length - 1
    var DEBUG = false

    def optarg: Boolean = {
      var i = 0
      while (i < args.length) {
        if (args(i) == "-K") {
          k = args(i+1).toInt
          i += 1
        }
        else if (args(i) == "-D") {
          DEBUG = true
        }
        else {
          Console.err.println("scala Main [-D] -K <int>")
          return false
        }
        i += 1
      }
      true
    }

    if (optarg) {
      val ps = kmmg(k, docs, DEBUG)
      println("")
      for (p <- ps) println(p)
    }
  }

}
