package CRP

import CRP.util._
import scala.collection.{mutable => mut}
import scala.collection.immutable._

object kmmg {

  def difference(n: Int, xs: List[Int], ys: List[Int]): List[Int] = {
    val a = Array.fill(n)(false)
    val b = Array.fill(n)(false)
    for (x <- xs) a(x) = true
    for (y <- ys) b(y) = true
    var ret = List[Int]()
    for (i <- (n-1) to 0 by -1) if (a(i) && !b(i)) ret = i :: ret
    ret
  }

  def isText(p: Pattern): Boolean =
    p.units.forall(a => a.isInstanceOf[Alphabet])

  def apply(k: Int, docs: Array[Text], DEBUG: Boolean = false): List[Pattern] = {

    val words: Set[Alphabet] =
      (for (t <- docs; a <- t.units) yield a).toSet

    val dictionary =
      mut.Map[String, mut.ListBuffer[Alphabet]]()
    for (t <- docs; a <- t.units) {
      if (!dictionary.contains(a.pos)) {
        dictionary += (a.pos -> mut.ListBuffer[Alphabet]())
      }
      dictionary(a.pos) += a
    }

    def isCovering(p:Pattern, c:List[Int]): Boolean =
      c.forall(i => preceq(docs(i), p))

    def intersect(p:Pattern, c:List[Int]): List[Int] =
      c.filter(i => preceq(docs(i), p))

    def tighten(p:Pattern, c:List[Int]): Pattern = {

      if (isText(p)) return p

      var subdict = mut.Map[String, mut.ListBuffer[Alphabet]]()
      for (i <- c; a <- docs(i).units) {
        if (!subdict.contains(a.pos)) {
          subdict += (a.pos -> mut.ListBuffer[Alphabet]())
        }
        subdict(a.pos) += a
      }

      val us = p.units.clone

      // <> -> <A>
      for (i <- 0 until p.length) {
        if (p(i).isInstanceOf[Var]) {
          for ((pos, _) <- subdict) {
            us(i) = Pos(pos)
            val q = new Pattern(us.clone: _*)
            if (isCovering(q, c)) return tighten(q, c)
          }
          us(i) = Var()
        }
      }

      // <A> -> a/A
      for (i <- 0 until p.length) {
        if (p(i).isInstanceOf[Pos]) {
          val pos = p(i).asInstanceOf[Pos].pos
          if (subdict.contains(pos)) {
            for (a <- subdict(pos)) {
              us(i) = a
              val q = new Pattern(us.clone: _*)
              if (isCovering(q, c)) return tighten(q, c)
            }
          }
          us(i) = Pos(pos)
        }
      }

      // <> -> <> <>
      val vs = new Array[Symbol](p.length + 1)
      for (i <- 0 until p.length) {
        vs(i) = p(i)
        if (p(i).isInstanceOf[Var] &&
          (i == 0 || !p(i-1).isInstanceOf[Var])) {
            vs(i+1) = Var()
            for (j <- i+1 until p.length) {
              vs(j+1) = p(j)
            }
            val q = new Pattern(vs: _*)
            if (isCovering(q, c)) return tighten(q, c)
          }
      }

      p
    }

    def division(p:Pattern, c:List[Int]): List[(Pattern, List[Int])] = {
      var cspc = Array[(Pattern, List[Int])]()
      if (DEBUG) Console.err.println("# division of " + (p, c))

      var subdict = mut.Map[String, mut.ListBuffer[Alphabet]]()
      for (i <- c; a <- docs(i).units) {
        if (!subdict.contains(a.pos)) {
          subdict += (a.pos -> mut.ListBuffer[Alphabet]())
        }
        subdict(a.pos) += a
      }

      val us = p.units.clone

      // <> -> <A>
      for (i <- 0 until p.length) {
        if (p(i).isInstanceOf[Var]) {
          for ((pos, _) <- subdict) {
            us(i) = Pos(pos)
            val q = new Pattern(us.clone: _*)
            val c2 = intersect(q, c)
            if (c2.length > 0) {
              cspc :+= (tighten(q, c2), c2)
            }
          }
          us(i) = Var()
        }
      }

      // <A> -> a/A
      for (i <- 0 until p.length) {
        if (p(i).isInstanceOf[Pos]) {
          val pos = p(i).asInstanceOf[Pos].pos
          if (subdict.contains(pos)) {
            for (a <- subdict(pos)) {
              us(i) = a
              val q = new Pattern(us.clone: _*)
              val c2 = intersect(q, c)
              if (c2.length > 0) {
                cspc :+= (tighten(q, c2), c2)
              }
            }
            us(i) = Pos(pos)
          }
        }
      }

      // <> -> <> <>
      val vs = new Array[Symbol](p.length + 1)
      for (i <- 0 until p.length) {
        vs(i) = p(i)
        if (p(i).isInstanceOf[Var] &&
          (i == 0 || !p(i-1).isInstanceOf[Var])) {
            vs(i+1) = Var()
            for (j <- i+1 until p.length) {
              vs(j+1) = p(j)
            }
            val q = new Pattern(vs.clone: _*)
            val c2 = intersect(q, c)
            if (c2.length > 0) {
              val r = tighten(q, c2)
              cspc :+= (r, c2)
            }
          }
      }

      var ret = List[(Pattern, List[Int])]()
      var m = c.length
      while (m > 0 && cspc.length > 0) {
        cspc = cspc.sortBy(-_._2.length)
        val (q, c2) = cspc.head
        cspc = cspc.tail
        if (c.length > c2.length && c2.length > 0) {
          ret = (tighten(q, c2), c2) :: ret
          m -= c2.length
          for (i <- 0 until cspc.length) {
            var (p, c3) = cspc(i)
            // cspc(i) = (p, c3 diff c2) // List.diff は遅い
            cspc(i) = (p, difference(docs.length, c3, c2))
          }
        }
      }

      if (DEBUG) {
        Console.err.println("--- div: following " + ret.length + " patterns")
        for ((p, c) <- ret) Console.err.println(p, c)
        Console.err.println("---")
      }

      ret
    }

    val whole = Range(0, docs.length).toList
    val p = tighten(new Pattern(Var()), whole)
    val Q = mut.PriorityQueue[(Int, Pattern, List[Int])]()(Ordering.by(_._1))
    Q += ((docs.length, p, whole))

    var ret = List[Pattern]()

    Console.err.println("k = " + k)
    while (!Q.isEmpty) {
      val (len, p, c) = Q.dequeue
      if (c.length == 1) {
        ret = p :: ret
      } else {
        val pcs = division(p, c)
        if (pcs.length <= 1) {
          if (DEBUG) Console.err.println("## not divisible")
          ret = p :: ret
        } else if (ret.length + Q.length + pcs.length <= k) {
          for ((q, d) <- pcs) Q += ((d.length, q, d))
        } else {
          if (DEBUG) Console.err.println("## divisible but too much")
          ret = p :: ret
        }
      }
    }
    ret
  }
}

