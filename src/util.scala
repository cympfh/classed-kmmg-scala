package CRP

object util {
  def toAlphabet(a: String): Alphabet = {
    val xs = a.split('_')
    val word = xs.slice(0, xs.length - 1).mkString("_")
    val pos = xs(xs.length-1)
    Alphabet(word, pos)
  }

  def toSymbol(a: String): Symbol = {
    if (a == "<>") {
      Var()
    } else if (a(0) == '<') {
      Pos(a.slice(1, a.length-1))
    } else {
      toAlphabet(a)
    }
  }

  def readText: Text = {
    val line = readLine
    if (line == null) return null
    new Text(line.split(' ').map(toAlphabet): _*)
  }

  def readAllText(ac: Array[Text]): Array[Text] =
    readText match {
      case null => ac
      case t => readAllText(ac :+ t)
    }

  def readPattern: Pattern = {
    val line = readLine
    if (line == null) return null
    new Pattern(line.split(' ').map(toSymbol): _*)
  }

  def preceq(a: Alphabet, b: Var): Boolean = true
  def preceq(a: Alphabet, b: Pos): Boolean = a.pos == b.pos
  def preceq(a: Alphabet, b: Alphabet): Boolean =
    a.pos == b.pos && a.word == b.word

  def preceq(a: Alphabet, b: Symbol): Boolean = b match {
    case Var() => true
    case Pos(_) => preceq(a, b.asInstanceOf[Pos])
    case Alphabet(_, _) => preceq(a, b.asInstanceOf[Alphabet])
  }

  def preceq(t: Text, p: Pattern): Boolean = {
    var n = t.length
    var m = p.length
    if (n < m) return false

    // argmin[j] { j >= i and p(j).is[Var] }
    def nextVar(i: Int): Int =
      if (i >= m || p(i).isInstanceOf[Var]) i
      else nextVar(i+1)

    // t(pos...pos+alpha) == p(begin...end) ?
    def partial_eq(pos: Int, begin: Int, end: Int): Boolean = {
      if (pos >= n) return false
      if (end >= m) return false
      if (begin == end) {
        true
      } else if (preceq(t(pos), p(begin))) {
        partial_eq(pos + 1, begin + 1, end)
      } else {
        false
      }
    }

    // tail
    while (m > 0 && !p(m-1).isInstanceOf[Var]) {
      if (preceq(t(n-1), p(m-1))) {
        n -= 1
        m -= 1
      } else {
        return false
      }
    }
    if (n == 0 && m == 0) return true
    if (m == 0) return false

    // p should be "<>[.<>]*<>" or "[.<>]*<>"

    var __pos = 0
    var __begin = 0

    // head
    while (!p(__begin).isInstanceOf[Var]) {
      if (preceq(t(__pos), p(__begin))) {
        __pos += 1
        __begin += 1
      } else {
        return false
      }
      if (__pos >= n) return false
    }

    // p should be "<>[.<>]*<>"

    var __end = -1;
    for (_ <- 0 until p.length) {
      while (__begin < m && p(__begin).isInstanceOf[Var]) {
        __pos += 1; __begin += 1
      }
      if (__begin >= m && __pos <= n) return true
      if (__pos >= n) return false

      __end = nextVar(__begin + 1)

      // search for all __pos
      var res = false
      while (!res && __pos < n - (__end - __begin)) {
        res = partial_eq(__pos, __begin, __end)
        if (res) {
          __pos += __end - __begin
          __begin = __end
        } else {
          __pos += 1
        }
      }
      if (__pos >= n - __end + __begin) return false
    }
    false
  }
}

