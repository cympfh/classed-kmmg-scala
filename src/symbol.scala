package CRP

sealed abstract class Symbol;

case class Var() extends Symbol {
  override def toString = "<>"
}

case class Pos(pos: String) extends Symbol {
  override def toString = "<" + pos + ">"
}

case class Alphabet(word: String, pos: String) extends Symbol {
  override def toString = word + "/" + pos
}
