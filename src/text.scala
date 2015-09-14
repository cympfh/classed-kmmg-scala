package CRP

class Text(xs: Alphabet*) {
  def units = xs toArray
  def apply(i: Int) = units(i)
  def length = units.length
  override def toString = units.mkString(" ")
}

