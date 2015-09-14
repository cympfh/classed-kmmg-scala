package CRP

class Pattern(xs: Symbol*) {
  def units = xs.toArray.clone
  def apply(i: Int) = units(i)
  def length = units.length
  override def toString = units.mkString(" ")
}
