package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x.+(other.x), y.+(other.y))

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar)

  def unary_- : Vector = new Vector(-x, -y)

  def euclideanLength: Double = Math.sqrt(x * x + y * y)

  def normalized: Vector = {
    if (x != 0 && y != 0) {
      new Vector(x / Math.sqrt(x * x + y * y), y / Math.sqrt(x * x + y * y))
    } else {
      if (x == 0) {
        new Vector(0, y / Math.sqrt(x * x + y * y))
      }
      if (y == 0) {
        new Vector(x / Math.sqrt(x * x + y * y), 0)
      }
      new Vector(0, 0)
    }
  }

  override def equals(other: Any): Boolean = {
    this.hashCode() == other.hashCode()
  }
  // Vector(x, y)
  override def toString: String = new String(s"Vector($x, $y)")
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(length * Math.cos(angle), Math.sin(angle) * length)

  def sum(list: List[Vector]): Vector = {
    var x: Double = 0
    var y: Double = 0
    for (vector <- list) {
      x = x + vector.x
      y = y + vector.y
    }
    new Vector(x, y)
  }

  def unapply(arg: Vector): Option[(Double, Double)] = Option(arg.x, arg.y)
}
