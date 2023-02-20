package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = {
    val new_x: Double = x * scalar
    val new_y: Double = y * scalar
    new Vector(new_x, new_y)
  }

  def unary_- : Vector = new Vector(-x, -y)

  def euclideanLength: Double = Math.sqrt(x * x + y * y)

  def normalized: Vector = {
    var norm_x: Double = x
    var norm_y: Double = y
    if (x != 0) {
      norm_x = x / this.euclideanLength
    }
    if (y != 0){
      norm_y = y / this.euclideanLength
    }
    new Vector(norm_x, norm_y)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: Vector => {
        other.x == this.x && other.y == this.y
      }
      case _ => false
    }
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
