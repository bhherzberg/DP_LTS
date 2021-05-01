package data

import scala.util.Random

case class DataPoint(x: Double, y:Double) {
  def getSlope(d2: DataPoint): Double = {
    if(d2.x - this.x == 0) {
      //TODO: WOrk with no deistinct x
      Double.PositiveInfinity
    } else {
      (d2.y - this.y)/(d2.x - this.x)
    }
  }

  def getResidual(alpha: Double, beta: Double): Tuple2[DataPoint, Double] = {
    val res = this.y - (alpha + beta * this.x)
    (this, math.pow(res, 2))
  }

  def perturb(variance: Double) : DataPoint = {
    val r = new Random()
    DataPoint(this.x, this.y + r.nextGaussian()*variance)
  }
}
