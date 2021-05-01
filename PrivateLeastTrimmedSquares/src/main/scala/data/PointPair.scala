package data

case class PointPair(d1: DataPoint, d2: DataPoint) {
  val slope = d1.getSlope(d2)

  override def toString: String = (d1, d2, slope).toString()
}

case class ResidualDataPoint(dataPoint: DataPoint, var beta: Double) {

  var residual = dataPoint.y - (dataPoint.x * beta)

  def resetBeta(betaNew: Double): ResidualDataPoint = {
    ResidualDataPoint(this.dataPoint, betaNew)
  }

  override def toString: String = {
    (dataPoint.x).toString()
  }
}
