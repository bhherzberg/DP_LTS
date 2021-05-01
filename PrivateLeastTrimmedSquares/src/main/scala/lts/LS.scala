package lts

import data.{DataPoint, RegressionModel}

case class LS(dataPoints: Seq[DataPoint]) {
  def regression(): (Double, Double) = {
    val size = dataPoints.length.toDouble
    var x_bar = dataPoints.map(_.x).sum / size
    var y_bar = dataPoints.map(_.y).sum / size
    var xx_bar = dataPoints.map(dp => math.pow(dp.x,2)).sum / size
    var yy_bar = dataPoints.map(dp => math.pow(dp.y,2)).sum / size
    var xy_bar = dataPoints.map(dp => dp.x * dp.y).sum / size
    val beta = (xy_bar - x_bar * y_bar) / (xx_bar - math.pow(x_bar, 2))
    val alpha = y_bar - beta* x_bar
    (alpha, beta)
  }
}
