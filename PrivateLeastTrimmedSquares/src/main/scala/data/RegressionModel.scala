package data

case class RegressionModel(var residuals: Seq[ResidualDataPoint], h: Int,l :Int) {

  var x_bar = 0.0
  var y_bar = 0.0
  var xx_bar = 0.0
  var yy_bar = 0.0
  var xy_bar = 0.0
  var cxy_bar = 0.0
  var beta_hat = 0.0
  var alpha_hat = 0.0
  var ss = 0.0
  val h_inv = (1.0 / h)


  def apply(): RegressionModel = {
    val rm = new RegressionModel(residuals, h, l)
    val range = (l until l + h)
//    println(range)
    rm.x_bar =  h_inv * range.map(index => residuals(index).dataPoint.x).sum
    rm.y_bar = h_inv * range.map(index => residuals(index).dataPoint.y).sum
    rm.xx_bar = h_inv * range.map(index => Math.pow(residuals(index).dataPoint.x, 2)).sum
    rm.yy_bar = h_inv * range.map(index => Math.pow(residuals(index).dataPoint.y, 2)).sum
    rm.xy_bar = h_inv * range.map(index => residuals(index).dataPoint.x * residuals(index).dataPoint.y).sum
    rm.reUpNoArgParams()
    rm
  }

  def moveIntervalOver(l_new:Int) = {
//    println(s"Moving Interval Over $l_new")
    val rm = new RegressionModel(residuals, h, l_new)
//    println(s"Removing ${residuals(rm.l - 1)} ")
//    println(s"Adding ${residuals(rm.l - 1 + h)} ")
    rm.x_bar = x_bar + h_inv * (residuals(rm.l - 1 + h).dataPoint.x - residuals(rm.l -1).dataPoint.x)
    rm.y_bar = y_bar + h_inv * (residuals(rm.l -1 + h).dataPoint.y - residuals(rm.l -1).dataPoint.y)
    rm.xx_bar = xx_bar + h_inv * (Math.pow(residuals(rm.l -1 + h).dataPoint.x, 2) - Math.pow(residuals(rm.l -1 ).dataPoint.x, 2))
    rm.yy_bar = yy_bar + h_inv * (Math.pow(residuals(rm.l -1 + h).dataPoint.y, 2) - Math.pow(residuals(rm.l -1).dataPoint.y, 2))
    rm.xy_bar = xy_bar + h_inv * (residuals(rm.l-1 + h).dataPoint.x * residuals(rm.l -1 + h).dataPoint.y - residuals(rm.l -1).dataPoint.x * residuals(rm.l -1).dataPoint.y)
    rm.reUpNoArgParams()
    rm
  }

  private def reUpNoArgParams(): Unit = {
    this.cxy_bar = xy_bar - x_bar * y_bar
    this.beta_hat = cxy_bar / (xx_bar - Math.pow(x_bar, 2))
    this.alpha_hat = y_bar - beta_hat * x_bar
    this.ss = (yy_bar - Math.pow(y_bar, 2)) - beta_hat * cxy_bar
  }

  def getDataPoints():Seq[DataPoint] = {
    residuals.slice(l, l + h).map(_.dataPoint)
  }

  def resetBeta(beta: Double): RegressionModel = {
    this.residuals = residuals.map((res) => res.resetBeta(beta))
    this.apply()
  }

  def resetResiduals(residuals: List[ResidualDataPoint]): RegressionModel = {
    RegressionModel(residuals, this.h, this.l)
  }

}
