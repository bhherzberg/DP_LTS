package lts

import data.{DataPoint, PointPair, RegressionModel, ResidualDataPoint}
import rnm.RandomNoisyMax


case class LTS(data: Seq[DataPoint]) {
  def generateDifferentiallyPrivateBeta(trimmingFactor: Int, granularity: Int, count: Int = 1, epsilon: Double): Seq[Double] = {
    val n = data.length
    val LTS_Originals = LTS_Hs((trimmingFactor to trimmingFactor))

    val alpha_betas = LTS_Originals.map(lts => {
//      println(s"DataPoints inclued ${lts._1.getDataPoints().sortBy(dp => dp.x)}")
      (lts._1.alpha_hat, lts._1.beta_hat)
    })


    RandomNoisyMax(data, granularity, trimmingFactor, epsilon, count).DP(alpha_betas.head._1, alpha_betas.head._2)

  }


  def LTS_Hs(hs: Seq[Int]): Seq[(RegressionModel, Int)] = {
    val slopes = findSortSlopes()
    val beta = slopes.head.slope - .001 //TODO: DO  WE NEED TO BE INTELLIGENT
    val residuals = findComputeResiduals(beta)
    //    var xs : Seq[Int] = Seq.empty[Int]
    //    var ys : Seq[Double] = Seq.empty[Double]
//    print("[")
//    hs.map(_ => print("."))
//    println("]")
//    print("[")
    var pairs = hs.par.map(h => {
      val rm = LTS(h, slopes, residuals)
//      print(".")
      //      if (h % 10 == 0) {
      //        xs = xs ++: hs
      //        ys = ys ++: (hs.map(h => h * rm.beta_hat))
      //      }
      (rm, h)
    }).seq
    //    Scatter(xs, ys,
    //          name = "line+marker",
    //          mode = ScatterMode(ScatterMode.Markers)).plot("HTML/best_fit_for_H_%s.html".format("all"))
//    print("]\n")
//    pairs.foreach(r => println(s"${r._2},${r._1.beta_hat}"))
    //    pairs.foreach(r => println(s"Points Included: ${r._1.getDataPoints()}"))
    pairs
    //    var prev = pairs(0)._1.getDataPoints()
    //    pairs.foreach(r => {
    //      prev = r._1.getDataPoints().intersect(prev)
    //      println(prev.length)
    //    })
  }

  def LTS(h: Int, slopes: Seq[PointPair], residuals: Seq[ResidualDataPoint]): RegressionModel = {
    iterateThroughSlopes(slopes, h, partB(residuals, h))
  }

  def findSortSlopes(): Seq[PointPair] = {
    val datapairs = data.combinations(2).map(se => (se(0),se(1))).toSeq.par
//      .flatMap(p1 => data.map(p2 => (p1, p2)))
      .filter { case (a, b) => a.x != b.x }
      .map { case (a, b) => PointPair(a, b) }.seq
    datapairs.sortBy(p => p.slope)
  }

  def findComputeResiduals(slope: Double): Seq[ResidualDataPoint] = {
    data.map(dp => ResidualDataPoint(dp, slope)).sortBy(rdp => (math.abs(rdp.residual), rdp.dataPoint.x))
  }

  def partB(residuals: Seq[ResidualDataPoint], h: Int): RegressionModel = {
    val n = data.size
    val initialRM = RegressionModel(residuals, h, 0).apply()
    //Loop though to update the values
    (1 to n - h).foldLeft(initialRM)({
      case (b: RegressionModel, p1: Int) => {
        val a = b.moveIntervalOver(p1)
        if (a.ss < b.ss) {
//          println(s"Result update in part B Slopes: ${a.ss}, ${b.ss}")
          a
        }; else {
//          println(s"No update in part B Slopes: ${a.ss}, ${b.ss}")
          b
        }
      }
    })
  }

  def iterateThroughSlopes(slopes: Seq[PointPair], h: Int, defaultRM: RegressionModel): RegressionModel = {
    slopes.zipWithIndex.foldLeft(defaultRM)({
      case (rm: RegressionModel, (pp: PointPair, index: Int)) => {
        var residuals = rm.residuals.toList
        val residualsIndexs = residuals.zipWithIndex
        val i_before = residualsIndexs.filter(r => r._1.dataPoint.y.equals(pp.d1.y) && r._1.dataPoint.x.equals(pp.d1.x)).head
        val j_before = residualsIndexs.filter(r => r._1.dataPoint.y.equals(pp.d2.y) && r._1.dataPoint.x.equals(pp.d2.x)).head
        if (i_before._2 < (data.size - h) || i_before._2 > h || j_before._2 < (data.size - h) || j_before._2 > h) {
//          println(s"Point pair: $pp \nResiduals Before: $residuals")
          residuals = residuals.updated(i_before._2, j_before._1)
          residuals = residuals.updated(j_before._2, i_before._1)
//          println(s"Residuals After: $residuals")
          val result = partB(residuals, h)
          if (result.ss < rm.ss) {
//            println(s"Result update in itterate Throuh Slopes: ${result.ss}")
            result
          }; else rm;
        } else {
          rm
        }
      }
    })
  }


}
