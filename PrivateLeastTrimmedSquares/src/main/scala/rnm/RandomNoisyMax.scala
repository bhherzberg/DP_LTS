package rnm

import data.DataPoint
import BetaChange.moveBeta
import lts.LTS

case class RandomNoisyMax(data: Seq[DataPoint],
                          granularity: Int,
                          trimmingFactor: Int,
                          epsilon: Double,
                          count: Int) {

  def DP(alpha: Double, beta: Double): Seq[Double] = {
    val sortedSlopes = LTS(data).findSortSlopes()
    val maxSlopes = sortedSlopes.slice(sortedSlopes.size - (trimmingFactor / 2), sortedSlopes.size).map(_.slope)
    val minSlopes = sortedSlopes.slice(0, trimmingFactor / 2).map(_.slope)

    val minSlope = (minSlopes.sum / minSlopes.size.toDouble)
    val maxSlope = (maxSlopes.sum / maxSlopes.size.toDouble)
    val slopes = (minSlope to maxSlope by ((maxSlope - minSlope) / granularity.toDouble))
    val changes = moveBeta(alpha, beta, data, trimmingFactor, slopes)
//    changes.sortBy(_._2).foreach({
//      case (k, v) => println((k, v))
//    })
    DeltaRNM(epsilon, count = count).rnm(changes, slopes)
  }

}
