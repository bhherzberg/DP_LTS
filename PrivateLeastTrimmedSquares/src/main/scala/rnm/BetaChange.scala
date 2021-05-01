package rnm

import data.DataPoint

import scala.collection.mutable
import scala.collection.parallel.immutable.ParHashMap

object BetaChange {
  def moveBeta(alpha: Double, beta: Double, datapoints: Seq[DataPoint], trimmingFactor: Int, slopes: Seq[Double]): Seq[(Int, Double)] = {
    val initial_datapoints = datapoints.map(_.getResidual(alpha, beta)).sortBy(_._2).slice(0, trimmingFactor)
    var hashMap = (mutable.Map[Int, Double]())
    slopes.foreach(testBeta => {
      val lowestResiduals = datapoints.map(_.getResidual(alpha, testBeta)).sortBy(_._2).slice(0, trimmingFactor)
      val numberChanged = initial_datapoints.size - initial_datapoints.map(_._1).intersect(lowestResiduals.map(_._1)).size + 1
      if (testBeta > beta) {
        if (!hashMap.contains(numberChanged) || hashMap(numberChanged) < testBeta) {
          hashMap = hashMap + (numberChanged -> testBeta)
        }
      } else {
        if (!hashMap.contains(-1 * numberChanged) || hashMap(-1 * numberChanged) > testBeta) {
          hashMap = hashMap + ((-1 * numberChanged) -> testBeta)
        }
      }
    })
    hashMap.toSeq.sortBy(_._1)
  }

}
