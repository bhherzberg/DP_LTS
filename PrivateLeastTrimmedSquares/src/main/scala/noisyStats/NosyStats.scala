package noisyStats

import data.DataPoint
import utils.RandomDistributions
import utils.RandomDistributions.laplaceNoise

import scala.math.{abs, log, sin}
import scala.util.Random

case class NosyStats(data: Seq[DataPoint], epsilon: Double) {
  val r = Random

  def compute(count: Int): Seq[Double] = {
    (0 until count).map(_ => this.compute())
  }

  def compute(): Double = {
    val data_len = data.size.toDouble
    val x_mean = data.map(_.x).sum / data_len
    val y_mean = data.map(_.y).sum / data_len

    val A = (0 until data_len.toInt).map(ii => {
      (data(ii).x - x_mean) * (data(ii).y - y_mean)
    }).sum
    val B = (0 until data_len.toInt).map(ii => {
      (data(ii).x - x_mean) * (data(ii).x - x_mean)
    }).sum
    val delta = 1-(1.0/data_len)
    val l1 = laplaceNoise(0, delta/(epsilon/2.0))
    val l2 = laplaceNoise(0, delta/(epsilon/2.0))
    if(B + l2 <= 0) {
       0
    } else {
      (A + l1)/(B+l2)
    }
  }


}
