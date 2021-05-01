package utils

import scala.math.{pow, sqrt}

object Stats {

  def mean(data: Seq[Double]): Double = {
    data.sum / data.size
  }

  def std(data: Seq[Double]) : Double = {
    sqrt(variance(data))
  }

  def variance(data: Seq[Double]): Double = {
    val mn = mean(data)
    data.map(a => pow(a- mn, 2)).sum / data.size
  }

}
