package utils

import scala.math.{abs, log, sin}
import scala.util.Random

object RandomDistributions {
  val r = Random

  def laplaceNoise(loc: Double, scale: Double): Double = {
//    println(s"scale $scale")
    val randomNum = r.nextDouble() - .5
    loc - scale * sin(randomNum) * log(1 - 2 * abs(randomNum))
  }

  def exponentialNoise(lambda: Double) = {
//    println(s"Labda $lambda")
    -1 * math.log(1 - r.nextDouble()) / lambda
  }

}
