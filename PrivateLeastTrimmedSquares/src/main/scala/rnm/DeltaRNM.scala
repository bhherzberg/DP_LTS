package rnm

import utils.RandomDistributions.{exponentialNoise, laplaceNoise}


case class DeltaRNM(epsilon: Double, delta: Double = 1, count: Int = 1) {


  def rnm(scoreBeta: Seq[(Int, Double)], slopesToScore: Seq[Double]): Seq[Double] = {
    val scores = getScores(scoreBeta, slopesToScore)
    (0 until count).map(_ => {
      val scoresPlusNoise = scores.par.map({ case (score, value) => {
//        val exp = laplaceNoise(0, 2/epsilon)
        val exp = exponentialNoise(2 * delta / epsilon)
        //      println(s"$exp")
        (score + exp, value)
      }
      }).seq
      val dp_beta = scoresPlusNoise.maxBy(_._1)._2
//      println(s"Differentially Private Beta : $dp_beta")
      dp_beta
    })
  }


  def getScores(scoreBeta: Seq[(Int, Double)], values: Seq[Double]): Seq[(Int, Double)] = {
    val sortedScores = scoreBeta.sortBy(_._2)
    val sortedValues = values.sorted
    var scoreIndex = 0
    sortedValues.map(value => {
      while (scoreIndex < (sortedScores.length - 1) && value > sortedScores(scoreIndex + 1)._2) {
        //        println(s"Hi There $value , ${sortedScores(scoreIndex + 1)}")
        scoreIndex += 1
      }
      //      println(s"score value pair: ${(-1 * math.abs(sortedScores(scoreIndex)._1),value)}")
      (-1 * math.abs(sortedScores(scoreIndex)._1), value)
    })
  }

}
