package utils

import data.DataPoint

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.io.Source
import scala.util.Random

object DataInput {
  def parseOrGenerateFile(args: Array[String]): List[DataPoint] = {
    val filename = args(0).toString
    try {
      var lines = Source.fromFile("src/main/resources/" + filename).getLines.toList
      lines.map(line => {
        val splitline = line.split("\\s+").map(_.trim).toList
        val x = splitline(0).toDouble
        val y = splitline(1).toDouble
        new DataPoint(x, y)
      })
    } catch {
      case e: FileNotFoundException => println("Writing to file: " + filename); generateNewFileWithValues(args)
      case e: IOException => println("Got an IOException!"); throw e
    }
  }

  def scale(data: Seq[DataPoint]): Seq[DataPoint] = {
    var new_data = data
    val min_x = (0 - data.map(_.x).min)
    val min_y = (0 - data.map(_.y).min)
    new_data = new_data.map(dp => DataPoint(dp.x + min_x, dp.y + min_y))
    val max_x = new_data.map(_.x).max
    val max_y = new_data.map(_.y).max
    new_data = new_data.map(dp => DataPoint(dp.x / max_x, dp.y / max_y))
    new_data
  }

  def generateNewFileWithValues(args: Array[String]): List[DataPoint] = {
    val filename = args(0).toString
    val n = args(1).toInt
    val distributionProb = args(2) match {
      case "Gaussian" => 1.2
      case "Bimodal" =>.5
      case "Outlier" => 1.3
    }
    val r = Random
    val pw = new PrintWriter(new File("src/main/resources/temp/" + filename))
    val data = (1 to n).map(a => {
      var y = 0.0
      var x = r.nextGaussian()
      if (r.nextDouble() > distributionProb) { // Bimodal
        y = 2 * x + r.nextGaussian() + 10
      } else if (distributionProb == 1.3 && (a > (n - 3))) { // Outlier (the last 3)
        x = 200 + r.nextGaussian()
        y = r.nextGaussian()
      } else { // Gaussian
        y = x + r.nextGaussian()
      }
      pw.write(x + " " + y + "\n")
      DataPoint(x, y)
    }).toList
    pw.close
    data
  }

}
