package runner

import utils.CsvGen.writeCsvFile
import utils.DataInput.{parseOrGenerateFile, scale}
import lts.{LS, LTS}
import noisyStats.NosyStats
import plotly.Plotly.TraceOps
import plotly.Scatter
import plotly.element.ScatterMode
import utils.Stats

import scala.math.abs

object CodeRunner {
  def main(args: Array[String]): Unit = {
    val iterations : Seq[Int] = (0 until 10)
    val datasetSizes : Seq[Int] = Seq(10, 25, 50,75, 100, 200, 300, 400, 500)
    val dataSetsTypes = (points: Int, iteration: Int) => Seq(
      Seq(s"Gaussian_count${points}_iteration$iteration.txt", s"$points", "Gaussian", s"$iteration"),
      Seq(s"Bimodal_count${points}_iteration$iteration.txt", s"$points", "Bimodal", s"$iteration"),
      Seq(s"Outlier_count${points}_iteration$iteration.txt", s"$points", "Outlier", s"$iteration")
    )
    val dataSets = datasetSizes.flatMap(c => iterations.flatMap(i => dataSetsTypes(c, i)))
    //Delete this par if you value your computer being able to do ther things while this runs
    // Seq(args_sp(2), args_sp(1), args_sp(3),count, true_beta, mclts_beta_avg, abs(mclts_beta_avg-true_beta), mclts_beta_std, noisy_stats_beta_avg, abs(noisy_stats_avg-true_beta), noisy_stats_std)
    val ouptuts:Seq[Seq[String]] = Seq(Seq("DataType", "IterationNumber", "numPoints", "runs", "trueBeta", "MCLTSBetaAvg", "MCLTSBetaAvgError", "MCLTSBetaStd", "NoisyStatsBetaAvg","NoisyStatsBetaAvgError", "NoisyStatsStd")) ++
      dataSets.par.map(args_sp => {
      val data = scale(parseOrGenerateFile(args_sp.toArray))
      val (y, x) = data.map(r => (r.y, r.x)).toList.unzip
      val n = data.length
      val h = (95 * n) / 100 - 2 // 95 percent of the data plus 2 extra points needed for small datasets
      val count = 10
//      Scatter(x, y,
//        name = "marker",
//        mode = ScatterMode(ScatterMode.Markers)).plot(s"HTML/${args_sp(2)}_${n}.html")

      //True Least squares value
      val true_beta = args_sp(2) match {
        case "Outlier" => LTS(data).LTS_Hs(Seq(n-3)).head._1.beta_hat
        case _ => LS(data).regression()._2
      }
      //MCLTS DP value
      val mclts_betas = LTS(data).generateDifferentiallyPrivateBeta(h, 1000000, count, .1)
      val mclts_beta_avg = Stats.mean(mclts_betas)
      val mclts_beta_std = Stats.std(mclts_betas.map(a => abs(a - true_beta)))
      //NoisyStats DP value
      val noisy_stats = NosyStats(data, .1).compute(count)
      val noisy_stats_avg = Stats.mean(noisy_stats)
      val noisy_stats_std = Stats.std(noisy_stats.map(a => abs(a - true_beta)))


      println(s"True Beta: $true_beta Average of $count MCLTS DP betas: $mclts_beta_avg Diff: ${math.abs(mclts_beta_avg-true_beta)} std: $mclts_beta_std")
      println(s"True Beta: $true_beta Average of $count NS DP betas: $noisy_stats_avg Diff: ${math.abs(noisy_stats_avg-true_beta)} std: $noisy_stats_std")
      Seq(args_sp(2), args_sp(3), args_sp(1),count, true_beta, mclts_beta_avg, abs(mclts_beta_avg-true_beta), mclts_beta_std, noisy_stats_avg,  abs(noisy_stats_avg-true_beta), noisy_stats_std).map(_.toString)
    }).seq

    writeCsvFile("output",ouptuts.toList.map(_.toList))

  }

}
