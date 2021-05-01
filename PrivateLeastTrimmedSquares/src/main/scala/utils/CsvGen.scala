package utils

import java.io.{File, FileNotFoundException, IOException, PrintWriter}

object CsvGen {
  def writeCsvFile[T] (filename: String, data: List[List[T]]) = {
    val pw = new PrintWriter(new File("src/main/resources/" + filename + ".csv"))
    for (row <- data) {
        for (v <- row) {
            pw.write(v + ",")
        }
        pw.write("\n")
    }
    pw.close
  }
}