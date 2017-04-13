package com.example
import java.io._

object Writer {
  
  def writeToFile(results: SimulationResults, filePath: String) {
    val file = new File(filePath)
    val bw = new BufferedWriter(new FileWriter(file))
    summarizeResults(results).foreach(bw.write)
    bw.close()
  }
  
  def printToScreen(results: SimulationResults) = summarizeResults(results).foreach(print)
  
  def summarizeResults(results: SimulationResults): Vector[String] =
    Vector("Candidate -> Employer\n") ++
    results.candidates.map { candidate =>
        s"${candidate.id} -> " + candidate.maybeMatch.map{_.id}.fold("No match"){ id => id.toString} + "\n"
    }
  
  
}
