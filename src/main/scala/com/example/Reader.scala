package com.example

object Reader {
  
  def readCandidateData(filePath: String): Vector[Candidate] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val candidates = (for (line <- bufferedSource.getLines) yield {
      val cols = line.split(",").map(_.trim.toInt).toVector
      Candidate(cols.head, cols.tail, None)
    }).toVector
    bufferedSource.close
    candidates
  }
  
  def readEmployerData(filePath: String): Vector[Employer] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val employers = (for (line <- bufferedSource.getLines) yield {
      val cols = line.split(",").map(_.trim.toInt).toVector
      val tail = cols.tail
      Employer(cols.head, tail.tail, tail.head)
    }).toVector
    bufferedSource.close
    employers
  }
  
}
