package com.example

object Main extends App {
  
  /*
  val NC = 3
  val NE = 3
  
  val candidates: Vector[Candidate] = Candidate.create(NC, NE, 23L)
  val employers: Vector[Employer] = Employer.create(NC, NE, 91L, 2)
  */
  
  val candidates = Reader.readCandidateData(args(0))
  val employers = Reader.readEmployerData(args(1))
  
  candidates.foreach(println)
  employers.foreach(println)
  
  val results = PartyMatcher.matchParties(candidates, employers)
  
  Writer.printToScreen(results)
  Writer.writeToFile(results, args(2))
}
