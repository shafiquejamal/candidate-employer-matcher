package com.example

import scala.util.Random

case class SimulationResults(candidates: Vector[Candidate], employers: Vector[Employer])

object Main extends App {

  val rng = new Random(9798720938L)

  val NC = 3
  val NP = 3

  val candidates: Vector[Candidate] = Candidate.create(NC, NP, 23L)
  val employers: Vector[Employer] = Employer.create(NC, NP, 91L)
  
  candidates.foreach(println)
  employers.foreach(println)
  
  val results = PartyMatcher.matchParties(candidates, employers)
  results.candidates.foreach(println)
  
}
