package com.example

import scala.util.Random

case class SimulationResults(
    choosers: Vector[Chooser],
    positions: Vector[Candidate])

object Main extends App {

  val rng = new Random(9798720938L)

  val NC = 3
  val NP = 3

  val choosers: Vector[Chooser] = Chooser.createChoosers(NC, NP, 23L)
  val candidates: Vector[Candidate] = Candidate.createCandidates(NC, NP, 91L)

  choosers.foreach(println)
  candidates.foreach(println)
  
  val results = PartyMatcher.matchParties(choosers, candidates)
  results.choosers.foreach(println)
  
}
