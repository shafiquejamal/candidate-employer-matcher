package com.example

import scala.util.Random

case class SimulationResults(
    choosers: Vector[Chooser],
    positions: Vector[Employer])

object Main extends App {

  val rng = new Random(9798720938L)

  val NC = 3
  val NP = 3

  val choosers: Vector[Chooser] = Chooser.create(NC, NP, 23L)
  val employers: Vector[Employer] = Employer.create(NC, NP, 91L)

  choosers.foreach(println)
  employers.foreach(println)
  
  val results = PartyMatcher.matchParties(choosers, employers)
  results.choosers.foreach(println)
  
}
