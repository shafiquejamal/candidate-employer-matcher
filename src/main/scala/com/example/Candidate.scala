package com.example


case class Candidate(override val id: Int, preferences: Vector[Int], override val maybeMatch: Option[Int]) extends Party

object Candidate {

  def createCandidates(NC: Int, NP: Int, seed: Long): Vector[Candidate] = 1.to(NP).toVector.map { index =>
    Candidate(index, Party.createPreference(NC, seed + index), None)
  }

}