package com.example


case class Candidate(
    override val id: Int,
    override val preferences: Vector[Int]) extends Party {
  require(preferences.distinct.length == preferences.length)
}

object Candidate {

  def createCandidates(NC: Int, NP: Int, seed: Long): Vector[Candidate] = 1.to(NP).toVector.map { index =>
    Candidate(index, Party.createPreference(NC, seed + index))
  }

}