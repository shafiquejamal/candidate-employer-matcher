package com.example

case class Candidate(
    override val id: Int,
    override val preferences: Vector[Int],
    maybeMatch: Option[Employer]) extends Party {
}

object Candidate {

  def create(NC: Int, NP: Int, seed: Long): Vector[Candidate] = 1.to(NC).toVector.map { index =>
    Candidate(index, Party.createPreference(NP, seed + index), None)
  }

}

