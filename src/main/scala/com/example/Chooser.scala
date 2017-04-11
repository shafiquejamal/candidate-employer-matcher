package com.example

case class Chooser(override val id: Int, preferences: Vector[Int], maybeMatch: Option[Candidate]) extends Party

object Chooser {

  def createChoosers(NC: Int, NP: Int, seed: Long): Vector[Chooser] = 1.to(NC).toVector.map { index =>
    Chooser(index, Party.createPreference(NP, seed + index), None)
  }

}

