package com.example

case class Chooser(
    override val id: Int,
    override val preferences: Vector[Int],
    maybeMatch: Option[Employer]) extends Party {
}

object Chooser {

  def create(NC: Int, NP: Int, seed: Long): Vector[Chooser] = 1.to(NC).toVector.map { index =>
    Chooser(index, Party.createPreference(NP, seed + index), None)
  }

}

