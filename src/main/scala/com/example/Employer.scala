package com.example


case class Employer(
    override val id: Int,
    override val preferences: Vector[Int]) extends Party {
  require(preferences.distinct.length == preferences.length)
}

object Employer {

  def create(NC: Int, NP: Int, seed: Long): Vector[Employer] = 1.to(NP).toVector.map { index =>
    Employer(index, Party.createPreference(NC, seed + index))
  }

}