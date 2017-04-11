package com.example

import scala.util.Random


case class Employer(
    override val id: Int,
    override val preferences: Vector[Int],
    availablePositions: Int) extends Party {
  require(preferences.distinct.length == preferences.length)
  require(availablePositions > 0)
}

object Employer {

  def create(NC: Int, NP: Int, seed: Long, limit: Int): Vector[Employer] = 1.to(NP).toVector.map { index =>
    val seedPlusIndex = seed + index
    Employer(index, Party.createPreference(NC, seedPlusIndex), createNumberOfAvailablePositions(limit, seedPlusIndex))
  }
  
  private def createNumberOfAvailablePositions(limit: Int, seed: Long) = {
    val rng = new Random(seed)
    rng.nextInt(limit)
  }

}