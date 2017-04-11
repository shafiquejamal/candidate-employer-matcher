package com.example

import scala.util.Random

trait Party {
  
  def id: Int
  def preferences: Vector[Int]
  
  require(preferences.distinct.length == preferences.length)
  
}

object Party {
  
  def createPreference(N: Int, seed: Long = 20198093): Vector[Int] = {
    val rng = new Random(seed)
    rng.shuffle(1.to(N)).toVector
  }
  
}