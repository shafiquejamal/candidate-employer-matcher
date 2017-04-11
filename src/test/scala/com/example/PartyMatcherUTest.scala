package com.example

import org.scalatest.{FlatSpecLike, ShouldMatchers}

class PartyMatcherUTest extends FlatSpecLike with ShouldMatchers {

  "The party matcher" should "update the candidates choice to choose its preferred employer among all offers and the " +
  "current employer choice" in {
    
    val employer8 = Employer(8, Vector(3, 4, 1))
    val employer5 = Employer(5, Vector(3, 2, 5))
    val employer6 = Employer(6, Vector(1, 2, 5))
    val employer1 = Employer(1, Vector(3, 2, 5))
    val employers = Vector(employer8, employer5)
    val preference = 3
    val candidateNoChoiceYet = Candidate(3, Vector(7, 6, 5, 2, 8), None)
    val candidateNoChange = Candidate(3, Vector(7, 6, 5, 2, 8), Some(employer6))
    val candidateChange = Candidate(3, Vector(7, 6, 8, 2, 5, 1), Some(employer1))
    
    val (candidateNoPrevChoice, rejectedEmployers) = PartyMatcher.maybeUpdatedCandidate(preference, employers, candidateNoChoiceYet)
    candidateNoPrevChoice should contain(candidateNoChoiceYet.copy(maybeMatch = Some(employer5)))
    val expectedRejectedEmployers = Vector(employer8.copy(preferences = employer8.preferences.tail))
    rejectedEmployers should contain theSameElementsAs expectedRejectedEmployers
    
    val (candidateKeepExistingChoice, _) = PartyMatcher.maybeUpdatedCandidate(preference, employers, candidateNoChange)
    candidateKeepExistingChoice should contain(candidateNoChange)
    
    val (candidateWithExistingGetsUpdated, _) = PartyMatcher.maybeUpdatedCandidate(preference, employers, candidateChange)
    candidateWithExistingGetsUpdated should contain(candidateChange.copy(maybeMatch = Some(employer8)))
  }
  
  it should "Return stable matches when the preferences for employers and candidates are properly formed " +
  "(no duplicated preferences, etc)" in {
  
    val candidate1 = Candidate(1, Vector(1, 3, 2), None)
    val candidate2 = Candidate(2, Vector(1, 2, 3), None)
    val candidate3 = Candidate(3, Vector(2, 3, 1), None)
    val employer1 = Employer(1, Vector(3, 2, 1))
    val employer2 = Employer(1, Vector(2, 3, 1))
    val employer3 = Employer(1, Vector(3, 1, 2))
    val candidates = Vector(candidate1, candidate2, candidate3)
    val employers = Vector(employer1, employer2, employer3)
    
    val results = PartyMatcher.matchParties(candidates, employers)
    
  }

}
