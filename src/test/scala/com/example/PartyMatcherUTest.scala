package com.example

import org.scalatest.{FlatSpecLike, ShouldMatchers}

class PartyMatcherUTest extends FlatSpecLike with ShouldMatchers {

  "The party matcher" should "update the candidates choice to choose its preferred employer among all offers and the " +
  "current employer choice" in {
    
    val employer8 = Employer(8, Vector(3, 4, 1), 1)
    val employer5 = Employer(5, Vector(3, 2, 5), 1)
    val employer6 = Employer(6, Vector(1, 2, 5), 1)
    val employer1 = Employer(1, Vector(3, 2, 5), 1)
    val employers = Vector(employer8, employer5)
    val preference = 3
    val candidateNoChoiceYet = Candidate(3, Vector(7, 6, 5, 2, 8), None)
    val candidateNoChange = Candidate(3, Vector(7, 6, 5, 2, 8), Some(employer6))
    val candidateChange = Candidate(3, Vector(7, 6, 8, 2, 5, 1), Some(employer1))
    
    val (candidateNoPrevChoice, rejectedEmployers) = PartyMatcher.chooseMostPreferredOffer(employers, candidateNoChoiceYet)
    candidateNoPrevChoice should contain(candidateNoChoiceYet.copy(maybeMatch = Some(employer5.copy(availablePositions = 0))))
    val expectedRejectedEmployers = Vector(employer8.copy(preferences = employer8.preferences.tail))
    rejectedEmployers should contain theSameElementsAs expectedRejectedEmployers
    
    val (candidateKeepExistingChoice, _) = PartyMatcher.chooseMostPreferredOffer(employers, candidateNoChange)
    candidateKeepExistingChoice should contain(candidateNoChange)
    
    val (candidateWithExistingGetsUpdated, _) = PartyMatcher.chooseMostPreferredOffer(employers, candidateChange)
    candidateWithExistingGetsUpdated should contain(candidateChange.copy(maybeMatch = Some(employer8.copy(availablePositions = 0))))
  }
  
  it should "Return stable matches when the preferences for employers and candidates are properly formed " +
  "(no duplicated preferences, etc)" in {
  
    val candidate1 = Candidate(1, Vector(1, 3, 2), None)
    val candidate2 = Candidate(2, Vector(1, 2, 3), None)
    val candidate3 = Candidate(3, Vector(2, 3, 1), None)
    val employer1 = Employer(1, Vector(3, 2, 1), 1)
    val employer2 = Employer(2, Vector(2, 3, 1), 1)
    val employer3 = Employer(3, Vector(3, 1, 2), 1)
    val candidates = Vector(candidate1, candidate2, candidate3)
    val employers = Vector(employer1, employer2, employer3)
    
    val results = PartyMatcher.matchParties(candidates, employers)
    val expected = Seq(
      Candidate(1,Vector(1, 3, 2),Some(Employer(3,Vector(1, 2),0))),
      Candidate(2,Vector(1, 2, 3),Some(Employer(1,Vector(2, 1),0))),
      Candidate(3,Vector(2, 3, 1),Some(Employer(2,Vector(3, 1),0)))
    )
    results.candidates should contain theSameElementsAs expected
  
  }
  
  it should "return stable matches when employers offer multiple positions" in {
    val candidate1 = Candidate(1, Vector(3, 1, 2), None)
    val candidate2 = Candidate(2, Vector(1, 2, 3), None)
    val candidate3 = Candidate(3, Vector(2, 1, 3), None)
    val candidate4 = Candidate(4, Vector(2, 3, 1), None)
    val candidate5 = Candidate(5, Vector(1, 3, 2), None)
    val candidate6 = Candidate(6, Vector(3, 2, 1), None)
    val employer1 = Employer(1, Vector(6, 1, 2, 4, 5), 3)
    val employer2 = Employer(2, Vector(6, 5, 1, 4), 2)
    val employer3 = Employer(3, Vector(4, 2, 1, 6), 1)
    val candidates = Vector(candidate1, candidate2, candidate3, candidate4, candidate5, candidate6)
    val employers = Vector(employer1, employer2, employer3)
    
    val results = PartyMatcher.matchParties(candidates, employers)
    val expected = Vector(Some(3), Some(1), None, Some(2), Some(1), Some(2))
    results.candidates.map(_.maybeMatch.map(_.id)) should contain theSameElementsInOrderAs expected
  }

  it should "return stable matches when there are more employers and positions than candidates" in {
  
    val candidate1 = Candidate(1, Vector(3, 4, 2, 1), None)
    val candidate2 = Candidate(2, Vector(3, 2, 5), None)
    val candidate3 = Candidate(3, Vector(3, 2, 5, 1), None)
    val employer1 = Employer(1, Vector(1, 2, 3), 3)
    val employer2 = Employer(2, Vector(1, 3, 2), 1)
    val employer3 = Employer(3, Vector(2, 1, 3), 1)
    val employer4 = Employer(4, Vector(2, 1, 3), 2)
    val employer5 = Employer(5, Vector(3, 2, 1), 2)
    val candidates = Vector(candidate1, candidate2, candidate3)
    val employers = Vector(employer1, employer2, employer3, employer4, employer5)
    
    val results = PartyMatcher.matchParties(candidates, employers)
    val expected = Vector(Some(4), Some(3), Some(2))
    results.candidates.map(_.maybeMatch.map(_.id)) should contain theSameElementsInOrderAs expected
    
  }
}
