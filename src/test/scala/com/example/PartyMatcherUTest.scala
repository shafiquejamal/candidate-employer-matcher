package com.example

import org.scalatest.{FlatSpecLike, ShouldMatchers}

class PartyMatcherUTest extends FlatSpecLike with ShouldMatchers {

  "The party matcher" should "update the choosers choice to choose its preferred candidate among all offers and the " +
  "current candidate choice" in {
    
    val candidate8 = Candidate(8, Vector(3, 4, 1))
    val candidate5 = Candidate(5, Vector(3, 2, 5))
    val candidate6 = Candidate(6, Vector(1, 2, 5))
    val candidate1 = Candidate(1, Vector(5, 2, 5))
    val candidates = Vector(candidate8, candidate5)
    val preference = 3
    val chooserNoChoiceYet = Chooser(3, Vector(7, 6, 5, 2, 8), None)
    val chooserNoChange = Chooser(3, Vector(7, 6, 5, 2, 8), Some(candidate6))
    val chooserChange = Chooser(3, Vector(7, 6, 8, 2, 5, 1), Some(candidate1))
    
    val (chooserNoPrevChoice, rejectedCandidates) = PartyMatcher.maybeUpdatedChooser(preference, candidates, chooserNoChoiceYet)
    chooserNoPrevChoice should contain(chooserNoChoiceYet.copy(maybeMatch = Some(candidate5)))
    val expectedRejectedCandidates = Vector(candidate8.copy(preferences = candidate8.preferences.tail))
    rejectedCandidates should contain theSameElementsAs expectedRejectedCandidates
    
    val (chooserKeepExistingChoice, _) = PartyMatcher.maybeUpdatedChooser(preference, candidates, chooserNoChange)
    chooserKeepExistingChoice should contain(chooserNoChange)
    
    val (chooserWithExistingGetsUpdated, _) = PartyMatcher.maybeUpdatedChooser(preference, candidates, chooserChange)
    chooserWithExistingGetsUpdated should contain(chooserChange.copy(maybeMatch = Some(candidate8)))
  }
  
  it should "Return stable matches when the preferences for candidates and choosers are properly formed " +
  "(no duplicated preferences, etc)" in {
  
    val chooser1 = Chooser(1, Vector(1, 3, 2), None)
    val chooser2 = Chooser(2, Vector(1, 2, 3), None)
    val chooser3 = Chooser(3, Vector(2, 3, 1), None)
    val candidate1 = Candidate(1, Vector(3, 2, 1))
    val candidate2 = Candidate(1, Vector(2, 3, 1))
    val candidate3 = Candidate(1, Vector(3, 1, 2))
    val choosers = Vector(chooser1, chooser2, chooser3)
    val candidates = Vector(candidate1, candidate2, candidate3)
    
    val results = PartyMatcher.matchParties(choosers, candidates)
    
  }

}
