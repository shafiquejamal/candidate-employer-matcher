package com.example

import org.scalatest.{FlatSpecLike, ShouldMatchers}

class PartyMatcherUTest extends FlatSpecLike with ShouldMatchers {

  "The party matcher" should "update the choosers choice to choose its preferred candidate among all offers and the " +
  "current candidate choice" in {
    
    val candidate1 = Candidate(8, Vector(3, 4, 1), None)
    val candidate2 = Candidate(5, Vector(3, 2, 5), None)
    val candidates = Vector(candidate1, candidate2)
    val preference = 3
    val chooserNoChoiceYet = Chooser(3, Vector(7, 6, 5, 2, 8), None)
    val chooserNoChange = Chooser(3, Vector(7, 6, 5, 2, 8), Some(6))
    val chooserChange = Chooser(3, Vector(7, 6, 8, 2, 5, 1), Some(1))
    
    val chooserFirstChoice = PartyMatcher.maybeUpdatedChooser(preference, candidates, chooserNoChoiceYet)
    chooserFirstChoice should contain(chooserNoChoiceYet.copy(maybeMatch = Some(5)))
    
    val chooserKeepExistingChoice = PartyMatcher.maybeUpdatedChooser(preference, candidates, chooserNoChange)
    chooserKeepExistingChoice should contain(chooserNoChange)
    
    val chooserWithExistingGetsUpdated = PartyMatcher.maybeUpdatedChooser(preference, candidates, chooserChange)
    chooserWithExistingGetsUpdated should contain(chooserChange.copy(maybeMatch = Some(8)))
  }

}
