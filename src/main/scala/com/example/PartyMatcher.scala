package com.example

object PartyMatcher {
  
  def maybeUpdatedChooser(
      preference: Int,
      candidatesForThisPreference: Vector[Candidate],
      chooser: Chooser): (Option[Chooser], Vector[Candidate]) = {
    
      val indicesOfCandidatesInChoosersPreferences =
        candidatesForThisPreference.map(candidate => chooser.preferences.indexOf(candidate.id))
      val lowestNonNegativeIndex = indicesOfCandidatesInChoosersPreferences.filter(_ > -1).min
      val indexOfLowestNonNegativeIndex = indicesOfCandidatesInChoosersPreferences.indexOf(lowestNonNegativeIndex)
      val idOfNewMatchAmongCandidates = candidatesForThisPreference(indexOfLowestNonNegativeIndex).id
      val newMatchAmongCandidates = candidatesForThisPreference.find(_.id == idOfNewMatchAmongCandidates).get
    
      val maybeIndexOfCurrentMatch =
        chooser.maybeMatch.map { currentMatch => chooser.preferences.indexOf(currentMatch.id) }
    
      val (maybeUpdatedChooser, rejectedCandidates) = maybeIndexOfCurrentMatch.fold {
        val rejectedCandidates =
          candidatesForThisPreference
          .filterNot(_.id == idOfNewMatchAmongCandidates)
          .map( candidate => candidate.copy(preferences = candidate.preferences.tail))
        (Some(chooser.copy(maybeMatch = Some(newMatchAmongCandidates))), rejectedCandidates)
      } { indexOfCurrentMatch =>
        if ((indexOfCurrentMatch < lowestNonNegativeIndex) && (indexOfCurrentMatch > -1)) {
          val rejectedCandidates =
            candidatesForThisPreference
            .map( candidate => candidate.copy(preferences = candidate.preferences.tail))
          (Some(chooser), rejectedCandidates)
        } else {
          val rejectedCandidates =
            candidatesForThisPreference
            .filterNot(_.id == idOfNewMatchAmongCandidates)
            .map( candidate => candidate.copy(preferences = candidate.preferences.tail)) ++
              chooser.maybeMatch.toVector.map(candidate => candidate.copy(preferences = candidate.preferences.tail))
          (Some(chooser.copy(maybeMatch = Some(newMatchAmongCandidates))), rejectedCandidates)
        }
      }
    (maybeUpdatedChooser, rejectedCandidates)
  }
  
  def matchParties(choosers: Vector[Chooser], candidates: Vector[Candidate]): SimulationResults = {
  
    // If either all candidate preferences is empty or all choosers have been matched, then we are done.
    // A chooser can be taken out when it has been matched to its top choice. The corresponding position can be removed as well
    if (candidates.isEmpty) {
      SimulationResults(choosers.sortWith(_.id < _.id), candidates.sortWith(_.id < _.id))
    } else {
      // consider only candidates who still have unexamined preferences
      val remainingCandidates = candidates.filter(_.preferences.nonEmpty)
      
      // group all candidates with preferences by their current top preference
      // by group, for each candidate with this common preference, update the chooser
      val updatedChoosersAndCandidates = remainingCandidates.groupBy(_.preferences.head)
        .map { case (preference, candidatesForThisPreference) =>
          // if there is no matching candidate, return None
          //  Otherwise, update the choosers current match with its most preferred candidate
          val (updatedChoosers, rejectedCandidates) =
            choosers.find(_.id == preference).fold[(Option[Chooser], Vector[Candidate])]{(None, candidatesForThisPreference)}{ chooser =>
            maybeUpdatedChooser(preference, candidatesForThisPreference, chooser)
        }
        (updatedChoosers, rejectedCandidates)
      }.toVector
  
      val updatedCandidates =
        updatedChoosersAndCandidates
        .flatMap {  case (_, candidatesWithRemainingPreferences) => candidatesWithRemainingPreferences }
        .sortWith(_.id < _.id)
      val updatedChoosers =
        updatedChoosersAndCandidates
        .flatMap {  case (choosersWithUpdatedMatches, _) => choosersWithUpdatedMatches }
      
      val idsOfUpdatedChoosers = updatedChoosers.map(_.id)
      val allChoosers =
        (choosers.filterNot(chooser => idsOfUpdatedChoosers.contains(chooser.id)) ++ updatedChoosers).sortWith(_.id < _.id)
      
      println("<-------------------------------------------------------------->")
      println("--------- all choosers ---------")
      allChoosers.foreach(println)
      println("--------- updated candidates ---------")
      updatedCandidates.foreach(println)
      
      matchParties(allChoosers, updatedCandidates)
      
    }
    
  }
  
}

