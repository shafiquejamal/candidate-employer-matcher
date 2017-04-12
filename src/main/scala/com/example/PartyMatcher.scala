package com.example

object PartyMatcher {
  
  def maybeUpdatedCandidate(
    preference: Int,
    employersForThisPreference: Vector[Employer],
    candidate: Candidate): (Option[Candidate], Vector[Employer]) = {
    
      val indicesOfEmployersInCandidatesPreferences =
        employersForThisPreference.map(employer => candidate.preferences.indexOf(employer.id))
      val lowestNonNegativeIndex = indicesOfEmployersInCandidatesPreferences.filter(_ > -1).min
      val indexOfLowestNonNegativeIndex = indicesOfEmployersInCandidatesPreferences.indexOf(lowestNonNegativeIndex)
      val idOfNewMatchAmongEmployers = employersForThisPreference(indexOfLowestNonNegativeIndex).id
      val newMatchAmongEmployers = employersForThisPreference.find(_.id == idOfNewMatchAmongEmployers).get
    
      val maybeIndexOfCurrentMatch =
        candidate.maybeMatch.map { currentMatch => candidate.preferences.indexOf(currentMatch.id) }
    
      val (maybeUpdatedCandidate, rejectedEmployers) = maybeIndexOfCurrentMatch.fold {
        val rejectedEmployers =
          employersForThisPreference
          .filterNot(_.id == idOfNewMatchAmongEmployers)
          .map( employer => employer.copy(preferences = employer.preferences.tail))
        (Some(
          candidate.copy(
            maybeMatch =
              Some(newMatchAmongEmployers.copy(
                availablePositions = newMatchAmongEmployers.availablePositions - 1)))), rejectedEmployers)
      } { indexOfCurrentMatch =>
        if ((indexOfCurrentMatch < lowestNonNegativeIndex) && (indexOfCurrentMatch > -1)) {
          val rejectedEmployers =
            employersForThisPreference
            .map( employer => employer.copy(preferences = employer.preferences.tail))
          (Some(candidate), rejectedEmployers)
        } else {
          val rejectedEmployers =
            employersForThisPreference
            .filterNot(_.id == idOfNewMatchAmongEmployers)
            .map( employer => employer.copy(preferences = employer.preferences.tail)) ++
            candidate.maybeMatch.toVector.map(employer => employer.copy(preferences = employer.preferences.tail, availablePositions = employer.availablePositions + 1))
          (Some(candidate.copy(maybeMatch = Some(newMatchAmongEmployers.copy(availablePositions = newMatchAmongEmployers.availablePositions - 1)))), rejectedEmployers)
        }
      }
    (maybeUpdatedCandidate, rejectedEmployers)
  }
  
  def matchParties(candidates: Vector[Candidate], employers: Vector[Employer]): SimulationResults = {
  
    // If either all employer preferences is empty or all candidates have been matched, then we are done.
    // A candidate can be taken out when it has been matched to its top choice. The corresponding position can be removed as well
    if (employers.isEmpty) {
      SimulationResults(candidates.sortWith(_.id < _.id), employers.sortWith(_.id < _.id))
    } else {
      // consider only employers who still have unexamined preferences
      val remainingEmployers = employers.filter(_.preferences.nonEmpty)
      
      // group all employers with preferences by their current top preference
      // by group, for each employer with this common preference, update the candidate
      val updatedCandidatesAndEmployers = remainingEmployers
        .map{ employer => (employer.preferences.take(employer.availablePositions), employer) }
        .flatMap { case (preferencesToOffer, employer) => preferencesToOffer.map { preference => (preference, employer) }  }
        .groupBy{ case (preference, _) => preference }
        .map { case (preference, employersAndPreferenceForThisPreference) =>
          val employersForThisPreference = employersAndPreferenceForThisPreference.map { case (_, employer) => employer }
          // if there is no matching employer, return None
          //  Otherwise, update the candidates current match with its most preferred employer
          val (updatedCandidates, rejectedEmployers) =
            candidates
            .find(_.id == preference)
            .fold[(Option[Candidate], Vector[Employer])]{(None, employersForThisPreference)}{ candidate =>
              maybeUpdatedCandidate(preference, employersForThisPreference, candidate)
        }
        (updatedCandidates, rejectedEmployers)
      }.toVector
  
      val updatedEmployers =
        updatedCandidatesAndEmployers
        .flatMap {  case (_, employersWithRemainingPreferences) => employersWithRemainingPreferences }
        .sortWith(_.id < _.id)
      val updatedCandidates =
        updatedCandidatesAndEmployers
        .flatMap {  case (candidatesWithUpdatedMatches, _) => candidatesWithUpdatedMatches }
      
      val idsOfUpdatedCandidates = updatedCandidates.map(_.id)
      val allCandidates =
        (candidates.filterNot(candidate =>
          idsOfUpdatedCandidates.contains(candidate.id)) ++ updatedCandidates)
        .sortWith(_.id < _.id)
      
      matchParties(allCandidates, updatedEmployers)
      
    }
    
  }
  
}

