package com.example

object PartyMatcher {
  
  def chooseMostPreferredOffer(
    employersForThisPreference: Vector[Employer],
    candidate: Candidate): (Option[Candidate], Vector[Employer]) = {
    
      val indicesOfEmployersInCandidatesPreferences =
        employersForThisPreference.map(employer => candidate.preferences.indexOf(employer.id))
      val listOfEmployersInThisCandidatesPreferences = indicesOfEmployersInCandidatesPreferences.filter(_ > -1)
      if (listOfEmployersInThisCandidatesPreferences.nonEmpty) {
        val lowestNonNegativeIndex = listOfEmployersInThisCandidatesPreferences.min
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
      } else {
        (Some(candidate),
          employersForThisPreference.map { employer => employer.copy(preferences = employer.preferences.tail) })
      }
     
      
  }
  
  def matchParties(candidates: Vector[Candidate], employers: Vector[Employer]): SimulationResults = {
  
    if (employers.isEmpty) {
      SimulationResults(candidates.sortWith(_.id < _.id), employers.sortWith(_.id < _.id))
    } else {
      val remainingEmployers = employers.filter(_.preferences.nonEmpty)
      
      val candidatesAndEmployersAfterOffersConsidered = remainingEmployers
        .map{ employer =>
          val iDsOfCandidatesToMakeOffersTo = employer.preferences.take(employer.availablePositions)
          (iDsOfCandidatesToMakeOffersTo, employer)
        }
        .flatMap { case (iDsOfCandidatesToMakeOffersTo, employer) =>
          iDsOfCandidatesToMakeOffersTo.map { preference => (preference, employer) }
        }
        .groupBy{ case (iDOfCandidateToMakeOffersTo, _) => iDOfCandidateToMakeOffersTo }
        .map { case (iDOfCandidateToMakeOffersTo, candidateIDAndEmployersMakingAnOfferToThisCandidate) =>
          val employersMakingAnOfferToThisCandidate =
            candidateIDAndEmployersMakingAnOfferToThisCandidate.map { case (_, employer) => employer }
          val (candidatesAfterDecidingOnOffers, rejectedEmployers) =
            candidates
            .find(_.id == iDOfCandidateToMakeOffersTo)
            .fold[(Option[Candidate], Vector[Employer])]
              {(None, employersMakingAnOfferToThisCandidate)}
              { candidate =>
              chooseMostPreferredOffer(employersMakingAnOfferToThisCandidate, candidate)
          }
          (candidatesAfterDecidingOnOffers, rejectedEmployers)
        }.toVector
  
      val employersAfterHavingMadeOffers =
        candidatesAndEmployersAfterOffersConsidered
        .flatMap {  case (_, employersWithRemainingPreferences) => employersWithRemainingPreferences }
        .sortWith(_.id < _.id)
      val candidatesAfterDecidingOnOffers =
        candidatesAndEmployersAfterOffersConsidered
        .flatMap {  case (candidatesWithUpdatedMatches, _) => candidatesWithUpdatedMatches }
      
      val idsOfCandidatesThatDecidedOnOffers = candidatesAfterDecidingOnOffers.map(_.id)
      val allCandidates =
        (candidates.filterNot(candidate =>
          idsOfCandidatesThatDecidedOnOffers.contains(candidate.id)) ++ candidatesAfterDecidingOnOffers)
        .sortWith(_.id < _.id)
      
      matchParties(allCandidates, employersAfterHavingMadeOffers)
      
    }
    
  }
  
}

