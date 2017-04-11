package com.example

object PartyMatcher {
  
  def maybeUpdatedChooser(
    preference: Int,
    employersForThisPreference: Vector[Employer],
    chooser: Chooser): (Option[Chooser], Vector[Employer]) = {
    
      val indicesOfEmployersInChoosersPreferences =
        employersForThisPreference.map(employer => chooser.preferences.indexOf(employer.id))
      val lowestNonNegativeIndex = indicesOfEmployersInChoosersPreferences.filter(_ > -1).min
      val indexOfLowestNonNegativeIndex = indicesOfEmployersInChoosersPreferences.indexOf(lowestNonNegativeIndex)
      val idOfNewMatchAmongEmployers = employersForThisPreference(indexOfLowestNonNegativeIndex).id
      val newMatchAmongEmployers = employersForThisPreference.find(_.id == idOfNewMatchAmongEmployers).get
    
      val maybeIndexOfCurrentMatch =
        chooser.maybeMatch.map { currentMatch => chooser.preferences.indexOf(currentMatch.id) }
    
      val (maybeUpdatedChooser, rejectedEmployers) = maybeIndexOfCurrentMatch.fold {
        val rejectedEmployers =
          employersForThisPreference
          .filterNot(_.id == idOfNewMatchAmongEmployers)
          .map( employer => employer.copy(preferences = employer.preferences.tail))
        (Some(chooser.copy(maybeMatch = Some(newMatchAmongEmployers))), rejectedEmployers)
      } { indexOfCurrentMatch =>
        if ((indexOfCurrentMatch < lowestNonNegativeIndex) && (indexOfCurrentMatch > -1)) {
          val rejectedEmployers =
            employersForThisPreference
            .map( employer => employer.copy(preferences = employer.preferences.tail))
          (Some(chooser), rejectedEmployers)
        } else {
          val rejectedEmployers =
            employersForThisPreference
            .filterNot(_.id == idOfNewMatchAmongEmployers)
            .map( emplo => emplo.copy(preferences = emplo.preferences.tail)) ++
              chooser.maybeMatch.toVector.map(employer => employer.copy(preferences = employer.preferences.tail))
          (Some(chooser.copy(maybeMatch = Some(newMatchAmongEmployers))), rejectedEmployers)
        }
      }
    (maybeUpdatedChooser, rejectedEmployers)
  }
  
  def matchParties(choosers: Vector[Chooser], employers: Vector[Employer]): SimulationResults = {
  
    // If either all employer preferences is empty or all choosers have been matched, then we are done.
    // A chooser can be taken out when it has been matched to its top choice. The corresponding position can be removed as well
    if (employers.isEmpty) {
      SimulationResults(choosers.sortWith(_.id < _.id), employers.sortWith(_.id < _.id))
    } else {
      // consider only employers who still have unexamined preferences
      val remainingEmployers = employers.filter(_.preferences.nonEmpty)
      
      // group all employers with preferences by their current top preference
      // by group, for each employer with this common preference, update the chooser
      val updatedChoosersAndEmployers = remainingEmployers.groupBy(_.preferences.head)
        .map { case (preference, employersForThisPreference) =>
          // if there is no matching employer, return None
          //  Otherwise, update the choosers current match with its most preferred employer
          val (updatedChoosers, rejectedEmployers) =
            choosers.find(_.id == preference).fold[(Option[Chooser], Vector[Employer])]{(None, employersForThisPreference)}{ chooser =>
            maybeUpdatedChooser(preference, employersForThisPreference, chooser)
        }
        (updatedChoosers, rejectedEmployers)
      }.toVector
  
      val updatedEmployers =
        updatedChoosersAndEmployers
        .flatMap {  case (_, employersWithRemainingPreferences) => employersWithRemainingPreferences }
        .sortWith(_.id < _.id)
      val updatedChoosers =
        updatedChoosersAndEmployers
        .flatMap {  case (choosersWithUpdatedMatches, _) => choosersWithUpdatedMatches }
      
      val idsOfUpdatedChoosers = updatedChoosers.map(_.id)
      val allChoosers =
        (choosers.filterNot(chooser => idsOfUpdatedChoosers.contains(chooser.id)) ++ updatedChoosers).sortWith(_.id < _.id)
      
      println("<-------------------------------------------------------------->")
      println("--------- all choosers ---------")
      allChoosers.foreach(println)
      println("--------- updated employers ---------")
      updatedEmployers.foreach(println)
      
      matchParties(allChoosers, updatedEmployers)
      
    }
    
  }
  
}

