library(stringr)
library(plyr)
library(igraph)
`%>%` <- dplyr::`%>%`

source("utils.R")

# Returns a random score for a single year given the number of participants in that year
# The probability for each score is in accordance with the probabilities of the
# Eurovision voting system.
get_score = function(numParticipants, year){
  score = 0
  pos = sample(1:(numParticipants-1), 1)
  scores = c(12, 10, 8:1)
  if (year<2016) {
    # Jury and public votes are added together
    if(pos <= 10) {
      score = scores[pos]
    }
  } else {
    # Jury and public votes separately
    jscore = 0
    pscore = 0
    if(pos <= 10) {
      jscore = scores[pos]
    }
    pos = sample(1:(numParticipants-1), 1)
    if(pos <= 10) {
      pscore = scores[pos]
    }
    score = (pscore + jscore) / 2
  }
  return(score)
}

# Calculates the for ESC voting bias detection based on an alpha value using
# the algoritm of Gatherer
calculate_threshold = function(startYear, endYear, dfs, sampleSize=100000, alpha=0.05) {
  # Calculates the mean of one simulation run over a period of time (years) given 
  # a corresponding array of the number of participants in each year
  getSimulationMean = function(numParticipants, years) {
    scores = mapply(get_score, numParticipants, years)
    return(mean(scores))
  }
  
  # Get a vector all the years in the period
  # Exclude 2020 because there was no ESC
  ys = if (! 2020 %in% startYear:endYear) startYear:endYear else c(startYear:2019, 2021:endYear)
  
  # Determine the number of participants for each year
  numParticipants = unlist(lapply(dfs[paste("y", ys, sep="")], ncol))
  
  # Simulate sampleSize amount of times and save the mean scores
  meanScores = replicate(sampleSize, getSimulationMean(numParticipants=numParticipants, years=ys))
  
  # Sort these scores in decreasing order determine the threshold according to the
  # significance level alpha
  sortedMeans = sort(meanScores, decreasing=TRUE)
  threshold = min(sortedMeans[1:as.integer(sampleSize*alpha)]) 
  return(threshold)
}

# Checks if there is a collusion between two countries in a given period of time
check_collusion = function(country1, country2, startYear, endYear, dfs, threshold) {
  givenVotes = get_givenVotes(country1, country2, dfs, startYear, endYear)
  timePeriod = length(givenVotes)
  givenVotes = givenVotes[!is.na(givenVotes)] 
  
  # Similar to Svete/Hostnik (2020), we only take the countries into account
  # that participated more than 20% of the times together
  if (length(givenVotes) <= (timePeriod/5) ) {
    return(FALSE)
  }
  bias = mean(givenVotes) - threshold
  
  return(bias)
}


# Calculate the bias among all participating countries over a given period of time
# using Gatherer's algorithm
get_collusions = function(startYear, endYear, dfs, plot=FALSE, alpha=0.05) {
  cntrs = get_countryNames(dfs, startYear, endYear)
  
  threshold = calculate_threshold(startYear, endYear, dfs, sampleSize=100000, alpha=alpha)
  
  # Build all possible pairs and remove duplicates
  combinations = expand.grid(Giver = cntrs, Receiver = cntrs, stringsAsFactors=FALSE)
  combinations = combinations[combinations["Giver"] != combinations["Receiver"],]
  
  combinations$Bias = mapply(check_collusion, 
                             as.character(combinations[,"Giver"]),
                             as.character(combinations[,"Receiver"]), 
                             MoreArgs=
                               list(startYear=startYear,
                                    endYear=endYear,
                                    dfs=dfs,
                                    threshold=threshold),
                             USE.NAMES = FALSE)
  
  # Find out two way collusions
  collusions = combinations[combinations$Bias > 0,]
  collusions$pairName = mapply(get_standardizedPairName,
                               collusions$Giver, collusions$Receiver)
  collusions$twoWayCollusion = collusions$Bias > 0 & (duplicated(collusions$pairName) | duplicated(collusions$pairName, fromLast=TRUE))
  
  # Calculate the mean Bias for the two way collusions
  collusions = aggregate(Bias ~ pairName, data=collusions, mean) %>%
    merge(y=collusions, by="pairName") %>%
    rename(c("Bias.x"="meanBias", "Bias.y"="Bias"))
  
  if (plot) {
    # Visualize as graph using igraph library
    twoWayCollusions = collusions[collusions$twoWayCollusion & duplicated(collusions$pairName),c("Giver", "Receiver", "meanBias")]
    twoWayCollusions$Giver = sapply(twoWayCollusions$Giver, convert_toISO)
    twoWayCollusions$Receiver = sapply(twoWayCollusions$Receiver, convert_toISO)
    collusionGraph = graph_from_data_frame(d=twoWayCollusions, directed=FALSE)
    plot(collusionGraph, vertex.label=V(collusionGraph)$name, 
         vertex.size=15, 
         edge.width=E(collusionGraph)$meanBias*2)
  }
  
  return(collusions)
}

dfs = load_all()

# Consider all years
get_collusions(1975, 2023, dfs, plot=TRUE)

# Split into two blocks and analyse these separately because
# from 1993/1994 many new countries started participating in the ESC
get_collusions(1975, 1992, dfs, plot=TRUE)

# Significance 0.01 gives a lot cleaner results here
get_collusions(1993, 2023, dfs, plot=TRUE, alpha=0.01)
