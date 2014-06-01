rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
 
  
  state <- toupper(state)
  outcome <- tolower(outcome)
  allowedOutcomes <- c("heart attack","heart failure", "pneumonia")
  
  if(!(outcome %in% allowedOutcomes))
  {
    stop("invalid outcome")
  }
  
  if(num=="best")
  {
    num <- 1
  }
  
  ## below are USPS state abbreiations, from http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations
  allowedStates <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","GU","MP","PR","VI","FM","MH","PW","AA","AE","AP","CM","CZ","NB","PI","TT")
  if(!(state %in% allowedStates))
  {
    stop("invalid state")
  }
  
  locHospitalName <- ""
  
  ## read in the source data from disk
  sourceOutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## pull out just the subset of outcome records that match the specified state
  targetOutcomes <- subset(sourceOutcomeData,sourceOutcomeData$State == state)
  
  ## further pull out just the subset of outcome records that match the specified outcome
  possibleHospitalOutcomes <- data.frame()
  rankedHospitalOutcomes <- data.frame()
  vectorHospitalNames <- vector()
  vectorRates <- vector()
  if(outcome=="heart attack")
  {
    vectorHospitalNames <- targetOutcomes$Hospital.Name
    vectorRates <- suppressWarnings(as.numeric(targetOutcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    possibleHospitalOutcomes <- cbind.data.frame(vectorHospitalNames,vectorRates)
  }
  else if(outcome=="heart failure")
  {    
    vectorHospitalNames <- targetOutcomes$Hospital.Name
    vectorRates <- suppressWarnings(as.numeric(targetOutcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    possibleHospitalOutcomes <- cbind.data.frame(vectorHospitalNames,vectorRates)
  }
  else if(outcome=="pneumonia")
  {
    vectorHospitalNames <- targetOutcomes$Hospital.Name
    vectorRates <- suppressWarnings(as.numeric(targetOutcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    possibleHospitalOutcomes <- cbind.data.frame(vectorHospitalNames,vectorRates)
  }
  else  
  {
    stop("invalid outcome")
  }  
  
  colnames(possibleHospitalOutcomes) <- NULL
  rankedHospitalOutcomes <- possibleHospitalOutcomes[order(possibleHospitalOutcomes[,2],possibleHospitalOutcomes[,1],decreasing=FALSE),] 
  
  colnames(rankedHospitalOutcomes) <- NULL
  ##names(rankedHospitalOutcomes) <- make.names(seq(ncol(rankedHospitalOutcomes)))
  
  if(num == "worst")
  {
    j <- 1
    while(!is.na(rankedHospitalOutcomes[j,2]))
    {
      j <- j+1
    }
    num <- j-1
  }
  
  if(num>nrow(rankedHospitalOutcomes))
  {
      locHospitalName <- NA
  }
  else
  {
    locHospitalName <- as.matrix(rankedHospitalOutcomes)[num,1]
    ##rankedHospitalOutcomes  
    ##vectorHospitalNames
    ##possibleHospitalOutcomes
  }
    
  ## Return the hospital's name
  names(locHospitalName) <- NULL
  locHospitalName
}