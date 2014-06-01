best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in the state with lowest 30-day death
  ## rate
 
  state <- toupper(state)
  outcome <- tolower(outcome)
  allowedOutcomes <- c("heart attack","heart failure", "pneumonia")
  
  if(!(outcome %in% allowedOutcomes))
  {
    stop("invalid outcome")
  }
  
  ## below are USPS state abbreiations, from http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations
  allowedStates <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","GU","MP","PR","VI","FM","MH","PW","AA","AE","AP","CM","CZ","NB","PI","TT")
  if(!(state %in% allowedStates))
  {
    stop("invalid state")
  }
  
  Hospital.Name <- ""
  
  ## read in the source data from disk
  sourceOutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
  ## pull out just the subset of outcome records that match the specified state
  targetOutcomes <- subset(sourceOutcomeData,sourceOutcomeData$State == state)
  
  ## further pull out just the subset of outcome records that match the specified outcome
  possibleHospitalOutcomes <- matrix()
  valueVector <- vector()
  if(outcome=="heart attack")
  {
    possibleHospitalOutcomes <- targetOutcomes
    suppressWarnings(valueVector <- as.numeric(possibleHospitalOutcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  }
  else if(outcome=="heart failure")
  {
    possibleHospitalOutcomes <- targetOutcomes
    valueVector <- suppressWarnings(as.numeric(possibleHospitalOutcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    
  }
  else if(outcome=="pneumonia")
  {
    possibleHospitalOutcomes <- targetOutcomes
    valueVector <- suppressWarnings(as.numeric(possibleHospitalOutcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  }
  else  
  {
    stop("invalid outcome")
  }
  
  ## identify the hospital with the best (lowest) mortality rates
  valueVector[is.na(valueVector)]  <- 101  ## simple trick to eliminate NA's from consideration without breaking the index
  i <- which.min(valueVector)
  Hospital.Name <- possibleHospitalOutcomes[i,2]
  
  ## Return the hospital's name
  Hospital.Name
}