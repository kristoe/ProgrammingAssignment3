rankall <- function(outcome, num = "best") 
{
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome <- tolower(outcome)
  allowedOutcomes <- c("heart attack","heart failure", "pneumonia")
  
  if(!(outcome %in% allowedOutcomes))
  {
    stop("invalid outcome")
  }
  
  numCounter <- num
  if(num=="best")
  {
    numCounter <- 1
  }
  
  ## below are USPS state abbreiations, from http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations
  allowedStates <- c("AL","VI","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","GU")
 
  ## read in the source data from disk
  sourceOutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character",stringsAsFactors=FALSE)
  vHospitalNames <- vector()
  vStates <- vector()
  vHospitalNames <- sourceOutcomeData$Hospital.Name
  vStates <- sourceOutcomeData$State
  
  ## build heart attack data frame
  dfHeartAttack <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
  vHeartAttackRates <- vector()
  vHeartAttackRates <- suppressWarnings(as.numeric(sourceOutcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  dfHeartAttack <- cbind.data.frame(vHospitalNames,vStates,vHeartAttackRates)
  
  
  ## build heart failure data frame
  dfHeartFailure <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
  vHeartFailureRates <- vector()
  vHeartFailureRates <- suppressWarnings(as.numeric(sourceOutcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  dfHeartFailure <- cbind.data.frame(vHospitalNames,vStates,vHeartFailureRates)  
  
  ## build pneumonia data frame
  dfPneumonia <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
  vPneumoniaRates <- vector()
  vPneumoniaRates <- suppressWarnings(as.numeric(sourceOutcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  dfPneumonia <- cbind.data.frame(vHospitalNames,vStates,vPneumoniaRates)
  
  ## pick which data frame to use
  dfOutcomes <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
  if(outcome=="heart attack")
  {
    dfOutcomes <- dfHeartAttack
    print("dfHeartAttack")
  }
  else if(outcome=="heart failure")
  {    
    dfOutcomes <- dfHeartFailure
    print("dfHeartFailure")
  }
  else if(outcome=="pneumonia")
  {
    dfOutcomes <- dfPneumonia
    ##print("dfPneumonia")
  }
  else  
  {
    stop("invalid outcome")
  }  
  
  ##print(nrow(dfOutcomes))
  
  dfResults <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
  for(i in allowedStates)
  {
    dfStateSubset <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
    dfRankedStateSubset <- data.frame(vHospitalNames=character(0), vStates=character(0), vRates=numeric(0),stringsAsFactors=FALSE)
    ##print(i)
    dfStateSubset <- dfOutcomes[dfOutcomes$vStates==i,]

    dfRankedStateSubset <- dfStateSubset[order(dfStateSubset[,3],dfStateSubset[,1],decreasing=FALSE),] 
    colnames(dfRankedStateSubset) <- NULL
    
    if(num == "worst")
    {
      j <- 1
      while(!is.na(dfRankedStateSubset[j,3]))
      {
        j <- j+1
      }
      numCounter <- j-1
    }
    
    if(numCounter>nrow(dfRankedStateSubset))
    {
      locHospitalName <- NA
    }
    else
    {
      locHospitalName <- as.matrix(dfRankedStateSubset)[numCounter,1]
##      print(locHospitalName)
    }
   

    ## add the hospital for the current state to the overall result set
    newRow <- vector()
    newRow <- c(vHospitalNames=locHospitalName,vStates=i,vRatings=num)    
    dfResults[nrow(dfResults)+1,] <- newRow
   
    rm(newRow)
    rm(locHospitalName)
    rm(dfStateSubset)
    rm(dfRankedStateSubset)
  }
  
  ## sort the final results overall on state
  dfResults <- dfResults[order(dfResults[,2],decreasing=FALSE),]
  
  ## return the results
  colnames(dfResults) <- c("hospital","state","rate")
  dfResults[,1:2]
  

}