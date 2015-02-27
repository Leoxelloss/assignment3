best <- function(state,outcome){
  ##read data
  filename="outcome-of-care-measures.csv"
  fileData <- read.csv(filename,colClass="character")
  ## set index to read correct column
  outcomeInt<-0
  if(outcome=="heart attack"){
    outcomeInt<-11
  }else if(outcome=="heart failure") {
    outcomeInt<-17
  }else if(outcome=="pneumonia") {
    outcomeInt<-23
  }
  ## handle errornous input
  if(outcomeInt==0){
    stop("invalid Outcome",call.=FALSE)
  }
  validStates<-c("TX","GU","WY","WI","AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
  ##check state and outcome are valid(no NA)
  matches <- match(state,validStates,nomatch=FALSE)
  if(!matches){
    stop("invalid state",call.=FALSE)
  }
  ## extract data of correct state
  data <- fileData[fileData[,"State"]==state & !is.na(fileData[,"Hospital.Name"]) & !is.na(fileData[,outcomeInt]),]

  ## Return hospital name in that state with lowest 30-day death
  ## rateif(!ma
  data[,outcomeInt] <- suppressWarnings(as.numeric(data[,outcomeInt]))
  ## result<-data[min(data[,outcomeInt],na.rm=TRUE),] ## extract row with lowes outcome
  data<-data[order(data[[outcomeInt]]),]
  # result<-data[1] 
  # result <-result[order("Hospital.Name"),] ## order the result
  bestScore<-data[1,outcomeInt]  ## get best outcome
  resultVector<-data[data[outcomeInt]==bestScore,"Hospital.Name"] ## return the name where out come are the best
  
  resultVector<-resultVector[!is.na(resultVector)]
  return(resultVector[[1]])
}