rankhospital <- function(state,outcome,rank){
  #resultFrame <- data.frame("Hospital.Name" = character(), "Rate" = character(),"Rank" = numeric(),stringsAsFactors=FALSE)
  ##read data
  filename="outcome-of-care-measures.csv"
  fileData <- read.csv(filename,colClass="character")
  ## Check that state and outcome are valid
  ## set index to read correct column
  outcomeInt<-0
  if(outcome=="heart attack"){
    outcomeInt<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }else if(outcome=="heart failure") {
    outcomeInt<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }else if(outcome=="pneumonia") {
    outcomeInt<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  ## handle errornous input
  if(outcomeInt==0){
    stop("Invalid Outcome",call.=FALSE)
  }
  ## check rank value
  ## valid if number or beat/worst
  if(!checknum(rank)){
    ##if not a number ,  ck if it is best or worse
    if(rank!="best"&rank!="worst"){
      stop("Invalid Rank",call.=FALSE)
    }else{
      if(rank =="best"){
        rank<-1
      }else{
        rank<- -1
      }
    }
  }  
  ## set a list of valid states
  validStates<-c("TX","GU","WY","WI","AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","ND","NC","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
  ##check state and outcome are valid(no NA)
  matches <- match(state,validStates,nomatch=FALSE)
  if(!matches){
    stop("Invalid state",call.=FALSE)
  }
  ## extract data of correct state
  data <- fileData[fileData[,"State"]==state & !is.na(fileData[,outcomeInt]),]
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data[,outcomeInt] <- suppressWarnings(as.numeric(data[,outcomeInt])) ##set target column as numeric
  data<-data[!is.na(data[outcomeInt]),]
  data<-data[order(data[[outcomeInt]]),]
 
 ## resultFrame <- rbind(resultFrame,c(result["Hospital.Name"],result[,11],count))  ## add all the result rows into resultframe
 if(rank<0){
   rank<-nrow(data)
 }
 bestScore<-data[rank,outcomeInt]  ## get best outcome
 bestHosp<-data[data[outcomeInt]==bestScore,]
 bestHosp<-bestHosp[order(bestHosp["Hospital.Name"]),]
 result<-bestHosp[1,"Hospital.Name"]
 ## for(i in 1:rank){
 #  resultFrame [1,]<- result
  ##}
  #colnames(resultFrame)<-c("Hospital.Name","Rate","Rank")
  return(result)
}

checknum <- function(x) is.numeric(x) & !is.na(x)