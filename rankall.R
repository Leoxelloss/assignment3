rankall <- function(outcome,num="best"){
  #resultFrame <- data.frame("Hospital.Name" = character(), "Rate" = character(),"Rank" = numeric(),stringsAsFactors=FALSE)
  resultFrame<-data.frame("hospital"=character(),"state"=character(),stringsAsFactors=FALSE)
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
  rank<-num
  ## check rank valuewd()
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
  validStates<-c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","ND","NC","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
 
  ## extract data of correct state
  data <- fileData[!is.na(fileData[,outcomeInt]),]
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data[,outcomeInt] <- suppressWarnings(as.numeric(data[,outcomeInt])) ##set target column as numeric
  
  ## For each state, find the hospital of the given rank
  for(state in validStates){
    stateData<-data[data["State"]==state,]
    
    stateData<-stateData[order(stateData[[outcomeInt]]),]
   # if(rank<0){
   #   rank<-nrow(stateData)
    #}
    if(rank>nrow(stateData)){
   #   resultFrame<-rbind(resultFrame,data.frame("hospital"="<NA>","state"=state))## set result into result frame
      ## set result into result frame
    }else{
     besthosp <- rankhospital(state,outcome,num)
 #   bestScore<-stateData[rank,outcomeInt]  ## get best outcome
   #bestHosps<-stateData[stateData[outcomeInt]==bestScore & !is.na(stateData[outcomeInt]),]## get best shop with the best score
   # bestStateHosp<-bestHosps[1,"Hospital.Name"]## get the best state hosp
    resultFrame<-rbind(resultFrame,data.frame("hospital"=besthosp,"state"=state))## set result into result frame
   }
  }
 # colnames(resultFrame)<-c("Hospital.Name","State")

  return(resultFrame)
}

checknum <- function(x) is.numeric(x) & !is.na(x)