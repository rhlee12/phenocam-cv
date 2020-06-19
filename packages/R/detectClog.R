#' @title Detect thrufall clog
#'
#' @author Roberti
#'
#' @details Summarizes spline fit output for clogged/not clogged.
#'
#' @param data the output of the \link{fitSpline} function
#'
#' @return Looks like you get a data frame of the date, sensor, and clog status.
#'
#' @export
#'

#This function takes output from the fitSplinePrecip() function and identifies partially and fully clogged precipitation gauges.

#create sliding window function for clog detection:
detectClog<-function(data){
  #go thru each precip collector and identify dates of full and partial clogs for precip collectors:
  clog.out<-list()
  for(i in 1:length(data)){
    #create a dataframe with 0s and dates:
    clog.out[[i]]<-data.frame(date=seq.Date(as.Date(data[[i]]$date[1]),as.Date(data[[i]]$date[nrow(data[[i]])]),by="day"))
    #initialize clogQF set to 0:
    clog.out[[i]]$clogQF<-rep(0,nrow(clog.out[[i]]))
    #add prcp column set to false:
    clog.out[[i]]$prcp<-rep(FALSE,nrow(clog.out[[i]]))
    #identify partial and full clog dates:
    clog.date.prtl<-data[[i]]$date[which(data[[i]]$fit<=0.3 & data[[i]]$fit>0.05)]
    clog.date.full<-data[[i]]$date[which(data[[i]]$fit<=0.05)]
    #add 1 for partial clog and 2 for full clog under clogQF:
    if(length(clog.date.prtl)!=0){
      clog.out[[i]]$clogQF[sapply(clog.date.prtl, function(x) grep(x,clog.out[[i]]$date))]<-1
    }
    if(length(clog.date.full)!=0){
      clog.out[[i]]$clogQF[sapply(clog.date.full, function(x) grep(x,clog.out[[i]]$date))]<-2
    }
    #replace FALSE with TRUE for rows in clog.out[[i]] for dates where precip actually occurred:
    clog.out[[i]]$prcp[sapply(data[[i]]$date, function(x) grep(x,clog.out[[i]]$date))]<-TRUE
    #go thru the time series and fill in 1's or 2's for clogged periods where precip wasn't falling:
    for(j in 2:nrow(clog.out[[i]])){
      #check current record for non precip day:
      if(clog.out[[i]]$prcp[j]==F){
        #check previous record; was it a precip day?
        #if(clog.out[[i]]$prcp[j-1]==T){
        #carry previous flag to today:
        clog.out[[i]]$clogQF[j]<-clog.out[[i]]$clogQF[j-1]
        #}
        #else{
        #
        #}
      }# end if statement
    }#end j loop
  }#end i loop
  #merge everything into one dataframe comprising dates and the clogQF of each sensor:
  temp<-suppressWarnings(Reduce(function(x, y) merge(x, y, all=TRUE,by=c("date","prcp")), clog.out))
  #rename the columns:
  names(temp)<-c("date","prcp",names(data))

  #return the dataframe:
  return(temp)
}

