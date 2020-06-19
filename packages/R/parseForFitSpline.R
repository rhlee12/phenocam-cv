#' @title Reformat precip data for spline fitting
#'
#' @author Roberti
#'
#' @details Prepares precip data for use in the \link{fitSplinePrecip} function
#'
#' @param data data in the format of the siteData parameter from the Drizzle App.
#'
#' @return A data frame to be used in the fitSplinePrecip function
#'
#' @export
#'

#make into DF:
#siteData<-data.frame(do.call(rbind,all.Data.site[unlist(keepInd)]),stringsAsFactors = F,row.names = NULL)

parseForFitSpline<-function(data=NULL){
  #make the data wide:
  wide_data<-reshape2::dcast(data, Date ~ Stream, value.var="Value")
  #replace any NAs with 0s:
  wide_data[is.na(wide_data)]<-0

  #go thru each row and identify clogs:
  strms.clog.lst<-list()
  strms.rtio.lst<-list()
  for(j in 1:nrow(wide_data)){
    #identify how many streams are showing no data:
    noPrcpRtio<-length(which(wide_data[j,2:ncol(wide_data)]<=0.05))/length(2:ncol(wide_data))
    #create dataframe row with just the date and 0s:
    strms.clog.lst[[j]]<-data.frame(t(c(as.character(wide_data$Date[j]),rep(0,length(2:ncol(wide_data))))))
    #create dataframe row of prcp ratios:
    strms.rtio.lst[[j]]<-data.frame(t(c(as.character(wide_data$Date[j]),rep(0,length(2:ncol(wide_data))))))
    #check for clogs:
    if(noPrcpRtio<1){
      #identify streams that do have precip:
      meanPrcp<-mean(as.numeric(wide_data[j,2:ncol(wide_data)]))

      #loessMod10<-loess(strms.rtio.df[,2] ~ index, data=strms.rtio.df, span=0.10)


      if(meanPrcp > 0.1){#need to make sure there's trace precip across the sensors:
        #identify the collector with the most precip: (add 1 to account for date col offset)
        prcp.maxInd<-which.max(wide_data[j,2:ncol(wide_data)])+1
        #get the ratio of the precip collected by each sensor to the max value:
        prcp.rtio<-round(wide_data[j,2:ncol(wide_data)]/wide_data[j,prcp.maxInd],3)
        #add in precip ratios:
        strms.rtio.lst[[j]][,2:ncol(strms.rtio.lst[[j]])]<-prcp.rtio
        #identify which ratios are less than N% of the max recorded precip for that day:
        ## <= 30% PARTIAL CLOG:
        clog.prtl<-which(prcp.rtio<=0.3)+1
        ## <= 10% FULL CLOG:
        clog.total<-which(prcp.rtio<=0.1)+1

        if(length(clog.prtl)!=0){
          #substitute these indices with 1s to denote a partial clog:
          strms.clog.lst[[j]][clog.prtl]<-1
        }
        if(length(clog.total)!=0){
          #substitute these indices with 2s to denote a total clog:
          strms.clog.lst[[j]][clog.total]<-2
        }

      }
    }
  }
  #combine all the data into a dfs:
  strms.clog.df<-data.frame(do.call(rbind,strms.clog.lst))
  strms.rtio.df<-data.frame(do.call(rbind,strms.rtio.lst))
  #browser()
  #add names:
  names(strms.clog.df)<-names(wide_data)
  names(strms.rtio.df)<-names(wide_data)

  #add in logic for sites that only have one stream.  Won't be able to clog detect here:
  #browser()
  if(length(2:ncol(strms.rtio.df))>1){
    #add extra column that identifies if precip was present:
    strms.clog.df$prcp<-ifelse(apply(strms.rtio.df[,2:ncol(strms.rtio.df)],1, function(x) all(x<=0.05)),FALSE,TRUE)
    strms.rtio.df$prcp<-ifelse(apply(strms.rtio.df[,2:ncol(strms.rtio.df)],1, function(x) all(x<=0.05)),FALSE,TRUE)

    ### Just keep rows with recorded precip:
    prcp.df<-strms.rtio.df[which(strms.rtio.df$prcp==T),]
    #made an index column:
    prcp.df$index<-1:nrow(prcp.df)
    return(prcp.df)
  }
  # else{
  #   prcp.df<-c()
  # }

}

#test<-parseForFitSpline(data = siteData)
#the output of this feeds into the fitSplinePrecip.R function:
