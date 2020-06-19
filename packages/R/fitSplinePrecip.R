#' @title Spline fit precip data
#'
#' @author Roberti
#'
#' @details Generates spline fit  of precip data for use in the \link{detectClog} function
#'
#' @param data output from the parseForFitSpline function
#' @param wndw The spline fit window to use, in days. I set the wndw = 9; this seems to be the best fit from preliminary tests
#'
#' @return A list of data frames for the spline fit values, by date
#'
#' @export
#'


## Create function for Spline fitting precip data:
fitSpline<-function(data,wndw){
  #get the number of columns that need spline fits:
  cols.dat<-grep("throughfall|tipping|dfir",tolower(names(data)))
  #initialize empty DF:
  out.spline<-list()
  #run spline fits on each of the precip columns:
  for(i in 1:length(cols.dat)){
    #go thru and apply 5-day moving splines:
    int.spline<-list()
    for(j in 1:nrow(data)){
      grabRows<-j:(j+wndw)
      if(grabRows[length(grabRows)]<=nrow(data)){
        # Fit natural spline:
        fit.temp <- lm(data[grabRows,cols.dat[i]] ~ splines::ns(data[grabRows,grep("index",names(data))])) #knots = seq(0.01, 2, by = 0.1))
        # Get fitted values:
        fit.temp.values <- data.frame(suppressWarnings(predict(fit.temp,interval="prediction", level = 1 - 0.05)))
        #truncate data to between 0 and 1:
        fit.temp.values[fit.temp.values<0]<-0
        fit.temp.values[fit.temp.values>1]<-1
        #combine all the data into 1 df for outputting:
        int.spline[[j]]<-data.frame(date=data$Date[grabRows],orig=data[grabRows,cols.dat[i]],fit.temp.values)
      }#end if() statement
    }#end for loop: j
    #merge all spline fits:
    temp<-suppressWarnings(Reduce(function(x, y) merge(x, y, all=TRUE,by=c("date","orig")), int.spline))
    #Identify fit columns:
    getMedInd<-grep("fit",names(temp))
    #get the median values across ro
    med.spline.wndw<-apply(temp[,getMedInd], 1, function(x) median(x,na.rm = T))
    #cbind this with the original data and dates:
    out.spline[[i]]<-data.frame(date=temp$date,orig=temp$orig,fit=med.spline.wndw)
  }#end for loop: i
  #name the nested DFs:
  names(out.spline)<-names(data)[cols.dat]
  #return the list:
  return(out.spline)
}

#test2<-fitSpline(data = test,wndw = 9)
