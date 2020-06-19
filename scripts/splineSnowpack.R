#spline fit for snowpack:
## Create function for Spline fitting precip data:
fitSpline<-function(data,wndw){
  #get the number of columns that need spline fits:
  cols.dat<-grep("percentsnow",tolower(names(data)))
  #only keep rows where percentsnow !=is.na()
  keepRows<-which(!is.na(data[,cols.dat]))
  data<-data[keepRows,]
  #add an index column:
  data$index<-1:nrow(data)
  #browser()
  #initialize empty DF:
  out.spline<-list()
  #run spline fits on each of the precip columns:
  for(i in 1:length(cols.dat)){
    #go thru and apply 5-day moving splines:
    int.spline<-list()
    for(j in 1:nrow(data)){
      grabRows<-j:(j+wndw)
      #browser()
      if(grabRows[length(grabRows)]<=nrow(data)){
        # Fit natural spline:
        #if(j==92)
        fit.temp <- lm(data[grabRows,cols.dat[i]] ~ ns(data[grabRows,grep("index",names(data))])) #knots = seq(0.01, 2, by = 0.1))
        # Get fitted values:
        fit.temp.values <- data.frame(suppressWarnings(predict(fit.temp,interval="prediction", level = 1 - 0.05)))
        #truncate data to between 0 and 1:
        fit.temp.values[fit.temp.values<0]<-0
        fit.temp.values[fit.temp.values>100]<-100
        #combine all the data into 1 df for outputting:
        int.spline[[j]]<-data.frame(date=data$date[grabRows],orig=data[grabRows,cols.dat[i]],fit.temp.values)
        #print(j)
      }#end if() statement
    }#end for loop: j
    #browser()
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

#open the files that Robert sent over:
data.heal<-readRDS("./Github/drizzle/snow-detection/HEAL_2019.rds")
data.cper<-readRDS("./Github/drizzle/snow-detection/CPER_2019_snowDepth.rds")

#spline fitting
out<-fitSpline(data = data.heal,wndw = 3)
plot(as.Date(out[[1]]$date),out[[1]]$orig,type="l")
lines(as.Date(out[[1]]$date),out[[1]]$fit,lwd=3,col="red")
