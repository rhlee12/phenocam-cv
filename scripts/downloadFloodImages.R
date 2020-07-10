#' @title Function to download phenocam imagery at any Phenocam site
#'
#' @author Josh 
#'
#' @details Will download and save phenocam imagery at any phenocam site for images captured near sunrise, solar noon, or sunset
#'
#' @param picSite The 4-Letter site code for a NEON site. Please, no OKSR.
#' @param dateStart start date of phenocam images to download
#' @param dateEnd end date of phenocam images to download
#' @param picTime a character expression of either 'sunrise', 'solarNoon', or 'sunset'.  The code will download the picture(s) dynamically as a function of the specified picTime.  
#'                For example, If 'sunset' is chosen, the images downloaded will be within +/- 15 minutes of local sunset time. 
#' @param picIR If TRUE, the images downloaded will be infrared (IR).  If FALSE, the images downloaded will be visible light, RGB.
#' @param rerun if TRUE, images will be downloaded regardless of whether or not they've already been downloaded and stored
#' @param useGMT if TRUE, images will be downloaded using GMT time.  If FALSE, images will be downloaded using local standard time (LST)
#
#' @return A list of URLs to the understory phenocam images at the specified time. If no images was recorded on you specified time for a given day, no URL for that day will be returned.
#'
#' @export
#'


## Example of image classifications from https://rstudio-pubs-static.s3.amazonaws.com/236125_e0423e328e4b437888423d3821626d92.html
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")

#install.packages("https://s3.ca-central-1.amazonaws.com/jeremiedb/share/mxnet/CPU/mxnet.zip", repos = NULL)

#Define getSeason function:
# getSzn<-function(x){
#   browser()
#   yq <- as.yearqtr(as.yearmon(x, "%m/%d/%Y") + 1/12)
#   DF$Season <- factor(format(yq, "%q"), levels = 1:4, 
#                       labels = c("winter", "spring", "summer", "fall"))
# }


CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

downloadFloodImages<-function(picSite="DELA",dateStart="2019-01-01",dateEnd="2019-12-31",picTime="solarNoon",
                        picIR=F,rerun=F,useGMT=F){
  #set seed for traceability:
  library(magrittr)
  library(rvest)
  library(reshape2)
  library(lubridate)
  set.seed(2020)
  
  #check to see if any of the data are already saved (don't want to download again)
  #sequence of dates to run:
  getDates<-seq.Date(as.Date(dateStart),as.Date(dateEnd),by="day")
  filenameSeq<-paste0("img_small_",picSite,"_",getDates,"_",picTime,".rds")
  #list the files in the directory:
  alreadyRun.raw<-sapply(filenameSeq, function(x) grep(x,list.files(path = "C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/")))
  alreadyRun.cln<-names(alreadyRun.raw)[which(unlist(alreadyRun.raw)!=0)]
  if(length(alreadyRun.cln)!=0){
    #identify the location of the DATE within the filename:
    ind.date.file<-gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",alreadyRun.cln)[[1]][1]
    #make new list that defines what dates still need to be run:
    getImageDates<-getDates[!getDates %in% as.Date(substr(alreadyRun.cln,ind.date.file,ind.date.file+9))]
  }
  else{
    getImageDates<-getDates
  }
  
  #are we looking at a NEON site or a non-NEON site:
  NEONcheck<-grep("[A-Z]{4}",picSite)
  
  #filenameRawData<-paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/images_",picSite,"_",dateStart,"_",dateEnd,"_",picTime,".rds")
  #filenameTruncData<-paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/data_",picSite,"_",dateStart,"_",dateEnd,"_",picTime,".rds")
  #if(file.exists(filenameRawData)){
  
  #If any images need to be downloaded:
  if(length(getImageDates)!=0){
    #source the getPhenoCam function:
    source('C:/Users/jroberti/Git/phenocam-cv/scripts/getPhenoUrls.R', echo=TRUE)
    #download a bunch of images, flooded and non-flooded from the phenocam network (full year of data from Dead Lake):
    dateSeq<-getImageDates
    #seq.Date(from = as.Date(dateStart),to = as.Date(dateEnd),by = "day")
    #check to see if a NEON site was entered:
    if(length(NEONcheck)!=0){
      #open the NEON field site csv to get the lat and lon info:
      metadata<-read.csv('C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/fieldSitesNEON.csv',header = T,stringsAsFactors = F)
      #find the site of interest
      metaSite<-metadata[grep(picSite,metadata$Site.ID),]
      siteLat<-metaSite$Latitude
      siteLon<-metaSite$Longitude
      
    }
    else{
      #get the site's lat and lon directly from the phenocam website:
      metaSite<-read_html(paste0("https://phenocam.sr.unh.edu/webcam/sites/",picSite,"/"))
      #get the lat / lon of the site:
      info.site<-metaSite %>%
        html_nodes("script")
      #remove html tags from the information:
      info.site<-gsub("<.*?>", "", info.site[1])
      lat.ind.start<-gregexpr(pattern ="lat",info.site)[[1]][1]
      lon.ind.start<-gregexpr(pattern ="lon",info.site)[[1]][1]
      #truncate the string from lat.ind.start to lon.ind.start + (lon.ind.start-lat.ind.start)
      stringLatLon<-substr(info.site,lat.ind.start,lon.ind.start+(lon.ind.start-lat.ind.start))
      #get the lat lon values:
      latLon.drty<-trimws(gsub("[A-z]|;|\n|=","",stringLatLon),"both")
      #assign lat, lon:
      siteMeta<-data.frame(t(data.frame(strsplit(x = latLon.drty,split = "\\s+"))),row.names = NULL,stringsAsFactors = F)
      names(siteMeta)<-c("lat","lon")
      #siteMeta<-as.character(siteMeta)
      siteLat<-as.numeric(siteMeta$lat)
      siteLon<-as.numeric(siteMeta$lon)
    }
    #for each date, get the sunset time:
    sunTimes<-read_html(paste0("https://www.esrl.noaa.gov/gmd/grad/solcalc/table.php?lat=",siteLat,"&lon=",siteLon,"&year=",substr(dateStart,0,4)))
    #scrape the webpage and get the applicable sun info:
    info.sun<-sunTimes %>%
      html_nodes("table") %>%
      html_table()
    #name the tables:
    names(info.sun)<-c("sunrise","sunset","solarNoon")
    #go thru each table and reshape from short-wide DF to long-tall DF:
    info.sun2<-melt(info.sun, id.vars="Day")
    #split back into individual dataframes:
    info.sun.lst<-split(info.sun2, info.sun2$L1)
    #go thru each dataframe, clean up the times, correct for daylight saving, get GMT time:
    for(i in 1:length(info.sun.lst)){
      #if(i==2){browser()}
      #only keep rows with legit dates:
      keepRows<-which(nchar(info.sun.lst[[i]]$value)!=0)
      info.sun.lst[[i]]<-info.sun.lst[[i]][keepRows,]
      #create a date format similar to the one in dateSeq.vec:
      info.sun.lst[[i]]$date<-as.Date(paste0(as.character(info.sun.lst[[i]]$variable),"-",as.numeric(info.sun.lst[[i]]$Day),"-",substr(dateStart,0,4)),format = "%b-%d-%Y")
      #create a new column, time_GMT, that includes offset:
      offsetTZ<-sunTimes %>%
        html_nodes("h4")
      #clean and convert to numeric absolute value:
      offsetTZ<-abs(as.numeric(gsub("[A-z]|/|:","",gsub("<.*?>","",offsetTZ[1]))))
      #make sure the value column is only hh:mm and not a combo of hh:mm and hh:mm:ss
      info.sun.lst[[i]]$value<-substr(info.sun.lst[[i]]$value,0,5)
      #is this a non NEON site?  If So, round images to 15 minutes:
      if(length(NEONcheck)!=0){
        #round the time to the nearest 15 minutes:
        info.sun.lst[[i]]$roundValue<-format(lubridate::round_date(as.POSIXct(info.sun.lst[[i]]$value,format="%H:%M"),unit = "15 minutes"),"%H:%M")
      }
      else{
        #if not, round to 20 and 50 minutes past the hour:
        info.sun.lst[[i]]$roundValue<-format(lubridate::round_date(as.POSIXct(info.sun.lst[[i]]$value,format="%H:%M"),unit = "30 minutes"),"%H:%M")
        info.sun.lst[[i]]$roundValue<-gsub(":00",":50",info.sun.lst[[i]]$roundValue)
        info.sun.lst[[i]]$roundValue<-gsub(":30",":20",info.sun.lst[[i]]$roundValue)
      }
      
      #identify where daylight saving dates: (for some reason some times aren't exactly at 3600, 1 hour, diff)
      index.DT<-which(diff(as.numeric(hm(info.sun.lst[[i]]$value)))>=3400)
      index.ST<-which(diff(as.numeric(hm(info.sun.lst[[i]]$value)))<=-3400)
      #correct the GMT times using the daylight and standard time indices:
      fixTimes<-seq(index.DT+1,index.ST-1,1)
      #convert all $value cols to hm() class in lubridate:
      info.sun.lst[[i]]$time_ST<-hm(info.sun.lst[[i]]$roundValue)
      #subtract 1 hour from the daylight saving sequence values and GMT_times:
      info.sun.lst[[i]]$time_ST[fixTimes]<-info.sun.lst[[i]]$time_ST[fixTimes]-hours(1)
      #create GMT time:
      info.sun.lst[[i]]$time_GMT<-info.sun.lst[[i]]$time_ST+hours(offsetTZ)
      #create another column that defines what image time to pull based on sunrise, solar noon, or sunset:
      #if(info.sun.lst[[i]]$L1[1]=="solarNoon"){
      
      if(useGMT==T){
        #set the picture time to applicable GMT time:
        info.sun.lst[[i]]$picTime<-info.sun.lst[[i]]$time_GMT
      }
      else{
        #else set picture time to local standard time:
        info.sun.lst[[i]]$picTime<-info.sun.lst[[i]]$time_ST
      }
      #convert the picTime column to a "HHmm" format: (need this for accessing phenoCam URLs)
      lubTime<-hm(gsub(" 0S","",info.sun.lst[[i]]$picTime))
      charMin<-minute(lubTime)
      fixMinutes<-which(nchar(minute(lubTime))==1)
      charMin[fixMinutes]<-paste0("0",minute(lubTime)[fixMinutes])
      info.sun.lst[[i]]$picTimeHHMM<-paste0(hour(lubTime),charMin)
      #truncate the dates to those in dateSeq:
      info.sun.lst[[i]]<-merge(data.frame(dateSeq),info.sun.lst[[i]],by.x="dateSeq",by.y="date")
    }
    #melt the dataframes back:
    info.sun.df<-data.frame(do.call(rbind,info.sun.lst),row.names = NULL)
    
    #correct for times >=2400:
    info.sun.df$picTimeHHMM[grep("24H",info.sun.df$picTime)]<-"2359"
    
    
    #browser()
    
    #assign seasons to the dates:
    # getSzn()
    
    
    #
    #assign season to date:
    #dateSeq.df<-data.frame(date=dateSeq.vec,season=NA)
    #dateSeq<-seq.Date(from = as.Date("2018-12-30"),to = as.Date("2019-12-31"),by = "day")
    
    
    #subset the dataframe to the correct time of day: sunrise, solarNoon, or sunset:
    info.sun.use<-info.sun.df[grep(picTime,info.sun.df$L1),]
    
    #define the sites domain (if NEON site)
    #browser()
    if(length(NEONcheck)!=0){
      domain<-metaSite$Domain.Number
    }
    else{
      domain<-NA
    }
    imagePath<-list()
    #browser()
    for(j in 1:length(info.sun.use$picTimeHHMM)){
      imagePath[[j]]<-getPhenoUrls(site = picSite,year = substr(dateStart,0,4),date = info.sun.use$date[j],
                                   time = info.sun.use$picTimeHHMM[j],IR=picIR,domn=domain)
      #pause so I don't overload system:
      CatchupPause(1)
    }
    #browser()
    
    #imagePath<-lapply(dateSeq, function(x) getPhenoUrls(site = picSite,year = substr(dateStart,0,4),date = x,time = picTime,IR=picIR))
    #browser()
    #load a sample image:
    sampleImg<-imager::load.image(imagePath[!is.na(imagePath)][[1]])
    #set the height and width of the image:
    width<-round(0.2 * dim(sampleImg)[1],0)
    height<-round((dim(sampleImg)[2]/dim(sampleImg)[1]*width),0)
    
    #go thru each image, convert to grayscale, change dimensions, and save
    for(i in 1:length(imagePath[!is.na(imagePath)])){ #length(imagePath[!is.na(imagePath)])
      #browser()
      ## Download the image
      img_raw<-imager::load.image(imagePath[!is.na(imagePath)][[i]])
      ## convert the grayscale image size to something smaller:
      img_small <- imager::resize(img_raw, size_x = width, size_y =  height)
      ### SAVE THE img_small and img_matrix for future analysis:
      saveRDS(object = img_small, file = paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/img_small_",picSite,"_",info.sun.use$date[i],"_",picTime,".rds"))
      #saveRDS(object = img_small, file = paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/img_small_",picSite,"_",info.sun.use$date[i],"_",picTime,".rds"))
      #pause so I don't overload system:
      CatchupPause(1)
      #plot image and get date for title:
      #date_plot<-stringr::str_sub(imagePath[!is.na(imagePath)][[i]],-21,-12)
      #plot(grayimg[[i]],main=date_plot)
      
      #grayimg@.Data
      ## Get the image as a matrix
      #img_matrix <- as.matrix(img_small)
      
      ## Coerce to a vector
      #img_vector[[i]] <- as.vector(t(img_matrix))
    }#end for loop
  }#end if
}#end function
  
