#' @title Get NEON (TIS)Phenocam Image URLs
#'
#' @author Robert 'n' Josh
#'
#' @details Will return a list of URLs to phenocam images at a TIS site, for a given year and time of day.
#'
#' @param site The 4-Letter site code for a NEON site. Please, no OKSR.
#' @param year You know, the year. Optional, if a date is specified.
#' @param date A YYYY-MM-DD formatted date, to get a single pheno img url for. If date and year are specified, date is used.
#' @param time a character expression of the time in military time. No ":", so 1:00 PM is "1300" here. Images are captured every 15 mins, so please enter times in the form HH00, HH15, HH30, HH45, where HH is the hour.
#
#' @return A list of URLs to the understory phenocam images at the specified time. If no images was recorded on you specified time for a given day, no URL for that day will be returned.
#'
#' @export
#'


getPhenoUrls=function(site, year=NULL, date, time="1200",IR=T,domn=domain){
  library(Z10)
  #siteMeta=Z10::get.site.meta(site)
  # What domain is our site?
  
  #domn=Z10::get.site.meta(site)$domain.code
  
  #pad time to correct format, and make character
  time=stringr::str_pad(string = time, width = 4, side = "left", pad = "0")
  
  
  if(!is.null(year)&missing(date)){
    # Make month seq
    months=stringr::str_pad(string = 1:12, width = 2, side = "left", pad = "0")
    
    # gate days in month
    days=lapply(as.numeric(months), function(x) 1:lubridate::days_in_month(x)) %>% `names<-`(value=months)
    
    # build a string of month/day
    monthDays=c()
    for(month in months){
      monthDays=c(monthDays,
                  paste0(month,"/", stringr::str_pad(string = days[[month]], width = 2, side = "left", pad = "0")))
    }
    
    #if this is a NEON site:
    if(!is.na(domn)){
      baseUrl=paste0("https://phenocam.sr.unh.edu/webcam/browse/NEON.", domn, ".", site, ".DP1.00042/", year, "/", monthDays)
    }
    else{
      #if this is not a NEON site:
      baseUrl=paste0("https://phenocam.sr.unh.edu/webcam/browse/",site, "/", year, "/", monthDays)
    }
    
    #baseUrls=paste0("https://phenocam.sr.unh.edu/webcam/browse/NEON.", domn, ".", site, ".DP1.00042/", year, "/", monthDays)
    
    #JOSH CODE
    allUrls=c()
    for(url in baseUrls){
      rvest_doc <- xml2::read_html(url)
      
      timeUrls <- rvest::html_nodes(rvest_doc,"div") %>%
        rvest::html_nodes("img") %>%
        subset(., grepl(pattern = site, x=.)) %>%
        gsub(pattern = "thumbnails", replacement = "archive") %>%
        stringr::str_extract(pattern = "/.*.jpg") %>%
        paste0("https://phenocam.sr.unh.edu", .)
      
      timeUrls=timeUrls[grep(pattern = paste0("_", time), x = timeUrls)]
      #Add logic for infrared image:
      if(IR==T){
        timeUrl<-gsub("00042_","00042_IR_",timeUrls)
      }
      
      allUrls=c(allUrls, timeUrls)
    }
    message("Image downloaded for ",site, " on ", date, " ", time)
    return(allUrls)
  }else if(!missing(date)){
    
    #if this is a NEON site:
    if(!is.na(domn)){
      baseUrl=paste0("https://phenocam.sr.unh.edu/webcam/browse/NEON.", domn, ".", site, ".DP1.00042/", gsub(pattern = "-", replacement = "/", x = date))
    }
    else{
      #if this is not a NEON site:
      baseUrl=paste0("https://phenocam.sr.unh.edu/webcam/browse/",site, "/", gsub(pattern = "-", replacement = "/", x = date))
    }
    
    rvest_doc <- xml2::read_html(baseUrl)
    
    timeUrl <- rvest::html_nodes(rvest_doc,"div") %>%
      rvest::html_nodes("img") %>%
      subset(., grepl(pattern = site, x=.)) %>%
      gsub(pattern = "thumbnails", replacement = "archive") %>%
      stringr::str_extract(pattern = "/.*.jpg") %>%
      paste0("https://phenocam.sr.unh.edu", .)
    
    timeUrl=timeUrl[grep(pattern = paste0("_", time), x = timeUrl)]
    
    #Add logic for infrared image:
    if(IR==T){
      timeUrl<-gsub("00042_","00042_IR_",timeUrl)
    }
    
    if(length(timeUrl)==0){timeUrl=NA}
    message("Image URL obtained for ",site, " on ", date, " ", time)
    return(timeUrl)
  }
}
