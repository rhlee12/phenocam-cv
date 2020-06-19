# Packages needed:
# - tesseract: OCR in R
#   (This package may be tricky to install. Need the poppler and the tesseract OS libraries on machine 
#   (on Mac, brew install poppler, brew install tesseract CentOS: sudo yum install poppler-cpp-devel, sudo yum install tesseract-devel leptonica-devel))
# - magick: image manipulation in R
# - Z10: get NEON data and stuff. Written by this cool cat
# - rgdal
#   (This package may be tricky to install. need gdal on OS library)
#   (on Mac, brew install gdal, CentOS: sudo yum install gdal)
#
# We love pipes
library(magrittr)
library(rvest)
library(RCurl)
library(XML)
#Get latest phenocam data

#EW WD
setwd("~/GitHub/drizzle/")

year="2019"

#siteList=c("YELL", "RMNP", "DCFS", "WOOD", "TEAK", "WREF", "BONA", "HEAL", "DEJU", "ABBY", "STEI", "UNDE", "HARV", "BART", "SCBI", "SERC", "UKFS", "SJER", "SOAP", "UKFS")
siteList=ytBangerz::siteInfo$SiteCode[ytBangerz::siteInfo$SiteType=="TER"]

getSiteNoonUrls=function(site, year){
  
  months=stringr::str_pad(string = 1:12, width = 2, side = "left", pad = "0")
  #siteMeta=Z10::get.site.meta(site)
  # What domain is our site?
  domn=Z10::get.site.meta(site)$domain.code
  
  baseUrls=paste0("https://phenocam.sr.unh.edu/webcam/browse/NEON.", domn, ".", site, ".DP1.00042/", year, "/", months)
  
  #JOSH CODE
  allUrls=c()
  for(url in baseUrls){
    rvest_doc <- xml2::read_html(url)
    
    noonUrls <- rvest::html_nodes(rvest_doc,"div") %>%  
      rvest::html_nodes("td") %>%  
      rvest::html_nodes("img") %>% 
      gsub(pattern = "thumbnails", replacement = "archive") %>%
      stringr::str_extract(pattern = "/.*.jpg") %>%
      paste0("https://phenocam.sr.unh.edu/", .)
    
    noonUrls=noonUrls[grepl(pattern = "_12000", x = noonUrls)]
    
    allUrls=c(allUrls, noonUrls)
  }
  return(allUrls)
}

getPhenoUrls=function(site, year, time="1200"){
  #pad time to correct format, and make character
  time=stringr::str_pad(string = time, width = 4, side = "left", pad = "0")

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
  
  #siteMeta=Z10::get.site.meta(site)
  # What domain is our site?
  domn=Z10::get.site.meta(site)$domain.code
  
  baseUrls=paste0("https://phenocam.sr.unh.edu/webcam/browse/NEON.", domn, ".", site, ".DP1.00042/", year, "/", monthDays)
  
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
    
    allUrls=c(allUrls, timeUrls)
  }
  return(allUrls)
}

# Let's work on understory
#latestImgUrl=paste0("https://phenocam.sr.unh.edu/data/latest/NEON.", domn,".", site ,".DP1.00042.jpg")
#latestImgUrl="https://phenocam.sr.unh.edu/data/archive/NEON.D19.HEAL.DP1.00042/2020/04/NEON.D19.HEAL.DP1.00042_2020_04_15_120006.jpg"
#latestImgUrl="https://phenocam.sr.unh.edu/data/archive/NEON.D19.HEAL.DP1.00042/2019/06/NEON.D19.HEAL.DP1.00042_2019_06_21_081505.jpg"
# LOAD EM UP
detectSnow=function(imagePath, rgSpan_Threshold=0.25, rbSpan_Threshold=0.6,  whiteThreshold=0.4){
  # Define what thresholds we need for white.
  #rgbSpan_Threshold=0.1 # how close must RGBs be to count as snow
  #whiteThreshold=0.4 # What is the minimum value all RGBs must have to count
  
  # Vsetigial code correction
  latestImgUrl=imagePath
  
  # get the latest image, load with magick
  latestImg=magick::image_read(latestImgUrl)
  
  #where will the processed image live?
  processedImgPath=paste0("./snow-detection/", site, "_pheno.jpg")
  # processedTiff=paste0("./", site, "_pheno.tiff")
  
  
  # Trim off the top 90 pixels of img, where the info lives
  # line 1 of camera metadata
  magick::image_crop(latestImg, geometry = "859x27+0+0") %>% # Crop to first row
    magick::image_modulate(saturation = 0, brightness = 80) %>% # Adjust color and brightness to make it easier
    magick::image_negate() %>% # flip colors (white -> black, red -> green, etc)
    magick::image_contrast(sharpen = 1000) %>% # up the contrast a ton
    magick::image_enhance() %>% # clean up any noise
    magick::image_write(path = "./metadata1.png", image = .) #save it out!
  # line 2 of camera metadata
  magick::image_crop(latestImg, geometry = "310x27+0+30") %>% 
    magick::image_modulate(saturation = 0, brightness = 80) %>%
    magick::image_negate() %>%
    magick::image_contrast(sharpen = 1000) %>% 
    magick::image_enhance() %>%
    magick::image_write(path = "./metadata2.png", image = .)
  # line 3 of camera metadata
  magick::image_crop(latestImg, geometry = "138x24+0+60") %>% 
    magick::image_modulate(saturation = 0, brightness = 80) %>%
    magick::image_negate() %>%
    magick::image_contrast(sharpen = 1000) %>% 
    magick::image_enhance() %>%
    magick::image_write(path = "./metadata3.png", image = .)
  
  
  #Use OCR to parse the info out! (They probably have this on the API but that's lame)
  metadata=c(tesseract::ocr("./metadata1.png"), 
             tesseract::ocr("./metadata2.png"),
             tesseract::ocr("./metadata3.png"))
  
  # Clean up our temporary metadata images
  file.remove(c("./metadata1.png", "./metadata2.png", "./metadata3.png"))
  
  # Remove the metadata from our analysis, white balance the image, and save it
  cropImage=magick::image_crop(latestImg, geometry = "1296x810+0+90") %>%
    magick::image_normalize() %>%
    magick::image_write(image = ., path = processedImgPath) 
  # save to jpeg
  #magick::image_write(path = processedTiff) 
  
  # Let the clustering begin
  # https://www.r-bloggers.com/r-k-means-clustering-on-an-image/
  
  # we need to load this image in another way to deal in RGB values
  processedImg=jpeg::readJPEG(source = processedImgPath)
  
  # "melt" the matrix to tall skinny df
  imgDims=dim(processedImg)
  imgRGB <- data.frame(
    x = rep(1:imgDims[2], each = imgDims[1]),
    y = rep(imgDims[1]:1, imgDims[2]),
    R = as.vector(processedImg[,,1]),
    G = as.vector(processedImg[,,2]),
    B = as.vector(processedImg[,,3])
  )
  # CLuster analysis!
  kClusters <- 6
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
  
  #browser()
  # How far apart are all the RGB values?
  rgbSpans=apply(X = kMeans$centers,MARGIN = 1, range) %>% apply(X = ., MARGIN = 2, diff) %>% abs()
  rgSpans= abs(kMeans$centers[,1]-kMeans$centers[,2])
  rbSpans= kMeans$centers[,3]-kMeans$centers[,1]
  # Check- are all the RGBs within a tollerance of 0.1? (on the grayscale)
  whiteRows=(rowSums(kMeans$centers>whiteThreshold)==3 & 
               rgSpans<=rgSpan_Threshold & 
               rbSpans<=rbSpan_Threshold&
               rbSpans>=-.05)
  
  #Remove temp files so I can download more
  tempFiles=dir(tempdir(), full.names = T, include.dirs = F, recursive = F)
  file.remove(tempFiles)
  
  #browser()
  # calculate the decimal percent of img that is snow
  snowPortion=sum(kMeans$size[whiteRows])/sum(kMeans$size)
  
  # EOF
  return(snowPortion)
  
  # colors=kMeans$centers
  # colors[rowSums(kMeans$centers>0.7)!=3,"R"]=145/255
  # colors[rowSums(kMeans$centers>0.7)!=3,"G"]=31/255
  # colors[rowSums(kMeans$centers>0.7)!=3,"B"]=183/255
  # 
  # kColours <- rgb(colors[kMeans$cluster,])
  # 
  # ggplot(data = imgRGB, aes(x = x, y = y)) + 
  #   geom_point(colour = kColours)
}


for(site in siteList){
# get all the noon URLs for our site + year
siteNoonUrls=getSiteNoonUrls(site=site, year=year)

# initialize empty DF for for loop
snowCover=data.frame()

# URL by URL, build DF 
for(url in siteNoonUrls){
  date=stringr::str_extract(string = url, pattern = "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>% gsub(pattern = "_", replacement = "-")
  message(paste0("Img date: ", date))
  print("Detecting...")
  start=Sys.time()
  dayVal=try(detectSnow(url))
  end=Sys.time()
  print(paste0(site, ": complete (",difftime(end, start, units = "secs") %>% round(digits = 1) ," seconds)."))
  if(class(dayVal)=="numeric"){
    percentSnow=round(dayVal*100, digits = 2)
  }else{
    percentSnow=NA
  }
  snowCover=rbind(snowCover, data.frame(date=date, percentSnow=percentSnow))
}
snowCover$date=as.Date(snowCover$date)
saveRDS(snowCover, file = paste0("./snow-detection/", site, "_", year, "_snowCoverage.rds"))

snowCover=snowCover[!is.na(snowCover$percentSnow),]

p=ggplot2::ggplot(data=snowCover, ggplot2::aes(x=date, y=percentSnow))+
  ggplot2::geom_path(na.rm = T, color="#0e1675")+
  ggplot2::geom_area(fill="#6449fc", alpha=0.4)+
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%B")+
  ggplot2::scale_y_continuous(limits = c(0, 100))+
  ggplot2::ggtitle(label = paste0(site, " ", year, " Snow Coverage"),
                   subtitle = "Percent of understory phenocamera image classified as snow")+
  ggplot2::theme_bw()+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=-45, vjust = -.7))

ggplot2::ggsave(filename = paste0("./snow-detection/", site, " ", year, ".png"), plot = p, device = "png", width = 6, height = 4, units = "in", dpi = 180)

}