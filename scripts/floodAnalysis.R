#' @title Model to detect flooding at NEON sites
#'
#' @author Josh 
#'
#' @details Will inform if a NEON site was flooded on a given day.
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
getSzn<-function(x){
  browser()
  yq <- as.yearqtr(as.yearmon(x, "%m/%d/%Y") + 1/12)
  DF$Season <- factor(format(yq, "%q"), levels = 1:4, 
                      labels = c("winter", "spring", "summer", "fall"))
}




floodAnalysis<-function(picSite="DELA",dateStart="2019-01-01",dateEnd="2019-12-31",picTime="sunrise",
                        picIR=T,rerun=F,useGMT=F){
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
  #make new list that defines what dates still need to be run:
  getImageDates<-getDates[!getDates %in% as.Date(substr(alreadyRun.cln,16,25))]
  
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
    #browser()
    #get the lat/lon for the NEON site:
    metaSite<-Z10::get.site.meta(site = picSite)
    siteLat<-metaSite$site.latitude
    siteLon<-metaSite$site.longitude
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
      #round the time to the nearest 15 minutes:
      info.sun.lst[[i]]$roundValue<-format(lubridate::round_date(as.POSIXct(info.sun.lst[[i]]$value,format="%H:%M"),unit = "15 minutes"),"%H:%M")
      #identify where daylight saving dates: (for some reason some times aren't exactly at 3600, 1 hour, diff)
      index.DT<-which(diff(as.numeric(hm(info.sun.lst[[i]]$value)))>=3500)
      index.ST<-which(diff(as.numeric(hm(info.sun.lst[[i]]$value)))<=-3500)
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
    
    #assign seasons to the dates:
    # getSzn()
    
    
    #
    #assign season to date:
    #dateSeq.df<-data.frame(date=dateSeq.vec,season=NA)
    #dateSeq<-seq.Date(from = as.Date("2018-12-30"),to = as.Date("2019-12-31"),by = "day")
    
    
    #subset the dataframe to the correct time of day: sunrise, solarNoon, or sunset:
    info.sun.use<-info.sun.df[grep(picTime,info.sun.df$L1),]
    imagePath<-list()
    #browser()
    for(j in 1:length(info.sun.use$picTimeHHMM)){
      imagePath[[j]]<-getPhenoUrls(site = picSite,year = substr(dateStart,0,4),date = info.sun.use$date[j],time = info.sun.use$picTimeHHMM[j],IR=picIR)
    }
    #browser()
    
    #imagePath<-lapply(dateSeq, function(x) getPhenoUrls(site = picSite,year = substr(dateStart,0,4),date = x,time = picTime,IR=picIR))
    #load a sample image:
    sampleImg<-imager::load.image(imagePath[!is.na(imagePath)][[1]])
    #set the height and width of the image:
    width<-round(0.2 * dim(sampleImg)[1],0)
    height<-round((dim(sampleImg)[2]/dim(sampleImg)[1]*width),0)
    
    #go thru each image, convert to grayscale, change dimensions, and save
    for(i in 1:length(imagePath[!is.na(imagePath)])){ #length(imagePath[!is.na(imagePath)])
      ## Download the image
      img_raw<-imager::load.image(imagePath[!is.na(imagePath)][[i]])
      ## Convert to grayscale
      img_gray <- imager::grayscale(img_raw)
      ## convert the grayscale image size to something smaller:
      img_small <- imager::resize(img_gray, size_x = width, size_y =  height)
      ### SAVE THE img_small and img_matrix for future analysis:
      saveRDS(object = img_small, file = paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/img_small_",picSite,"_",info.sun.use$date[i],"_",picTime,".rds"))
      #saveRDS(object = img_small, file = paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/img_small_",picSite,"_",info.sun.use$date[i],"_",picTime,".rds"))
      
      #plot image and get date for title:
      #date_plot<-stringr::str_sub(imagePath[!is.na(imagePath)][[i]],-21,-12)
      #plot(grayimg[[i]],main=date_plot)
      
      #grayimg@.Data
      ## Get the image as a matrix
      #img_matrix <- as.matrix(img_small)
      
      ## Coerce to a vector
      #img_vector[[i]] <- as.vector(t(img_matrix))
    }#end for loop
  }

  #Open all the images, convert to matrix, 
  

  #open the nominal images if they already exist:
  setwd("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/")
  openThese.raw<-sapply(paste0("img_small_",picSite,"_",getDates,"_",picTime,".rds"), function(x) list.files(pattern=x))
  #make sure the file has data:
  openThese.cln<-openThese.raw[which(sapply(openThese.raw, function(x) length(x))==1)]
  imagesRaw<-lapply(openThese.cln, function(x) readRDS(x))
  #name the images so we can keep track of dates etc.
  names(imagesRaw)<-names(openThese.cln)
  #open the metadata file:
  siteMetadata<-read.csv(paste0(picSite,"_metadata.csv"),header = T,stringsAsFactors = F)
  
  
  # get the latest image, load with magick
  #imgs<-lapply(imagePath[!is.na(imagePath)], function(x) imager::load.image(x))
  
  #set the height and width of the images to something smaller than nominal image, but keep aspect ratio
  #imagePath<-getPhenoUrls(site = "DELA",year = 2019,date = "2019-01-01",time = "0800",IR=T)
  #sampleImg<-imager::load.image(imagePath[!is.na(imagePath)][[1]])

  
  #download each image, convert to grayscale and change dimensions:
  #grayimg<-readRDS("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/images_DELA.rds")
  
  #initialize img_vector 
  #img_vector<-list()
  #if(length(imagePath)==1){
    #img_vector<-list()
    #img_save<-list()
    
    #name the images by the date:
    #names(grayimg)<-paste0("img_",stringr::str_sub(unlist(imagePath[!is.na(imagePath)]),-21,-12))
    #save all the grayscale images and the vector data:
    
    #grayimg<-readRDS("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/images_DELA.rds")
    
  #}else{
  img_vector<-list()
  for(i in 1:length(imagesRaw)){ #length(imagePath[!is.na(imagePath)])
    #download the image
    #imageDir<-imager::load.image(imagePath[!is.na(imagePath)][[i]])
    #convert the image size to something smaller:
    #img_resized <- imager::resize(imageDir, size_x = width, size_y =  height)
    ## Set to grayscale
    #grayimg[[i]] <- imager::grayscale(img_resized)
    #plot image and get date for title:
    #date_plot<-stringr::str_sub(imagePath[!is.na(imagePath)][[i]],-21,-12)
    #plot(grayimg[[i]],main=date_plot)
    ## Get the image as a matrix
    img_matrix <- as.matrix(imagesRaw[[i]])
    #grayimg@.Data
    ## Coerce to a vector
    img_vector[[i]] <- as.vector(t(img_matrix))
  }
  #}
  browser()
  
  
  
  #add labels to each image by looking thru each and noting if it's dry (0) or flooded (1)
  #ind.floodEnd1<-grep("2019_01_08",names(grayimg))
  #ind.floodEnd2<-grep("2019_02_21",names(grayimg))
  #ind.floodEnd3<-grep("2019_03_06",names(grayimg))
  label<-c(rep(1,ind.floodEnd1),rep(0,ind.floodEnd2-ind.floodEnd1),rep(1,ind.floodEnd3-ind.floodEnd2),
           rep(0,length(grayimg)-(ind.floodEnd3)))
  
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, img_vector)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  #names(feature_matrix) <- paste0("pixel", c(1:img_size))
  #if (add_label) {
  ## Add label
  feature_matrix <- cbind(label = label, feature_matrix)
  
}

# #set seed for traceability:
# library(magrittr)
# set.seed(2020)
# 
# #download a bunch of images, flooded and non-flooded from the phenocam network (full year of data from Dead Lake):
# dateSeq<-seq.Date(from = as.Date("2018-12-30"),to = as.Date("2019-12-31"),by = "day")
# source('C:/Users/jroberti/Kaggle/imgClassification/getPhenoUrls.R', echo=TRUE)
# imagePath<-lapply(dateSeq, function(x) getPhenoUrls(site = "DELA",year = 2019,date = x,time = "0800",IR=T))
# 
# # get the latest image, load with magick
# #imgs<-lapply(imagePath[!is.na(imagePath)], function(x) imager::load.image(x))
# 
# #set the height and width of the images to something smaller than nominal image, but keep aspect ratio
# #imagePath<-getPhenoUrls(site = "DELA",year = 2019,date = "2019-01-01",time = "0800",IR=T)
# sampleImg<-imager::load.image(imagePath[!is.na(imagePath)][[1]])
# width<-round(0.2 * dim(sampleImg)[1],0)
# height<-round((dim(sampleImg)[2]/dim(sampleImg)[1]*width),0)
# 
# #download each image, convert to grayscale and change dimensions:
# #grayimg<-readRDS("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/images_DELA.rds")
# 
# #initialize img_vector 
# img_vector<-list()
# if(length(imagePath)==1){
#   grayimg<-list()
#   for(i in 1:length(imagePath[!is.na(imagePath)])){ #length(imagePath[!is.na(imagePath)])
#     #download the image
#     imageDir<-imager::load.image(imagePath[!is.na(imagePath)][[i]])
#     #convert the image size to something smaller:
#     img_resized <- imager::resize(imageDir, size_x = width, size_y =  height)
#     ## Set to grayscale
#     grayimg[[i]] <- imager::grayscale(img_resized)
#     #plot image and get date for title:
#     date_plot<-stringr::str_sub(imagePath[!is.na(imagePath)][[i]],-21,-12)
#     plot(grayimg[[i]],main=date_plot)
#     ## Get the image as a matrix
#     img_matrix <- as.matrix(grayimg[[i]])
#     #grayimg@.Data
#     ## Coerce to a vector
#     img_vector[[i]] <- as.vector(t(img_matrix))
#   }
#   #name the images by the date:
#   names(grayimg)<-paste0("img_",stringr::str_sub(unlist(imagePath[!is.na(imagePath)]),-21,-12))
#   #save all the grayscale images and the vector data:
#   saveRDS(object = grayimg, file = "C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/images_DELA.rds")
#   #grayimg<-readRDS("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/images_DELA.rds")
#   
# }else{
#   for(i in 1:length(grayimg)){ #length(imagePath[!is.na(imagePath)])
#     #download the image
#     #imageDir<-imager::load.image(imagePath[!is.na(imagePath)][[i]])
#     #convert the image size to something smaller:
#     #img_resized <- imager::resize(imageDir, size_x = width, size_y =  height)
#     ## Set to grayscale
#     #grayimg[[i]] <- imager::grayscale(img_resized)
#     #plot image and get date for title:
#     #date_plot<-stringr::str_sub(imagePath[!is.na(imagePath)][[i]],-21,-12)
#     #plot(grayimg[[i]],main=date_plot)
#     ## Get the image as a matrix
#     img_matrix <- as.matrix(grayimg[[i]])
#     #grayimg@.Data
#     ## Coerce to a vector
#     img_vector[[i]] <- as.vector(t(img_matrix))
#   }
# }
# 
# #Manually add labels to each image by looking thru each and noting if it's dry (0) or flooded (1)
# ind.floodEnd1<-grep("2019_01_08",names(grayimg))
# ind.floodEnd2<-grep("2019_02_21",names(grayimg))
# ind.floodEnd3<-grep("2019_03_06",names(grayimg))
# label<-c(rep(1,ind.floodEnd1),rep(0,ind.floodEnd2-ind.floodEnd1),rep(1,ind.floodEnd3-ind.floodEnd2),
#          rep(0,length(grayimg)-(ind.floodEnd3)))
# 
# ## bind the list of vector into matrix
# feature_matrix <- do.call(rbind, img_vector)
# feature_matrix <- as.data.frame(feature_matrix)
# ## Set names
# #names(feature_matrix) <- paste0("pixel", c(1:img_size))
# #if (add_label) {
#   ## Add label
# feature_matrix <- cbind(label = label, feature_matrix)
# #}

#grab flooded and non-flooded data:
#data.flooded<-feature_matrix[grep(1,feature_matrix$label),]
#data.dry<-feature_matrix[grep(0,feature_matrix$label),]
#pick the same number of 

#"C:/Users/jroberti/Kaggle/imgClassification/train/"
#lapply(grayimg, function(x) plot(x))



#open one of the images in the training set:
# library(imager)
# img.exampleCat<-imager::load.image(paste0(imageDir,"/cat.0.jpg"))
# plot(img.exampleCat)
# 
# img.exampleDog<-imager::load.image(paste0(imageDir,"/dog.0.jpg"))
# plot(img.exampleDog)
# 
# 
# ## 2 Data Preprocessing
# ### Convert the images to dims of 28 x 28 and turn into greyscale:
# width <- 28
# height <- 28
# ## pbapply is a library to add progress bar *apply functions
# ## pblapply will replace lapply
# library(pbapply)
# extract_feature <- function(dir_path, width, height, is_cat = TRUE, add_label = TRUE) {
#   img_size <- width*height
#   ## List images in path
#   images_names <- list.files(dir_path)
#   if (add_label) {
#     ## Select only cats or dogs images
#     images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
#     ## Set label, cat = 0, dog = 1
#     label <- ifelse(is_cat, 0, 1)
#   }
#   print(paste("Start processing", length(images_names), "images"))
#   ## This function will resize an image, turn it into greyscale
#   feature_list <- pblapply(images_names, function(imgname) {
#     ## Read image
#     img <- imager::load.image(file.path(dir_path, imgname))
#     ## Resize image
#     img_resized <- imager::resize(img, size_x = width, size_y =  height)
#     ## Set to grayscale
#     grayimg <- imager::grayscale(img_resized)
#     ## Get the image as a matrix
#     img_matrix <- as.matrix(grayimg)
#     #grayimg@.Data
#     ## Coerce to a vector
#     img_vector <- as.vector(t(img_matrix))
#     return(img_vector)
#   })
#   ## bind the list of vector into matrix
#   feature_matrix <- do.call(rbind, feature_list)
#   feature_matrix <- as.data.frame(feature_matrix)
#   ## Set names
#   names(feature_matrix) <- paste0("pixel", c(1:img_size))
#   if (add_label) {
#     ## Add label
#     feature_matrix <- cbind(label = label, feature_matrix)
#   }
#   return(feature_matrix)
# }
# width<-28
# height<-28
# #image_dir<-"~/Github/L0-tracking/scratch/imgs/imgClassification/train/"
# #apply above function to the cats data and the dogs data:
# cats_data <- extract_feature(dir_path = imageDir, width = width, height = height)
# dogs_data <-extract_feature(dir_path = imageDir, width = width, height = height,is_cat = F)
# #save the data as .rds just in case:
# saveRDS(cats_data, "C:/Users/jroberti/Kaggle/imgClassification/cat.rds")
# saveRDS(dogs_data, "C:/Users/jroberti/Kaggle/imgClassification/dog.rds")





## 3 Train the Model
# library(caret)
# ## Bind rows in a single dataset
# #complete_set <- rbind(cats_data, dogs_data)
# ## test/training partitions
# training_index <- createDataPartition(feature_matrix$label, p = .8, times = 1)
# training_index <- unlist(training_index)
# train_set <- feature_matrix[training_index,]
# dim(train_set)
# #create the test set:
# test_set <- feature_matrix[-training_index,]
# dim(test_set)
# ## Fix train and test datasets
# train_data <- data.matrix(train_set)
# #remove the label column before running simulation:
# train_x <- t(train_data[, -1])
# train_y <- train_data[,1]
# train_array <- train_x
# dim(train_array) <- c(width, height, 1, ncol(train_x))
# 
# test_data <- data.matrix(test_set)
# test_x <- t(test_set[,-1])
# test_y <- test_set[,1]
# test_array <- test_x
# dim(test_array) <- c(height, width, 1, ncol(test_x))
# 
# #building the actual model:
# library(mxnet)
# ## Model
# mx_data <- mx.symbol.Variable('data')
# ## 1st convolutional layer 5x5 kernel and 20 filters.
# conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5, 5), num_filter = 10) #20
# tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
# pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2 ))
# ## 2nd convolutional layer 5x5 kernel and 50 filters.
# conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 20) #50
# tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
# pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# ## 1st fully connected layer
# flat <- mx.symbol.Flatten(data = pool_2)
# fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 100) #500
# tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
# ## 2nd fully connected layer
# fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
# ## Output
# NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)
# 
# ## Set seed for reproducibility
# mx.set.seed(100)
# 
# ## Device used. Sadly not the GPU :-(
# device <- mx.cpu()
# 
# ## Train on 1200 samples
# model <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_y,
#                                      ctx = device,
#                                      num.round = 10,
#                                      array.batch.size = 100,
#                                      learning.rate = 0.05,
#                                      momentum = 0.9,
#                                      wd = 0.00001,
#                                      eval.metric = mx.metric.accuracy,
#                                      epoch.end.callback = mx.callback.log.train.metric(100))
# ## Test test set
# predict_probs <- predict(model, test_array)
# #predict_probs <- predict.MXFeedForwardModel(model, test_array)
# predicted_labels <- max.col(t(predict_probs)) - 1
# table(test_data[, 1], predicted_labels)
# 
# #get the accuracy of the model:
# sum(diag(table(test_data[, 1], predicted_labels)))/length(test_data[, 1])
# 
# 
# ### old school mxnet function:
# predict.MXFeedForwardModel <- function(model, X, ctx=NULL, array.batch.size=128, array.layout="auto") {
#   if (is.null(ctx)) ctx <- mx.ctx.default()
#   if (is.array(X) || is.matrix(X)) {
#     if (array.layout == "auto") {
#       array.layout <- mxnmx.model.select.layout.predict(X, model)
#     }
#     if (array.layout == "rowmajor") {
#       X <- t(X)
#     }
#   }
#   X <- mx.model.init.iter(X, NULL, batch.size=array.batch.size, is.train=FALSE)
#   print('itterSet')
#   X$reset()
#   if (!X$iter.next()) stop("Cannot predict on empty iterator")
#   dlist = X$value()
#   pexec <- mx.simple.bind(model$symbol, ctx=ctx, data=dim(dlist$data), grad.req="null")
#   mx.exec.update.arg.arrays(pexec, model$arg.params, match.name=TRUE)
#   mx.exec.update.aux.arrays(pexec, model$aux.params, match.name=TRUE)
#   packer <- mx.nd.arraypacker()
#   X$reset()
#   while (X$iter.next()) {
#     dlist = X$value()
#     mx.exec.update.arg.arrays(pexec, list(data=dlist$data), match.name=TRUE)
#     mx.exec.forward(pexec, is.train=FALSE)
#     out.pred <- mx.nd.copyto(pexec$ref.outputs[[1]], mx.cpu())
#     padded <- X$num.pad()
#     oshape <- dim(out.pred)
#     ndim <- length(oshape)
#     packer$push(mx.nd.slice(out.pred, 0, oshape[[ndim]] - padded))
#   }
#   X$reset()
#   return(packer$get())
# }
