
floodAnalysis<-function(picTime="solarNoon"){
  #open images with applicable picTime:
  setwd("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/")

  #openThese.raw<-sapply(paste0("img_small_",picSite,"_",getDates,"_",picTime,".rds"), function(x) list.files(pattern=x))
  openThese.raw<-sapply(paste0("*_solarNoon.rds"), function(x) list.files(pattern=x))
  #make sure the file has data:
  openThese.cln<-openThese.raw[which(sapply(openThese.raw, function(x) length(x))==1)]
  imagesRaw<-lapply(openThese.cln, function(x) readRDS(x))
  #name the images so we can keep track of dates etc.
  names(imagesRaw)<-gsub(".rds","",substr(openThese.cln,11,nchar(openThese.cln)))
  
  
  #quick exploratory stuff: https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
  # library(ggplot2)
  # library(dplyr)
  # bdf <- as.data.frame(imagesRaw[[2]])
  # head(bdf,3)
  # bdf <- mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
  # ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)
  
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
    
    ########## CONVERT TO GRAYSCALE OR GET TGB RATIOS ######
    ## Convert to grayscale 
    img_gray <- imager::grayscale(imagesRaw[[i]])
    ## or create ratio of RGB vectors:
    ############## Decide what to do here ########
    ## Set to grayscale
    #grayimg[[i]] <- imager::grayscale(img_resized)
    #plot image and get date for title:
    #date_plot<-stringr::str_sub(imagePath[!is.na(imagePath)][[i]],-21,-12)
    #plot(grayimg[[i]],main=date_plot)
    ## Get the image as a matrix
    #img_matrix <- as.matrix(imagesRaw[[i]][,,,1])/as.matrix(imagesRaw[[i]][,,,2])
    img_matrix <- as.matrix(img_gray)
    #grayimg@.Data
    ## Coerce to a vector
    img_vector[[i]] <- as.vector(t(img_matrix))
  }
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, img_vector)
  feature_matrix <- as.data.frame(feature_matrix)
  #add the image names to the feature_matrix to preserve traceability:
  feature_matrix$metaname<-gsub("_solarNoon|sunrise|sunset","",names(imagesRaw))
  
  #open the metadata file:
  #siteMetadata<-read.csv(paste0(picSite,"_metadata.csv"),header = T,stringsAsFactors = F)
  siteMetadata<-read.csv("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/site_metadata.csv",header = T,stringsAsFactors = F)
  #get site names:
  sites<-names(siteMetadata)[2:ncol(siteMetadata)]
  #convert stupid excel dates to POSIX:
  siteMetadata$posix<-as.Date(siteMetadata$date,format = "%m/%d/%Y")
  #Tmap the name of each image to the appropriate tag for a specific site:
  site.meta.lst<-list()
  for(j in 1:length(sites)){
    site.meta.tags<-siteMetadata[,grep(paste0(sites[j],"|posix"),names(siteMetadata))]
    #create a string that matches the feature_matrix$metaname:
    site.meta.tags$metaname<-paste0(names(site.meta.tags)[grep(sites[j],names(site.meta.tags))],"_",site.meta.tags$posix)
    #output:
    site.meta.lst[[j]]<-site.meta.tags
    #rename so I can do.call:
    names(site.meta.lst[[j]])<-c("tag","posix","metaname")
  }
  #do.call into one dataframe:
  site.meta.df<-do.call(rbind,site.meta.lst)
  #merge 'site.meta.df' with 'feature_matrix'
  metadata.full<-merge(site.meta.df,feature_matrix,by="metaname")
  #add labels:
  label<-metadata.full$tag
  ## Add label
  feature_matrix <- cbind(label = label, feature_matrix)
  #remove the metaname and posix columns:
  rmvCol<-grep("metaname|posix",names(feature_matrix))
  if(length(rmvCol)!=0){
    feature_matrix<-feature_matrix[,-rmvCol]
  }
  #}
  
  ### manual labels ####
  #add labels to each image by looking thru each and noting if it's dry (0) or flooded (1)
  #ind.floodEnd1<-grep("2019_01_08",names(grayimg))
  #ind.floodEnd2<-grep("2019_02_21",names(grayimg))
  #ind.floodEnd3<-grep("2019_03_06",names(grayimg))
  # label<-c(rep(1,ind.floodEnd1),rep(0,ind.floodEnd2-ind.floodEnd1),rep(1,ind.floodEnd3-ind.floodEnd2),
  #          rep(0,length(grayimg)-(ind.floodEnd3)))
  ### manual labels ####
  
  ## Set names
  #names(feature_matrix) <- paste0("pixel", c(1:img_size))
  #if (add_label) {
  
  
  ### Create the model
  # 3 Train the Model
  library(caret)
  ## Bind rows in a single dataset
  #complete_set <- rbind(cats_data, dogs_data)
  ## test/training partitions
  training_index <- createDataPartition(feature_matrix$label, p = .8, times = 1)
  training_index <- unlist(training_index)
  train_set <- feature_matrix[training_index,]
  dim(train_set)
  #create the test set:
  test_set <- feature_matrix[-training_index,]
  dim(test_set)
  ## Fix train and test datasets
  train_data <- data.matrix(train_set)
  #remove the label column before running simulation:
  train_x <- t(train_data[, -1])
  train_y <- train_data[,1]
  train_array <- train_x
  #define width and height:
  width<-dim(imagesRaw[[1]])[1]
  height<-dim(imagesRaw[[1]])[2]
  #dim(train_array) <- c(width, height, 1, ncol(train_x)) #run this if using RGB data only??
  test_data <- data.matrix(test_set)
  test_x <- t(test_set[,-1])
  test_y <- test_set[,1]
  test_array <- test_x
  #dim(test_array) <- c(height, width, 1, ncol(test_x)) #run this if using RGB data only
  
  #building the actual model:
  library(mxnet)
  ## Model
  mx_data <- mx.symbol.Variable('data')
  ## 1st convolutional layer 5x5 kernel and 20 filters.
  conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5, 5), num_filter = 20) #20
  tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
  pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2 ))
  ## 2nd convolutional layer 5x5 kernel and 50 filters.
  conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50) #50
  tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
  pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  ## 1st fully connected layer
  flat <- mx.symbol.Flatten(data = pool_2)
  fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500) #500
  tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
  ## 2nd fully connected layer
  fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
  ## Output
  NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)
  
  ## Set seed for reproducibility
  mx.set.seed(100)
  
  ## Device used. Sadly not the GPU :-(
  device <- mx.cpu()
  browser()
  
  library(neuralnet)
  # fit neural network
  nn<-neuralnet(label~.,data=train_set, hidden=1,act.fct = "logistic",
                linear.output = FALSE)
  ## Prediction using neural network
  #remove the label from test_set
  test_label<-test_set$label
  #remove the label from the test set:
  col.label<-grep("label",names(test_set))
  if(length(col.label)!=0){
    test_set<-test_set[,-col.label]
  }
  Predict<-compute(nn,test_set)
  Predict$net.result
  # Converting probabilities into binary classes setting threshold level 0.5
  prob <- Predict$net.result
  pred.vec <- ifelse(prob>0.5, 1, 0)
  #combine with the test set labels:
  pred.df<-data.frame(truth=test_label,pred=pred.vec)
  #get the accuracy:
  table(pred.df)
  sum(diag(table(pred.df$truth, pred.df$pred)))/length(pred.df$truth)
  Sys.time()-start.time
  
  
  
  
  ## Train on 1200 samples
  model <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_y,
                                       ctx = device,
                                       num.round = 10,
                                       array.batch.size = 100,
                                       learning.rate = 0.05,
                                       momentum = 0.9,
                                       wd = 0.00001,
                                       eval.metric = mx.metric.accuracy,
                                       epoch.end.callback = mx.callback.log.train.metric(100))
  ## Test test set
  predict_probs <- predict(model, test_array)
  #predict_probs <- predict.MXFeedForwardModel(model, test_array)
  predicted_labels <- max.col(t(predict_probs)) - 1
  table(test_data[, 1], predicted_labels)
  
  #get the accuracy of the model:
  sum(diag(table(test_data[, 1], predicted_labels)))/length(test_data[, 1])
  
  browser()
  
  ### old school mxnet function:
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
