
setwd("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/train/")

#openThese.raw<-sapply(paste0("img_small_",picSite,"_",getDates,"_",picTime,".rds"), function(x) list.files(pattern=x))
openThese.raw<-sapply(paste0("*_solarNoon.rds"), function(x) list.files(pattern=x))
#make sure the file has data:
openThese.cln<-openThese.raw[which(sapply(openThese.raw, function(x) length(x))==1)]
imagesRaw<-lapply(openThese.cln, function(x) readRDS(x))
#output message:
message("Opening ", length(imagesRaw), " images for analysis")
#name the images so we can keep track of dates etc.
names(imagesRaw)<-openThese.cln
#open 1 to get the height and width of the image:
dir_path<-getwd()
img_test <- readRDS(file.path(dir_path, names(imagesRaw)[1]))
width<-dim(img_test)[1]
height<-dim(img_test)[2]
img_size <- width*height
  


feature_list <- pblapply(names(imagesRaw), function(imgname) {
  ######## USING IMAGER ######
  #check to see if the file is .rds or an image:
  filetype<-ifelse(length(grep(".jpg|.png|.jpeg|.bmp",imgname))!=0,"image","rds")
  if(filetype=="rds"){
    #open the image data:
    img <- readRDS(file.path(dir_path, imgname))
  }
  else{
    #open the image:
    img <- imager::load.image(file.path(dir_path, imgname))
  }
  ## Resize image (some of these need to be converted to ensure they're all the same size)
  img_resized <- imager::resize(img, size_x = width, size_y =  height)
  ## Set to grayscale
  grayimg <- imager::grayscale(img_resized)
  ## Get the image as a matrix
  img_matrix <- as.matrix(grayimg)
  ## Coerce to a vector
  img_vector <- as.vector(t(img_matrix))
  ######## USING IMAGER ######
  return(img_vector)
})

## bind the list of vector into matrix
feature_matrix <- do.call(rbind, feature_list)
feature_matrix <- as.data.frame(feature_matrix)
## Set names
names(feature_matrix) <- paste0("pixel", c(1:img_size))

## bind the list of vector into matrix
#feature_matrix <- do.call(rbind, img_vector)
#feature_matrix <- as.data.frame(feature_matrix)
#add the image names to the feature_matrix to preserve traceability:
feature_matrix$metaname<-gsub("img_small_|_solarNoon|sunrise|sunset|.rds","",names(imagesRaw))

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
rmvCol<-grep("metaname|posix",names(feature_matrix))
if(length(rmvCol)!=0){
  feature_matrix<-feature_matrix[,-rmvCol]
}
feature_matrix <- list(X = feature_matrix, y = label)

#browser()
# Check processing on random image
par(mar = rep(0, 4))
randoImg <- t(matrix(as.numeric(feature_matrix$X[2,]),
                     nrow = width, ncol = height, T))
image(t(apply(randoImg, 2, rev)), col = gray.colors(12),
      axes = F)


####create training and testing datasets #####
library(caret)
training_index <- createDataPartition(feature_matrix$y, p = .8, times = 1)
training_index <- unlist(training_index,use.names = F)
trainData <- list(X=feature_matrix$X[training_index,],
                  y=feature_matrix$y[training_index])
##gsub the character data in trainData$y for numeric:
trainData$y<-gsub("DRY",0,trainData$y)
trainData$y<-gsub("FLOOD",1,trainData$y)
# Fix structure for 2d CNN
train_array <- t(trainData$X)
dim(train_array) <- c(height, width, nrow(trainData$X), 1)
# Reorder dimensions (will now be numImages x height x width x colDimension)
train_array <- aperm(train_array, c(3,1,2,4))
#create the test set: (for some reason you need to manually include the first index in here or it won't work)
testData <- list(X=feature_matrix$X[unique(c(1,which(!seq(1:nrow(feature_matrix$X)) %in% training_index))),],
                 y=feature_matrix$y[unique(c(1,which(!seq(1:nrow(feature_matrix$X)) %in% training_index)))])
testData$y<-gsub("DRY",0,testData$y)
testData$y<-gsub("FLOOD",1,testData$y)
#fix structure for 2d CNN:
test_array <- t(testData$X)
dim(test_array) <- c(height, width, nrow(testData$X), 1)
# Reorder dimensions (will now be numImages x height x width x colDimension)
test_array <- aperm(test_array, c(3,1,2,4))

# Check random image again
randoImg <- train_array[2,,,]
image(t(apply(randoImg, 2, rev)), col = gray.colors(12),
      axes = F)


# Build CNN model
model <- keras_model_sequential() 
model %>% 
  layer_conv_2d(kernel_size = c(3, 3), filter = 32,
                activation = "relu", padding = "same",
                input_shape = c(height, width, 1),
                data_format = "channels_last") %>%
  layer_conv_2d(kernel_size = c(3, 3), filter = 32,
                activation = "relu", padding = "valid") %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  #layer_dropout(rate = 0.25) %>%
  
  layer_conv_2d(kernel_size = c(3, 3), filter = 64, strides = 2,
                activation = "relu", padding = "same") %>%
  layer_conv_2d(kernel_size = c(3, 3), filter = 64,
                activation = "relu", padding = "valid") %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  #layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  layer_dense(units = 50, activation = "relu") %>% 
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)


#compile the model:
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = "adam",
  metrics = c('accuracy')
)
browser()
history <- model %>% fit(
  x = train_array, y = as.numeric(trainData$y), 
  epochs = 15, 
  #validation_data = list(x=test_array,y=as.numeric(testData$y))
  validation_split = 0.2
)

plot(history)

# Compute probabilities and predictions on test set
predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

# Visual inspection of 32 cases
set.seed(100)
random <- sample(1:nrow(testData$X), 32)
preds <- predictions[random,]
probs <- as.vector(round(probabilities[random,], 2))

par(mfrow = c(4, 8), mar = rep(0, 4))
for(i in 1:length(random)){
  image(t(apply(test_array[random[i],,,], 2, rev)),
        col = gray.colors(12), axes = F)
  legend("topright", legend = ifelse(preds[i] == 0, "DRY", "FLOOD"),
         text.col = ifelse(preds[i] == 0, 2, 4), bty = "n", text.font = 2)
  legend("topleft", legend = probs[i], bty = "n", col = "white")
}

#save the model:
save(model, file = paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/CNNmodel_",gsub(" |:","_",Sys.time()),".RData"))


