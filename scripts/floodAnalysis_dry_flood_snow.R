
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
siteMetadata<-read.csv("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/site_metadata_2.csv",header = T,stringsAsFactors = F)
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
#remove any instances where the tag is "" or NA:
rmvInds<-which(metadata.full$tag=="" | is.na(metadata.full$tag))
if(length(rmvInds)!=0){
  metadata.full<-metadata.full[-rmvInds,]
}

#rmv 'Partial' instances:
rmvPartialImgs<-grep("PARTIAL",metadata.full$tag)
if(length(rmvPartialImgs)!=0){
  metadata.full<-metadata.full[-rmvPartialImgs,]
}
#add labels:
label<-metadata.full$tag
## Add label
#rmvCol<-grep("metaname|posix",names(feature_matrix))
#need to remove the 'tag' column too.  We're reassigning it as "label" in $y component
rmvCol<-grep("metaname|posix|tag",names(metadata.full)) 
#grab the metaname column and keep this. Want to keep these with the data for reference after model is fit:
metaColumn<-grep("metaname",names(metadata.full))
if(length(rmvCol)!=0){
  #keep metacolumn for traceability
  metanames<-metadata.full[,metaColumn]
  #feature_matrix<-feature_matrix[,-rmvCol]
  metadata.full<-metadata.full[,-rmvCol]
}
metadata.full <- list(X = metadata.full, y = label,z=metanames)
#feature_matrix <- list(X = feature_matrix, y = label)

#browser()
# Check processing on random image
par(mar = rep(0, 4))
randoImg <- t(matrix(as.numeric(metadata.full$X[2,]),
                     nrow = width, ncol = height, T))
image(t(apply(randoImg, 2, rev)), col = gray.colors(12),
      axes = F)


####create training and testing datasets #####
library(caret)
training_index <- createDataPartition(metadata.full$y, p = .8, times = 1)
training_index <- unlist(training_index,use.names = F)
trainData <- list(X=metadata.full$X[training_index,],
                  y=metadata.full$y[training_index],
                  z=metadata.full$z[training_index])
##gsub the character data in trainData$y for numeric:
trainData$y<-gsub("DRY",0,trainData$y)
#trainData$y<-gsub("PARTIAL FLOOD",1,trainData$y)
trainData$y<-gsub("FLOOD",1,trainData$y)
# trainData$y<-gsub("PARTIAL SNOW","SNOW",trainData$y)
 trainData$y<-gsub("SNOW",2,trainData$y)
# Fix structure for 2d CNN
train_array <- t(trainData$X)
dim(train_array) <- c(height, width, nrow(trainData$X), 1)
# Reorder dimensions (will now be numImages x height x width x colDimension)
train_array <- aperm(train_array, c(3,1,2,4))
#create the test set: (for some reason you need to manually include the first index in here or it won't work)
testData <- list(X=metadata.full$X[unique(c(1,which(!seq(1:nrow(metadata.full$X)) %in% training_index))),],
                 y=metadata.full$y[unique(c(1,which(!seq(1:nrow(metadata.full$X)) %in% training_index)))],
                 z=metadata.full$z[unique(c(1,which(!seq(1:nrow(metadata.full$X)) %in% training_index)))])
testData$y<-gsub("DRY",0,testData$y)
#testData$y<-gsub("PARTIAL FLOOD",1,testData$y) #
testData$y<-gsub("FLOOD",1,testData$y)
# testData$y<-gsub("PARTIAL SNOW","SNOW",testData$y)
testData$y<-gsub("SNOW",2,testData$y)
#fix structure for 2d CNN:
test_array <- t(testData$X)
dim(test_array) <- c(height, width, nrow(testData$X), 1)
# Reorder dimensions (will now be numImages x height x width x colDimension)
test_array <- aperm(test_array, c(3,1,2,4))

# Check random image again
randoImg <- train_array[2,,,]
image(t(apply(randoImg, 2, rev)), col = gray.colors(12),
      axes = F)


########## [START] MODEL TAKEN FROM CNN_example2; trying to see if it will work with snow #####model <- keras_model_sequential() %>% 
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(height,width,1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")

#Let's display the architecture of our model so far.
#the output of every Conv2D and MaxPooling2D layer is a 3D tensor of shape (height, width, channels). 
#The width and height dimensions tend to shrink as you go deeper in the network. 
#The number of output channels for each Conv2D layer is controlled by the first argument (e.g., 32 or 64). 
#Typically, as the width and height shrink, you can afford (computationally) to add more output channels in each Conv2D layer.
summary(model)


####Add Dense layers on top
#To complete our model, you will feed the last output tensor from the convolutional base (of shape (3, 3, 64)) 
#into one or more Dense layers to perform classification. 
#Dense layers take vectors as input (which are 1D), while the current output is a 3D tensor. 
#First, you will flatten (or unroll) the 3D output to 1D, then add one or more Dense layers on top. 
#CIFAR has 10 output classes, so you use a final Dense layer with 10 outputs and a softmax activation.
model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  #'units' in the next line is the number of total classes that are being predicted.  In this case, we have DRY, FLOOD, and SNOW, so we set units = 3.
  layer_dense(units = 3, activation = "softmax")   #layer_dense(units = 10, activation = "softmax")
#Note Keras models are mutable objects and you don't need to re-assign model in the chubnk above.
summary(model) 
#As you can see, our (3, 3, 64) outputs were flattened into vectors of shape (576) before going through two Dense layers

#####Compile and train the model
model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% fit(
  x = train_array, y = as.numeric(trainData$y), 
  epochs = 15, 
  #validation_data = list(x=test_array,y=as.numeric(testData$y))
  validation_split = 0.2 # will hold out 0.X of training data for validation.
)
########## [END] MODEL TAKEN FROM CNN_example2; trying to see if it will work with snow #####


plot(history)

# Compute predictions, predicted classes, and probabilities for test set:
pred_class <-  predict_classes(model, test_array)
pred_prob<- data.frame(model %>% predict(test_array))
names(pred_prob)<-c("DRY","FLOOD","SNOW")
probabilities <- predict_proba(model, test_array)
#score <- model %>% evaluate(testData, testData$y, verbose = 0)
#get the accuracy of the model on test set:
acc.df<-data.frame(truth=testData$y,preds=predictions,probs=probabilities,image=testData$z)

# testData$y<-gsub("0","DRY",testData$y)
# testData$y<-gsub("1","FLOOD",testData$y)
# testData$y<-gsub("2","SNOW",testData$y)
# 
# predictions<-gsub("0","DRY",predictions)
# predictions<-gsub("1","FLOOD",predictions)
# predictions<-gsub("2","SNOW",predictions)

#create confusion matrix:
confusionMatrix(data = as.factor(testData$y), as.factor(predictions))

# Visual inspection of 8 cases
set.seed(100)
#random <- sample(1:nrow(testData$X), 8)
#preds <- pred_class#predictions[random,]
#probs <- as.vector(round(probabilities, 2)) #as.vector(round(probabilities[random,], 2))

par(mfrow = c(2, 4), mar = rep(0, 4))
for(i in 1:length(random)){
  image(t(apply(test_array[i,,,], 2, rev)),
        col = gray.colors(12), axes = F)
  legend("topright", legend = paste0(names(which.max(pred_prob[i,]))," \n", 
                                     round(pred_prob[i,][which.max(pred_prob[i,])],3)), bty = "n", 
         text.col = "blue", text.font = 2)
         #text.col = ifelse(preds[i] == 0, 2, 4), 
  # legend("topright", legend = ifelse(preds[i] == 0, "DRY", "FLOOD"),
  #        text.col = ifelse(preds[i] == 0, 2, 4), bty = "n", text.font = 2)
  #legend("topleft", legend = pred_prob[i,][which.max(pred_prob[i,])], bty = "n", col = "white")
}

#save the model:
modelType<-"dry_flood_snow"
save(model, file = paste0("C:/Users/jroberti/Git/phenocam-cv/data/floodDetection/CNNmodel_",modelType,".RData"))
#gsub(" |:","_",Sys.time())


