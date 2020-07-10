#Neural Network Example:
#https://www.datacamp.com/community/tutorials/neural-network-models-r

#first, set seed:
set.seed(2020)

library(neuralnet)
# creating training data set
TKS<-c(20,10,30,20,80,30)
CSS<-c(90,20,40,50,50,80)
Placed=c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df<-data.frame(TKS,CSS,Placed)
# fit neural network
nn<-neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
             linear.output = FALSE)
# plot neural network
plot(nn)
# creating test set
TKS=c(30,40,85)
CSS=c(85,50,40)
test=data.frame(TKS,CSS)
## Prediction using neural network
Predict<-compute(nn,test)
Predict$net.result
# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred


##########  run the cat and dog dataset thru the neural network #############
#set seed for traceability:
set.seed(2020)

#set image directory:
imageDir<-"C:/Users/jroberti/Kaggle/imgClassification/train/"
# 
# #open one of the images in the training set:
# library(imager)
# img.exampleCat<-imager::load.image(paste0(imageDir,"/cat.0.jpg"))
# plot(img.exampleCat)
# 
# img.exampleDog<-imager::load.image(paste0(imageDir,"/dog.0.jpg"))
# plot(img.exampleDog)
# 
# 
# ## 2 Data Preprocessing
# ### Convert the images to dims of 40 x 40 and turn into greyscale:
# width <- 40
# height <- 40
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
#   #browser()
#   #only grab 1000 images:
#   #grab only first 1000 images of each
#   images_names<-images_names  #images_names[1:500]
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
# #width<-40
# #height<-40
# #image_dir<-"~/Github/L0-tracking/scratch/imgs/imgClassification/train/"
# #apply above function to the cats data and the dogs data:
# cats_data <- extract_feature(dir_path = imageDir, width = width, height = height)
# dogs_data <-extract_feature(dir_path = imageDir, width = width, height = height,is_cat = F)
# #save the data as .rds just in case:
# saveRDS(cats_data, "C:/Users/jroberti/Kaggle/imgClassification/cat.rds")
# saveRDS(dogs_data, "C:/Users/jroberti/Kaggle/imgClassification/dog.rds")


### open the cat and dog .rds files:



## 3 Train the Model
library(caret)
## Bind rows in a single dataset
complete_set <- rbind(cats_data, dogs_data)
## test/training partitions
training_index <- createDataPartition(complete_set$label, p = .9, times = 1)
training_index <- unlist(training_index)
train_set <- complete_set[training_index,]
dim(train_set)
#create the test set:
test_set <- complete_set[-training_index,]
dim(test_set)

start.time<-Sys.time()
library(neuralnet)
# fit neural network
nn<-neuralnet(label~.,data=train_set, hidden=3,act.fct = "logistic",
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
