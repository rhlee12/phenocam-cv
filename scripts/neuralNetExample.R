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
cats_data<-readRDS("C:/Users/jroberti/Kaggle/imgClassification/cat.rds")
dogs_data<-readRDS("C:/Users/jroberti/Kaggle/imgClassification/dog.rds")

## 3 Train the Model
library(caret)
## Bind rows in a single dataset
complete_set <- rbind(cats_data, dogs_data)
#make smaller batches of the datasets:
model_set<-complete_set
  complete_set[sample(nrow(complete_set), round(nrow(complete_set)*0.05,0)), ]

#Normalize the data: (probably don't need to do with images because grayscale should take care of this)
#maxs <- apply(model_set, 2, max) 
#mins <- apply(model_set, 2, min)
#scaled <- as.data.frame(scale(model_set, center = mins, scale = maxs - mins))
#train_ <- scaled[index,]
#test_ <- scaled[-index,]


  #complete_set
  

## test/training partitions
training_index <- createDataPartition(model_set$label, p = .9, times = 1)
training_index <- unlist(training_index)
train_set <- model_set[training_index,]
dim(train_set)
#create the test set:
test_set <- model_set[-training_index,]
dim(test_set)


### Using mxnet ###
start.time<-Sys.time()
library(mxnet)
## Fix train and test datasets
train_data <- data.matrix(train_set)
train_x <- t(train_data[, -1])
train_y <- train_data[,1]
train_array <- train_x
dim(train_array) <- c(40, 40, 1, ncol(train_x))

test_data <- data.matrix(test_set)
test_x <- t(test_set[,-1])
test_y <- test_set[,1]
test_array <- test_x
dim(test_array) <- c(40, 40, 1, ncol(test_x))


## Model
mx_data <- mx.symbol.Variable('data')
## 1st convolutional layer 5x5 kernel and 20 filters.
conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2 ))
## 2nd convolutional layer 5x5 kernel and 50 filters.
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
## 1st fully connected layer
flat <- mx.symbol.Flatten(data = pool_2)
fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
## 2nd fully connected layer
fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
## Output
NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)

## Set mx seed for reproducibility
mx.set.seed(100)

## Device used. Sadly not the GPU :-(
device <- mx.cpu()

## Train on 1200 samples
model <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_y,
                                     ctx = device,
                                     num.round = 20, #30
                                     array.batch.size = 100, #100
                                     learning.rate = 0.05,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

## Test test set
predict_probs <- predict(model, test_array)
predicted_labels <- max.col(t(predict_probs)) - 1
table(test_data[, 1], predicted_labels)

#get the accuracy of the model:
sum(diag(table(test_data[, 1], predicted_labels)))/length(test_data[, 1])
Sys.time()-start.time



caret::avNNet	

caret::pcaNNet	




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
