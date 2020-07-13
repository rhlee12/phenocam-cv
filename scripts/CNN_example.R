### Convolutional Neural Networks in R ###
#https://www.r-bloggers.com/convolutional-neural-networks-in-r/

library(keras)
library(EBImage)
library(stringr)
library(pbapply)


#set image directory:
imageDir<-"C:/Users/jroberti/Kaggle/imgClassification/train/"

# Set image size
width<-50
height<-50
library(pbapply)
extract_feature <- function(dir_path, width, height, is_cat = TRUE, add_label = TRUE) {
  img_size <- width*height
  ## List images in path
  images_names <- list.files(dir_path)
  if (add_label) {
    ## Select only cats or dogs images
    images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
    ## Set label, cat = 0, dog = 1
    label <- ifelse(is_cat, 0, 1)
  }
  #browser()
  #only grab 1000 images:
  #grab only first 1000 images of each
  images_names<-images_names[1:1000]
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- imager::load.image(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- imager::resize(img, size_x = width, size_y =  height)
    ## Set to grayscale
    grayimg <- imager::grayscale(img_resized)
    ## Get the image as a matrix
    img_matrix <- as.matrix(grayimg)
    #grayimg@.Data
    ## Coerce to a vector
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    ## Add label
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}

cats_data <- extract_feature(dir_path = imageDir, width = width, height = height)
dogs_data <-extract_feature(dir_path = imageDir, width = width, height = height,is_cat = F)

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
## Fix train and test datasets
train_data <- data.matrix(train_set)
train_x <- t(train_data[, -1])
train_y <- train_data[,1]
train_array <- train_x
#dim(train_array) <- c(40, 40, 1, ncol(train_x))

test_data <- data.matrix(test_set)
test_x <- t(test_set[,-1])
test_y <- test_set[,1]
test_array <- test_x
#dim(test_array) <- c(40, 40, 1, ncol(test_x))


#flatten the images:
# Fix structure for 2d CNN
# Fix structure for 2d CNN
train_array %>%
layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = "adam",
  metrics = c('accuracy')
)

history %>% fit(
  x = train_array, y = as.numeric(trainData$y),
  epochs = 30, batch_size = 100,
  validation_split = 0.2
)

plot(history)