## CNN example 3
#https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_classification/

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

dim(train_images)
dim(train_labels)

#each label is between 0 and 9:
train_labels[1:20]

dim(test_images)
dim(test_labels)

#Preprocess the data:
library(tidyr)
library(ggplot2)

image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")


#It's important that the training set and testing set are preprocessed in the same way:
train_images <- train_images / 255
test_images <- test_images / 255


#Display the first 25 images from the training set and display the class name below each image. 
#Verify that the data is in the correct format and we're ready to build and train the network.
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

#Build The Model:
#Building the neural network requires configuring the layers of the model, then compiling the model.
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

#Compile The Model:
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

#Train the Model:
model %>% fit(train_images, train_labels, epochs = 5, verbose = 2)

#Evaluate the Accuracy:
score <- model %>% evaluate(test_images, test_labels, verbose = 0)
cat('Test loss:', score$loss, "\n")
cat('Test accuracy:', score$acc, "\n")

#Make predictions:
predictions <- model %>% predict(test_images)
#Here, the model has predicted the label for each image in the testing set. Let's take a look at the first prediction:
predictions[1, ]
#A prediction is an array of 10 numbers. 
#These describe the "confidence" of the model that the image corresponds to each of the 10 different articles of clothing. 
#We can see which label has the highest confidence value:
which.max(predictions[1, ])
#Alternatively, we can also directly get the class prediction:
class_pred <- model %>% predict_classes(test_images)
class_pred[1:20]
#As the labels are 0-based, this actually means a predicted label of 9 (to be found in class_names[9]). 
#So the model is most confident that this image is an ankle boot. And we can check the test label to see this is correct:
test_labels[1]
#Let's plot several images with their predictions. Correct prediction labels are green and incorrect prediction labels are red.
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}
#Finally, use the trained model to make a prediction about a single image.
# Grab an image from the test dataset
# take care to keep the batch dimension, as this is expected by the model
img <- test_images[1, , , drop = FALSE]
dim(img)
#Now predict the image:
predictions <- model %>% predict(img)
predictions
#predict returns a list of lists, one for each image in the batch of data. Grab the predictions for our (only) image in the batch:
# subtract 1 as labels are 0-based
prediction <- predictions[1, ] - 1
which.max(prediction)
#Or, directly getting the class prediction again:
class_pred <- model %>% predict_classes(img)
class_pred
