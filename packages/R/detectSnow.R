#' @title Get NEON (TIS)Phenocam Image URLs
#'
#' @author Robert 'n' Josh
#'
#' @details Will return a list of URLs to phenocam images at a TIS site, for a given year and time of day.
#'
#' @param imagePath the file path or URL to a phenocam image.
#' @param rgSpan_Threshold The absolute maximum span between red and green values for a pixel to count as 'white' (snow) Defaults to 0.25, meaning a set of R=.75, G=0.8 will count, R=0.4, G=0.7 will not.
#' @param rbSpan_Threshold The maximum difference between blue and red values. Defaults to 0.6. The value is the threhsold for B-R values, meaning B must always be larger than R values for a positve value specified in this parameter.
#' @param white_Threshold The minimum value all RGB values to have to count as white. For example R=0.6, G=0.9, B=0.5 would satisfy this threshold (but fail the rgSpan_Threshold and rbSpan_Threshold). Defaults to 0.4.
#'
#' @return The portion of the image passed to the function which can be classified as snow, as a decimal percent.
#'
#' @export
#'


detectSnow=function(imagePath, rgSpan_Threshold=0.25, rbSpan_Threshold=0.6,  white_Threshold=0.4){
  # Define what thresholds we need for white.
  #rgbSpan_Threshold=0.1 # how close must RGBs be to count as snow
  #whiteThreshold=0.4 # What is the minimum value all RGBs must have to count

  # Vsetigial code correction
  latestImgUrl=imagePath

  # get the latest image, load with magick
  latestImg=magick::image_read(latestImgUrl)

  #where will the processed image live?

  processedImgPath=paste0(tempdir(), "/", site, "_pheno.jpg")
  # processedTiff=paste0("./", site, "_pheno.tiff")


  # # Trim off the top 90 pixels of img, where the info lives
  # # line 1 of camera metadata
  # magick::image_crop(latestImg, geometry = "859x27+0+0") %>% # Crop to first row
  #   magick::image_modulate(saturation = 0, brightness = 80) %>% # Adjust color and brightness to make it easier
  #   magick::image_negate() %>% # flip colors (white -> black, red -> green, etc)
  #   magick::image_contrast(sharpen = 1000) %>% # up the contrast a ton
  #   magick::image_enhance() %>% # clean up any noise
  #   magick::image_write(path = "./metadata1.png", image = .) #save it out!
  # # line 2 of camera metadata
  # magick::image_crop(latestImg, geometry = "310x27+0+30") %>%
  #   magick::image_modulate(saturation = 0, brightness = 80) %>%
  #   magick::image_negate() %>%
  #   magick::image_contrast(sharpen = 1000) %>%
  #   magick::image_enhance() %>%
  #   magick::image_write(path = "./metadata2.png", image = .)
  # # line 3 of camera metadata
  # magick::image_crop(latestImg, geometry = "138x24+0+60") %>%
  #   magick::image_modulate(saturation = 0, brightness = 80) %>%
  #   magick::image_negate() %>%
  #   magick::image_contrast(sharpen = 1000) %>%
  #   magick::image_enhance() %>%
  #   magick::image_write(path = "./metadata3.png", image = .)
  #
  #
  # #Use OCR to parse the info out! (They probably have this on the API but that's lame)
  # metadata=c(tesseract::ocr("./metadata1.png"),
  #            tesseract::ocr("./metadata2.png"),
  #            tesseract::ocr("./metadata3.png"))

  # Clean up our temporary metadata images
  #file.remove(c("./metadata1.png", "./metadata2.png", "./metadata3.png"))

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
  whiteRows=(rowSums(kMeans$centers>white_Threshold)==3 &
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
