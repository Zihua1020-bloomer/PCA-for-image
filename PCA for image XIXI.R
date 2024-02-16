# # # # # # # # # # # # # # # # # # 
#     PCA for image XIXI          #
# # # # # # # # # # # # # # # # # # 

# Load the library --------------
library(png)
library(ggplot2)
library(factoextra)
library(tidyverse)
library(jpeg)
library(gridExtra) # put the plot from ggplot2 together


# Load the image ------------
# Read image 
photo <- readJPEG("/Users/zihualai/Desktop/XIXI.jpg")
# structure
str(photo)
# dimension
dim(photo)

# plotting the rasterImage – colour photo-------------
plot(1, type="n") 
rasterImage(photo, 0.6, 0.6, 1.4, 1.4)


# plotting the rasterImage – black & white photo---------------
# summing up RGB shades
photo.sum<-photo[,,1]+photo[,,2]+photo[,,3] 
# dividing by max
photo.bw<-photo.sum/max(photo.sum)	
plot(1, type="n")	
rasterImage(photo.bw, 0.6, 0.6, 1.4, 1.4)
writeJPEG(photo.bw, "photo_bw.jpg")

# PCA ------------
# We are going to break down each color scheme into three data frames
# RGB color matrices
r<-photo[,,1]
g<-photo[,,2]
b<-photo[,,3]

# Then we can apply the PCA separately for each color scheme.
# PCA for each color scheme
r.pca<-prcomp(r, center=FALSE, scale.=FALSE) # PCA for R colour component
g.pca<-prcomp(g, center=FALSE, scale.=FALSE)
b.pca<-prcomp(b, center=FALSE, scale.=FALSE)

# PCA objects into a list
rgb.pca<-list(r.pca, g.pca, b.pca)	# merging all PCA into one object


# the importance of PC-------------------
f1<-fviz_eig(r.pca, main="Red", barfill="red", ncp=5, addlabels=TRUE)
f2<-fviz_eig(g.pca, main="Green", barfill="green", ncp=5, addlabels=TRUE)
f3<-fviz_eig(b.pca, main="Blue", barfill="blue", ncp=5, addlabels=TRUE)
grid.arrange(f1, f2, f3, ncol=3)

# loop for different photos------------
vec<-seq.int(3, round(nrow(photo)), length.out=9)
for(i in vec){
  photo.pca<-sapply(rgb.pca, function(j) {
    new.RGB<-j$x[,1:i] %*% t(j$rotation[,1:i])}, simplify="array")
  assign(paste("photo_", round(i,0), sep=""), photo.pca) # saving as object
  writeJPEG(photo.pca, paste("photo_", round(i,0), "_princ_comp.jpg", sep=""))
}


# Final result that we got-----------
install.packages("/Users/zihualai/Desktop/magick_2.8.2.tgz", repos = NULL, type = "source")
library(magick)
library(png)
library(abind)

pp=10 
photo.pca2<-abind(r.pca$x[,1:pp] %*% t(r.pca$rotation[,1:pp]),
                  g.pca$x[,1:pp] %*% t(g.pca$rotation[,1:pp]),
                  b.pca$x[,1:pp] %*% t(b.pca$rotation[,1:pp]),
                  along=3)
plot(image_read(photo.pca2))

# check how the size of photo decreases under this PS compression--------
install.packages("/Users/zihualai/Desktop/metrix_1.1.0.tgz", repos = NULL, type = "source")
library(metrix)

# define the function of mse
mse <- function(true, predicted) {
  mean((true - predicted) ^ 2)
}

sizes<-matrix(0, nrow=9, ncol=4)
colnames(sizes)<-c("Number of PC", "Photo size", "Compression ratio", "MSE-Mean Squared Error")
sizes[,1]<-round(vec,0)
for(i in 1:9) {
  path<-paste("photo_", round(vec[i],0), "_princ_comp.jpg", sep="")
  sizes[i,2]<-file.info(path)$size 
  photo_mse<-readJPEG(path)
  sizes[i,4]<-mse(photo, photo_mse) # from Metrics::
}
sizes[,3]<-round(as.numeric(sizes[,2])/as.numeric(sizes[9,2]),3)
sizes

# So, choose number of PC as 80

