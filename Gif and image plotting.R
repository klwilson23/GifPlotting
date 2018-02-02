# instructions:
# download imagemagick for your OS.
# during install, click on the 'install legacy files'
# install the packages below:
# (1) animation (you may need to install devtools() and then use install_github for this)
# animation package is the key R package to communicate with ImageMagick to make the gif
# (2) raster: allows us to plot the customized images onto the plotting surface
# (3) png: allows us to read in PNG files into a matrix-like format that can be used in R via raster
# (4) jpeg: allows us to read in JPEG files into a matrix-like format that can be used in R via raster

rm(list=ls(all=TRUE))

setwd("~/Google Drive/Gif Examples/")

#library(devtools)
#install_github("yihui/animation",force=TRUE)
library(animation)
library(raster)
library(png)
library(jpeg)
library(plot3D)
jpgIMG <- readJPEG("pic.jpg")
pngimg <- readPNG("SarahTrout.png")

# let's use the mtcars dataset

attach(mtcars) 

m1 <- lm(mpg~wt+I(wt^2)+disp)

newDat <- data.frame("wt"=seq(from=min(wt),to=max(wt),length=50),"disp"=seq(from=min(disp),to=max(disp),length=50))
predMPG <- matrix(NA,nrow=50,ncol=50)
for(i in 1:length(newDat$wt))
{
  for(j in 1:length(newDat$disp))
  {
    predMPG[i,j] <- predict(m1,newdata=data.frame("wt"=newDat$wt[i],"disp"=newDat$disp[j]),type="r") 
  }
}

# let's plot an z ~ f(x,y), but have the perspective for the 3-D plot move.
saveGIF(for(i in 1:360){
  persp(newDat$wt,newDat$disp,predMPG,xlab="Car weight",ylab="Engine displacement",zlab="MPG",theta=i,phi=20)
},interval=0.05,movie.name="3d perspective example.gif",ani.width=1080, ani.height=900)

# in the above saveGIF() function, interval sets the speed of the plots. width and height sets the resolution. movie.name sets the file name.

# let's plot an z ~ f(x,y), but have the perspective for the 3-D plot move.
saveGIF(for(i in 1:360){
  scatter3D(wt, disp, mpg,phi=20,theta=i,type="h",clab="MPG",xlab="Car weight",ylab="Engine displacement",zlab="MPG",bty="g",col=ramp.col(c("dodgerblue", "orange")),pch=19,lwd=1.5)
},interval=0.05,movie.name="3d scatterplot example.gif",ani.width=1080, ani.height=900)

# let's plot an z ~ f(x,y), but have the perspective for the 3-D plot move.

pal <- colorRampPalette(c("dodgerblue", "orange"))
saveGIF(for(i in 1:length(mtcars$wt)){
  scatter3D(wt, disp, mpg,phi=20,theta=30,type="p",clab="MPG",xlab="Car weight",ylab="Engine displacement",zlab="MPG",bty="g",col=pal(length(mpg)),pch=NA,lwd=1.5)
  points3D(wt[1:i],disp[1:i],mpg[1:i],pch=19,add=TRUE,col=pal(length(mpg)),colkey=FALSE,type="h")
},interval=1,movie.name="3d adding points example.gif",ani.width=1080, ani.height=900)

# col=pal(length(mpg)+1)[mpg[1:i][order(mpg[1:i])]]

# make some fake data
x <- 1:50
m <- 0.5
c <- 20
b <- 2
y <- m*x+c*x^2+b

# below uses the dimensions of the images to make sure the custom image is plotted in the same dimensions as it is input
# raster plots the custom image with x,y coordinates (bottom, left, top, right)
# hence 'offsets' are used
xoffset <- dim(jpgIMG)[2]/(dim(jpgIMG)[1]+dim(jpgIMG)[2])
yoffset <- 1-xoffset

# this uses those dimensions and scales them to fit on your plot. trial-and-error this scalar. sd(variable) might be useful to constrain the image to the axes of your plot
xoffset <- 0.25*xoffset*sd(x)
yoffset <- 0.25*yoffset*sd(y)

# use the for() loop to make many plots. the saveGIF() function compresses all those individual plots into one GIF. it then automatically deletes those files, and just saves the single GIF.
saveGIF(for(i in x){
  plot(x[i],y[i],xlim=1.5*range(x),ylim=1.5*range(y),col="0")
  xcenter <- x[i]
  ycenter <- y[i]
  rasterImage(jpgIMG,xleft=(xcenter-xoffset),xright=(xcenter+xoffset),
              ybottom=(ycenter-yoffset),ytop=(ycenter+yoffset))
},interval=0.1,movie.name="example.gif",ani.width=1080, ani.height=900)

# let's add a background image, then plot over it. useful if you have some image you'd like to demonstrate something with.

xoffset <- dim(pngimg)[2]/(dim(pngimg)[1]+dim(pngimg)[2])
yoffset <- 1-xoffset

xoffset <- 0.35*xoffset*sd(x)
yoffset <- 0.35*yoffset*sd(y)

saveGIF(for(i in x){
  plot(x[i],y[i],xlim=range(x),ylim=range(y),col="0")
  rasterImage(jpgIMG,xleft=min(x),xright=max(x),ybottom=min(y),ytop=max(y))
  xcenter <- x[i]
  ycenter <- y[i]
  rasterImage(pngimg,xleft=(xcenter-xoffset),xright=(xcenter+xoffset),
              ybottom=(ycenter-yoffset),ytop=(ycenter+yoffset))
},interval=0.1,movie.name="fish example.gif",ani.width=1080, ani.height=900)

# let's use an image and move with it with a systematic function like a sine-wave.

xoffset <- dim(pngimg)[2]/(dim(pngimg)[1]+dim(pngimg)[2])
yoffset <- 1-xoffset

x <- seq(1,20,by=0.1)
xoffset <- 0.5*xoffset*sd(x)
yoffset <- 0.5*yoffset*sd(sin(x))

saveGIF(for(i in 1:length(x)){
  plot(x[i],sin(x[i]),xlim=range(x),ylim=range(sin(x)),col="0")
  if(i>1){lines(x[1:i],sin(x[1:i]),lwd=1.5,lty=3,col="orange")}
  xcenter <- x[i]
  ycenter <- sin(x[i])
  rasterImage(pngimg,xleft=(xcenter-xoffset),xright=(xcenter+xoffset),
              ybottom=(ycenter-yoffset),ytop=(ycenter+yoffset))
  points(x[1:i][1:i%%10==0],sin(x[1:i][1:i%%10==0]),pch=21,bg="grey50",cex=0.5)
},interval=0.1,movie.name="sin wave example.gif",ani.width=1080, ani.height=900)

# let's just plot the sine-wave, but have the x-axis move with us.
#ani.options()
#ani.options(interval = 2)
saveGIF(for(i in 11:(length(x)-11)){
  plot(x[i],sin(x[i]),xlim=range(x[(i-10):(i+10)]),ylim=range(sin(x)),col="0")
  lines(x[1:i],sin(x[1:i]),lwd=3,lty=3,col="orange")
  points(x[1:i][1:i%%10==0],sin(x[1:i][1:i%%10==0]),pch=21,bg="grey50",cex=3)
  if(sin(x[i])==max(sin(x[11:(length(x)-11)]))){text(x[i],sin(x[i])*0.9,labels="Maximum y-value")}
},interval=ifelse(sin(x[11:(length(x)-11)])==max(sin(x[11:(length(x)-11)])),5,0.1),movie.name="sin wave no fish example.gif",ani.width=1080, ani.height=900)


