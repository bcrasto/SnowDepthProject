
# Last edits on xxxx-xx-xx by Brandon Crasto (explain what you did here)
# Last edits on 2020-04-15 by Mario Bretfeld (streamlined)

# load all required packages
housekeeping <- function() {
  lapply(c("exifr", 
           "roll",
           "ggplot2",
           "imager",
           "magick",
           "dplyr",
           "leaflet",
           "peakPick",
           "nabor",
           "ggpmisc",
           "zoo"
           ), require, character.only = TRUE)
}

########################################################
# SiteInfo   
########################################################
site.info <- function(i){
  
  i = i - 1

  refrencedata <- na.locf(refrencedata)
  

  
  tree.locations <<- data.frame(
    crop.x.1 = c(refrencedata$tree1.crop.x.1[i] , refrencedata$tree2.crop.x.1[i] , refrencedata$tree3.crop.x.1[i]), 
    crop.y.1 = c(refrencedata$tree1.crop.y.1[i] , refrencedata$tree2.crop.y.1[i] , refrencedata$tree3.crop.y.1[i]),
    crop.x.2 = c(refrencedata$tree1.crop.x.2[i] , refrencedata$tree2.crop.x.2[i] , refrencedata$tree3.crop.x.2[i]),
    crop.y.2 = c(refrencedata$tree1.crop.y.2[i] , refrencedata$tree2.crop.y.2[i] , refrencedata$tree3.crop.y.2[i])
  )
  
  fg.bg.locations <<- data.frame(
    fg.x.1 = c(refrencedata$tree1.fg.x.1[i], refrencedata$tree2.fg.x.1[i], refrencedata$tree3.fg.x.1[i]),   # topleft of foreground
    fg.y.1 = c(refrencedata$tree1.fg.y.1[i], refrencedata$tree2.fg.y.1[i], refrencedata$tree3.fg.y.1[i]),
    fg.x.2 = c(refrencedata$tree1.fg.x.2[i], refrencedata$tree2.fg.x.2[i], refrencedata$tree3.fg.x.2[i]), # bottomright of foreground
    fg.y.2 = c(refrencedata$tree1.fg.y.2[i], refrencedata$tree2.fg.y.2[i], refrencedata$tree3.fg.y.2[i]),  
    
    bg.x.1 = c(refrencedata$tree1.bg.x.1[i], refrencedata$tree2.bg.x.1[i], refrencedata$tree3.bg.x.1[i]), # topleft of background
    bg.y.1 = c(refrencedata$tree1.bg.y.1[i], refrencedata$tree2.bg.y.1[i], refrencedata$tree3.bg.y.1[i]),  
    bg.x.2 = c(refrencedata$tree1.bg.x.2[i], refrencedata$tree2.bg.x.2[i], refrencedata$tree3.bg.x.2[i]),
    bg.y.2 = c(refrencedata$tree1.bg.y.2[i], refrencedata$tree2.bg.y.2[i], refrencedata$tree3.bg.y.2[i])  # bottomright of background
  )
  
 output.data <<- data.frame(
  min = NA,
  Q.1 = NA,
  median = NA,
  mean = NA,
  Q.3 = NA,
  max = NA
)  
   
  
}



########################################################
# Load Pictures 
########################################################
load.pictures <- function(plot.im, picture.filename, blur) {

  im <<- isoblur(load.image(paste(picture.filename)),blur)
  
  if (plot.im == T) {  plot(im) }
  print(paste("Loaded", picture.filename, "!"))
}

########################################################
# find reference and cut 
########################################################
select.reference <- function (verbose, plot.im, crop.x.1, crop.x.2, crop.y.1, crop.y.2) {
  
  if (verbose == T) { print("Finding Reference and Cut...") }
  
crop <<- imsub(im,
               x %inr% c(round(dim(im)[1]* crop.x.1),
                         round(dim(im)[1]* crop.x.2)),
               y %inr% c(round(dim(im)[2]* crop.y.1),
                         round(dim(im)[2]* crop.y.2)))
    if (plot.im == T) { plot(crop) }

HOld <<- crop
crop.gray <- grayscale(crop, method = "Luma", drop = TRUE)
test <<- rbind.data.frame(test, sd(crop.gray))

}

########################################################
# Find Fore-/Background 
########################################################
find.fg.bg <- function(verbose, 
                       fg.x.1, fg.x.2, fg.y.1, fg.y.2,
                       bg.x.1, bg.x.2, bg.y.1, bg.y.2) {
  
  if (verbose == T) { print("Finding Fore- and Background...") }
  
  
  #get foreground/background independent of picture resolution
  fg.1 <- c(round(dim(crop)[1]*fg.x.1), round(dim(crop)[2]*fg.y.1),
            round(dim(crop)[1]*fg.x.2), round(dim(crop)[2]*fg.y.2))
  
  bg.1 <- c(round(dim(crop)[1]*bg.x.1), round(dim(crop)[2]*bg.y.1),
            round(dim(crop)[1]*bg.x.2), round(dim(crop)[2]*bg.y.2))
  
  

  #Corresponding pixel sets
  px.fg <<- ((Xc(crop) %inr% fg.1[c(1,3)]) & (Yc(crop) %inr% fg.1[c(2,4)]))
  px.bg <<- ((Xc(crop) %inr% bg.1[c(1,3)]) & (Yc(crop) %inr% bg.1[c(2,4)]))
  
  #Highlight to show it on img
  highlight(px.fg)
  highlight(px.bg,col="blue")
  im.lab <<- sRGBtoLab(crop)
  
}
########################################################
#Reshape image data into matrix with 3 columns 
########################################################

process.image <- function(verbose, plot.im, im){
  if (verbose == T) { print("Processing Image to B/W...") }
  
  fknn <- function(X,Xp,cl,k=1){
    out <- nabor::knn(X,Xp,k=k)
    cl[as.vector(out$nn.idx)] %>% matrix(dim(out$nn.idx)) %>% rowMeans
  }
  cvt.mat <- function(px) matrix(im.lab[px],sum(px)/3,3)
  fgMat <- cvt.mat(px.fg)
  bgMat <- cvt.mat(px.bg)
  labels <- c(rep(1,nrow(fgMat)),rep(0,nrow(bgMat)))
  testMat <- cvt.mat(px.all(im))
  out <- fknn(rbind(fgMat,bgMat),testMat,cl=labels,k=5)
  msk <<- as.cimg(rep(out,3),dim=dim(im))
  #result
  if (plot.im == T) { plot(msk) }
  msk
}

########################################################
# find snow-tree boundary 
########################################################

find.boundary <- function(verbose, i, j) {
  
  if (verbose == T) { print("Finding Snow-Tree Boundary...") }
  boundary.df <- data.frame(index = 1:dim(msk)[1],
                            boundary = rowSums(msk))
  
 plot(boundary~index, data = boundary.df, type = "l")

 boundary.df$boundary
 
 output.data[1,1]  <<- summary(boundary.df$boundary)[1]
 output.data[1,2]  <<- summary(boundary.df$boundary)[2]
 output.data[1,3]  <<- summary(boundary.df$boundary)[3]
 output.data[1,4]  <<- summary(boundary.df$boundary)[4]
 output.data[1,5]  <<- summary(boundary.df$boundary)[5]
 output.data[1,6]  <<- summary(boundary.df$boundary)[6]
 output.data$Tree  <<- i
 output.data$Picture  <<- j
}


########################################################
# crop bottom half ##not used
########################################################
select.bottom_half <- function (verbose, plot.im, crop.y.1, crop.y.2) {
  if (verbose == T) { print("Selecting Bottom Half") }
  crop <<- imsub(-msk,
                 x %inr% c(round(dim(im)[1]* 0),
                           round(dim(im)[1]* 1)),
                 y %inr% c(round(dim(im)[2]* crop.y.1),
                           round(dim(im)[2]* crop.y.2))) %>% if (plot.im == T) { plot }
}





########################################################
# find boundary values and store to data frame
########################################################
# find.tree.boundary <- function(crop.y.1, crop.y.2, width.pixel){
#   print("Finding Boundary? ...")
#     
#   boundary.df <- data.frame(index = 1:dim(im)[1],
#                             boundary = rowSums(-crop))
#   
#   plot(boundary~index, data = boundary.df, type = "l", ylim = c(0, 1400))
#   
#   boundary.df$plateaus <-c(NA, ifelse((abs(diff(boundary.df$boundary)) <= 15) == T, 
#                                       boundary.df$boundary, NA))
#   
#   boundary.df$plateaus.filled <- roll_mean(boundary.df$plateaus,
#                                            width = width.pixel,
#                                            weights = rep(1, width),
#                                            min_obs = width.pixel,
#                                            complete_obs = F,
#                                            na_restore = FALSE,
#                                            online = TRUE)
#   
#   lines(boundary.df$plateaus.filled, col = "red", type = "l", ylim = c(0, 1400), lwd = 5)
#   
# }


########################################################
# actually do all things above
########################################################

loop.through.images <- function(verbose, plot.im, first.pic, last.pics, pictures.list, blur) {
  
  #number of pictures
  for(j in first.pic : last.pics){
    y <- data.frame()
    
    load.pictures(plot.im, picture.filename = pictures.list[j], blur)
    
    
    #number of trees
    for(i in 1:3){
      
      # select reference point and cut
      select.reference(verbose, plot.im,
                       crop.x.1 = tree.locations$crop.x.1[i], 
                       crop.y.1 = tree.locations$crop.y.1[i],
                       crop.x.2 = tree.locations$crop.x.2[i],
                       crop.y.2 = tree.locations$crop.y.2[i])
      
      # find fore (red)- and background (blue) and highlight ###########################Changed fg and bg values
      find.fg.bg(verbose,
                 fg.x.1 = fg.bg.locations$bg.x.1[i],
                 fg.y.1 = fg.bg.locations$bg.y.1[i],  # topleft of foreground
                 fg.x.2 = fg.bg.locations$bg.x.2[i],
                 fg.y.2 = fg.bg.locations$bg.y.2[i],  # bottomright of foreground
                 
                 bg.x.1 = fg.bg.locations$fg.x.1[i], 
                 bg.y.1 = fg.bg.locations$fg.y.1[i],  # topleft of background
                 bg.x.2 = fg.bg.locations$fg.x.2[i], 
                 bg.y.2 = fg.bg.locations$fg.y.2[i])  # bottomright of background
      
      #imager::save.image(crop,  paste0("C:/Users/branc/Desktop/New folder (3)/cropSaves",i,"/",number,"_",j,"_",i,".jpeg"))
      
      # convert to black & white
      process.image(verbose, plot.im, im = crop)
     
      
      
      # find snow-tree boundary
      find.boundary(verbose, i, j)
      
      # adding current data to existing data 
      y <- rbind.data.frame(y ,output.data)
      
      ###################################################################################move this to crop? 
      #test <<- rbind.data.frame(test, sd(im = crop))
      
    } 
    # compiling existing data into a bigger list
    x <<- rbind.data.frame(x, y)
    
  }
}

########################################################
# adjust # rows of exifr output to match snow depth output
########################################################

stitch.results <- function(first.pic, last.pics, store) {
  
  for ( i in first.pic:last.pics){
    
    for( j in 1:3){ 
      z <<- rbind.data.frame(z ,store[i,])  
    }
    
    #print("--------------------------------------------------------------------")
  }
  setwd(PictureDirectory)
  final.result <- cbind.data.frame(x, z) 
}

