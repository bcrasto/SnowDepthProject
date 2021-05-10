#snowDepth1
# create a blank slate 
rm(list = ls(all.names = TRUE))

master.folder <- "~/OneDrive - Kennesaw State University/Students/Brandon Crasto"
PictureDirectory <- "~/OneDrive - Kennesaw State University/Research/Chimney/Data/BB-UF/CamPhotos"

master.folder <- "C:/Users/branc/Desktop/New folder (3)"
PictureDirectory <- "C:/Users/branc/Desktop/New folder (3)/snow"

#Set Directories
working.dir          <- paste0(master.folder,"/code/SnowDepth1")      # location of R code files
DataDirectory        <- paste0(master.folder,"/Data/snowdepth1/V11")      # location of output
CalibrationDirectory <- paste0(master.folder,"/Calibration")   # location of calibration images
refrenceDirectory    <- paste0(master.folder,"/treeParameters")   # location of refrence points
setwd(working.dir)


# load functions
source("Snow_fcts.R")

# load all required packages
housekeeping()


# get information on pictures
image.folders <- list.dirs(path = PictureDirectory, full.names = TRUE, recursive = TRUE)


refrencedata <- read.csv(file.path(refrenceDirectory, "refrencedata.csv"), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
test <- data.frame()


i=2
for(i in 2:25) {
  site.info(i)
  PictureDirectory = image.folders[i]
  setwd(PictureDirectory)
  pictures.list <- list.files(PictureDirectory, pattern = ".JPG")
  last.pics <- length(list.files(PictureDirectory))
  x <- data.frame()
  z <- data.frame()
  
  cat("there are ",paste(last.pics)," pictures")
  
  store <- read_exif(pictures.list,
                   tags = c("FileName", "ImageWidth", "ImageHeight", "CreateDate"))


  # loop through images and process them
  loop.through.images(plot.im = F, verbose = F, first.pic =1 , last.pics = last.pics,
                    pictures.list = pictures.list, blur = 3)



  #make arguments match
  stitch.results(first.pic = 1, last.pics = last.pics, store)

  #combine both df
  setwd(PictureDirectory)
  final.result <- cbind.data.frame(x, z) 

  # plot results
  setwd(PictureDirectory)

  write.csv(final.result,paste0(DataDirectory,"/SnowUF2019_",i,".csv"))

}


  
###########################
msk
dim(msk)
-msk
plot(msk)
summary(msk)
