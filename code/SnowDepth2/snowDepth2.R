#snowDepth2
# create a blank slate 
rm(list = ls(all.names = TRUE))

master.folder <- "~/OneDrive - Kennesaw State University/Students/Brandon Crasto"
PictureDirectory <- "~/OneDrive - Kennesaw State University/Research/Chimney/Data/BB-UF/CamPhotos"

master.folder <- "C:/Users/branc/Desktop/New folder (4)/SnowDepthProject"
PictureDirectory <- "C:/Users/branc/Desktop/New folder (3)/snow"

#Set Directories
working.dir          <- paste0(master.folder,"/code/SnowDepth2")      # location of R code files
DataDirectory        <- paste0(master.folder,"/Data/snowDepth2/v11")      # location of output
CalibrationDirectory <- paste0(master.folder,"/Calibration")   # location of calibration images
setwd(working.dir)


getwd()
#Load Functions
source("snowDepth2_functions.R")

#Load Packages
housekeeping()

# get calibration values
# should create a df with avrg. snow values for each 2-hour window (2, 4, 6, ...)
#get_calibration_values()

#Extract MetaData
get.Metadata() # want this to be by-folder


#Go through each image and get R G B means and percentage
loop.through.images(plot.im = F, 
                    verbose = F, 
                    first.pic = 490 , 
                    last.pics = number.pics,
                    pictures.list = pictures.list)

#store in a timestamp format and filter by given hour of day
bla <- time.filter(metaData,
            min_hours = 0,
            max_hours = 24)

# generate output
write.csv(metaData,paste0(DataDirectory,"/MetaData.csv"))
write.csv(bla,paste0(DataDirectory,"/MetaData_filtered.csv"))



metaData <- read.csv(paste0(DataDirectory,"/","MetaData.csv"))
bla <- read.csv(paste0(DataDirectory,"/","MetaData_filtered.csv"))

#######################################################

install.packages("magick")
library(magick)

imagePath = "C:/Users/branc/Desktop/New folder (3)/Temp/IMAG0016.JPG"
frink <- image_read(imagePath)
frink <- image_crop(frink, "3648x2736+0+1500")

image_write(frink, path = imagePath, format = "JPG")



#plot color graph
colordistance::plotPixels(imagePath, lower = NULL, upper = NULL, n = 5000)

kmeans.clusters <- colordistance::getKMeanColors(imagePath, n = 2, plotting = T)








