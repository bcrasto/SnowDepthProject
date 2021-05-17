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
           "zoo",
           "lubridate",
           "scatterplot3d",
           "cowplot",
           "tidyverse"
  ), require, character.only = TRUE)
}



########################################################
# Find number of Pictures and get metadata
########################################################
get.Metadata <- function(){

  setwd(PictureDirectory)
  pictures.list <<- list.files(PictureDirectory, pattern = ".JPG", recursive = TRUE)
  number.pics <<- length(pictures.list)
  cat("there are ",paste(number.pics)," pictures")
  metaData <<- read_exif(pictures.list,
                     tags = c("FileName", "ImageWidth", "ImageHeight", "CreateDate"))
  
  metaData <<- cbind.data.frame(metaData, PictureDirectory,
                                     R = NA,
                                     G = NA,
                                     B = NA,
                                     Pct = NA)  
  
  #initialize progress bar
  pb <<- progress::progress_bar$new(
    format = "  progress [:bar] :percent eta: :eta",
    total = number.pics, clear = FALSE, width= 100)
  
}


########################################################
# actually loop through images 
########################################################
loop.through.images <- function(verbose, plot.im, first.pic, last.pics, pictures.list) {

  #number of pictures
  for(j in first.pic : last.pics){
      
      #img <- jpeg::readJPEG(paste0(list.files(PictureDirectory, pattern = ".JPG")[j]))
      imag <- paste0(metaData$PictureDirectory[j],"/", metaData$SourceFile[j])
      
      imag <- image_read(imag)
      imag <- image_crop(imag, "3648x2736+0+1500")
      imagePath <- paste0("C:/Users/branc/Desktop/New folder (3)/Temp/Temp.JPG")
      image_write(imag, path = imagePath, format = "JPG")
      imag <- imagePath
      
      
      if (verbose == T) {cat("Currently on image number",j, "\n")}
      
      #plot image
      
      
      
      #plot color graph
      colordistance::plotPixels(imag, lower = NULL, upper = NULL, n = 5000)
      
      kmeans.clusters <- colordistance::getKMeanColors(imag, n = 2, plotting = T)
    
      
      center <<- colordistance::extractClusters(kmeans.clusters)
      #center_out <<- center
      center <<- center[order(center$Pct),]
      center <<- center[1,]
      
      metaData$R[j] <<- center$R
      metaData$G[j] <<- center$G
      metaData$B[j] <<- center$B
      metaData$Pct[j] <<- center$Pct
      
      #progress Bar
      pb$tick()
      #Sys.sleep(1 / 100)

    
  }
  
}
###########################################################################
#store in a timestamp format
###########################################################################
time.filter <- function(store, min_hours, max_hours){
  store$Timestamp <- parse_date_time(store$CreateDate, orders = c('y:m:d HMS'))
  str(store)
  
  store$Hours <- hour(store$Timestamp)
  store$Date <- as.Date(store$Timestamp)
  store$Hours <- hour(store$Timestamp)
  store$Day <- day(store$Timestamp)
  store$Month <- month(store$Timestamp)
  
  
  store %>% filter(Hours >= min_hours & Hours <= max_hours)
  
}


###################################################
#Plotting latest attempt
###################################################

plot.latest <- function(Pct.min, B.min, mean_Pct.min ){
  

  
  data.v3 <<- data.in %>% 
    filter(Pct > 0.2 & B > 0.2) %>% 
    filter(Hours != lag(Hours, 1)) %>%
    group_by(Timestamp = as.Date(Timestamp))%>%
    summarise(photo_Snow_mean = mean(Pct),
              SD_Pct = sd(Pct),
              N = n()) %>%
    #filter(mean_Pct > mean_Pct.min) %>% 
    mutate(photo_Snow_norm = photo_Snow_mean/max(photo_Snow_mean),
           perc_SD = SD_Pct/photo_Snow_mean,
           Timestamp = as.POSIXct(Timestamp)) # %>%
    #filter(perc_SD <= 0.3 &                    # filter where SD is xx% of mean value (i.e. high noise)
     #        N >= 4)                            # filter where fewer than xx photos per day (storms etc)
  
  #write.csv(data.v3,paste0(master.folder,"/data.v3.csv"))
  
  ggplot(data = data.v3, 
         aes(x = Timestamp, 
             y = photo_Snow_mean)) +
    geom_ribbon(aes(ymin = photo_Snow_mean - SD_Pct,
                    ymax = photo_Snow_mean + SD_Pct),
                alpha = 0.3) +
    geom_line() +  
    geom_point() +
    theme_linedraw() +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    #geom_smooth() + 
    theme(axis.text.x=element_text(angle = 60, hjust = 1))+
    labs(title = paste("Attempt 2")) +
    scale_x_datetime(breaks = "1 month") #+
  #geom_line(aes(y=rollmean(mean_Pct, 7, na.pad=TRUE))) 
  
}
###############################################################################
#Plot 3 graphs
###############################################################################
plot.graphs <- function(all_data_long, startDate1, endDate1, startDate2, endDate2, startDate3, endDate3){
 # all_data_long <- mergeData  %>%
  #  pivot_longer(cols = c(normData, real_Snow_norm),
   #              names_to = "Dataset", 
    #             values_to = "Snow_norm")
  
  #Plot
  p1 <- ggplot(all_data_long) +
    geom_line(aes(x = Timestamp,
                  y = Snow_norm, 
                  color = Dataset)) + 
    theme_linedraw() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_datetime(limits = c(as.POSIXct(startDate1),as.POSIXct(endDate1)))+    
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  #Plot 2
  p2 <- ggplot(all_data_long) +
    geom_line(aes(x = Timestamp,
                  y = Snow_norm, 
                  color = Dataset)) + 
    theme_linedraw() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_datetime(limits = c(as.POSIXct(startDate2),as.POSIXct(endDate2)))+    
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  #Plot Year
  p3 <- ggplot(all_data_long) + 
    geom_line(aes(x = Timestamp,
                  y = Snow_norm, 
                  color = Dataset)) + 
    theme_linedraw() +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_datetime(limits = c(as.POSIXct(startDate3),as.POSIXct(endDate3)))+    
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  
  
  
  
  bottom_row <- plot_grid(p1,p2, ncol = 2)
  
  
  plot_grid(bottom_row,p3, labels = "", nrow = 2, rel_heights = c(1,1))
}

###############################################################################
#Plot 3 graphs snowdepth1
###############################################################################
plot.graphs2 <- function(all_data_long2, startDate1, endDate1, startDate2, endDate2, startDate3, endDate3){
  
  #Plot
  p1 <- ggplot(all_data_long2) +
    geom_point(aes(x = as.POSIXct(Timestamp),
                  y = Snow_norm, 
                  color = Dataset)) + 
    theme_linedraw() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_datetime(limits = c(as.POSIXct("2019-04-01"),as.POSIXct("2019-05-01"))) +
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  p1
  
  #Plot 2
  p2 <- ggplot(all_data_long2) +
    geom_point(aes(x = as.POSIXct(Timestamp),
                  y = Snow_norm, 
                  color = Dataset)) + 
    theme_linedraw() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_datetime(limits = c(as.POSIXct("2019-10-01"),as.POSIXct("2019-12-31")))+
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  #Plot Year
  p3 <- ggplot(all_data_long2) + 
    geom_point(aes(x = as.POSIXct(Timestamp),
                  y = Snow_norm, 
                  color = Dataset)) + 
    theme_linedraw() +
    theme(panel.border = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_datetime(limits = c(as.POSIXct("2019-03-15"),as.POSIXct("2019-12-31")))+
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  
  
  bottom_row <- plot_grid(p1,p2, ncol = 2)
  
  
  plot_grid(bottom_row,p3, nrow = 2, rel_heights = c(1,1))
}
##########################################################################
#Linear Model
##########################################################################
linear.model <- function(mergeData, range.min, range.max, photo_Snow_norm, real_Snow_norm){
  
  
  
  mergeData2 <- mergeData %>% filter(Timestamp >= range.min & Timestamp <= range.max)
  
  # linear model approach
  plm <- ggplot(mergeData2, aes(x = photo_Snow_norm,
                        y = real_Snow_norm)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm", fullrange = T) +
    geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(limits = c(0, NA))
  
  print(plm)
  print(summary(lm_out <- lm(real_Snow_norm ~ photo_Snow_norm, data = mergeData2)))
  
}



###########################################################################
###########################################################################
###########################################################################
# Function Graveyard
###########################################################################


########################################################
# Find Pictures   
########################################################
find.pictures <- function(PictureDirectory){
  image.files <<- list.dirs(path = PictureDirectory, full.names = TRUE, recursive = TRUE)
  image.files
  
}

###########################################################################
#check condition to store data/check if there is anything in df
###########################################################################
condition.check <- function(store, metaData){
  if(nrow(store) == 0){
    store <<- metaData
  }
  else
  {
    store <<- rbind(store, metaData)
  } 
}



