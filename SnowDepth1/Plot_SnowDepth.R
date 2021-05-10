#graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load packages (make sure all are installed)
require("tidyverse"); require("lubridate"); require("grid")
require("ggridges"); require('grDevices'); require('data.table')

# User Input
#data.dir = "~/OneDrive - Kennesaw State University/Students/Brandon Crasto/Snow Trends"


file.identifier <- "SnowUF20"



DataDirectory <- "C:/Users/branc/Desktop/New folder (3)/Data/snowdepth1/v11"
data.dir = DataDirectory

# load data (this will load all csv files with the file identifier specified above and stitch them together in one DF)
setwd(data.dir)

file.list <- list.files(pattern = file.identifier)
file.list

data.raw <- rbindlist(lapply(file.list, read.csv, stringsAsFactors = F,                              # load data
                                 na.strings = c("-9999","NA","NaN","NAN"), 
                                 skip = '0', sep = ',', header = T), fill = TRUE) 

#colnames(test) <- c("Contrast")

data.raw <- cbind.data.frame(data.raw, test) 
#write.csv(data.raw,paste0(data.dir,"/YearRawData.csv"))

# convert date and add hour column
data.raw$Date <- parse_date_time(data.raw$CreateDate, orders = c('ymd HMS'))
data.raw$Hour <- hour(data.raw$Date)

# remove repeated photos due to movement etc. (triggered images, not timelapse)
data.v2 <- data.raw %>% filter(Hour!= lag(Hour,1, order_by = Tree))


# separate nighttime/daytime pictures
H.id <- data.v2$Hour > 6 & data.v2$Hour < 16
data.daytime <- as.data.table(data.v2)[H.id == T]
data.nighttime <- as.data.table(data.v2)[H.id == F]

#data.input <- data.nighttime
data.input <- data.daytime




########################################################################
# Functionsa
########################################################################
# plot function for timeseries
plot.a.tree <- function(subset.data) {

  ggplot(data = subset.data, 
         aes(x = Date, y = normData)) +
    geom_point() +  
    geom_smooth() + theme_gray(base_size = 15) #+   
#    scale_x_datetime(date_breaks = "1 month", date_labels = "%m")
  
}

#plot average of all trees + SD
plot.all.trees <- function(subset.data, startDate1, endDate1, startDate2, endDate2, startDate3, endDate3) {
  
  p1 <- ggplot(data = subset.data, 
          aes(x = Date, y = normData)) +
          geom_line(color = "red")+
          geom_point(color= "red")+  
    theme_linedraw() +
    theme(
      legend.position = "none",
      panel.border = element_rect(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )+
          geom_errorbar(aes(ymin= normData - normDataSD, ymax = normData + normDataSD, color = "red"),width=0.2, alpha = .5)+
          #geom_line(data = realData2019, aes(x = as.POSIXct(Date), y = real_Snow_norm))+
          scale_x_datetime(limits = c(as.POSIXct(startDate1),as.POSIXct(endDate1)))+    
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  
  p2 <- ggplot(data = subset.data, 
               aes(x = Date, y = normData)) +
    geom_line(color = "red")+
    geom_point(color= "red")+  
    theme_linedraw() +
    theme(
      legend.position = "none",
      panel.border = element_rect(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )+
    geom_errorbar(aes(ymin= normData - normDataSD, ymax = normData + normDataSD, color = "red"),width=0.2, alpha = .5)+
   # geom_line(data = realData2019, aes(x = as.POSIXct(Date), y = real_Snow_norm))+
    scale_x_datetime(limits = c(as.POSIXct(startDate2),as.POSIXct(endDate2)))+    
    xlab("Date")+
    ylab("Snow Trend Normalized")
  
  
  p3 <- ggplot(data = subset.data, 
               aes(x = Date, y = normData, color = "red")) +
    geom_line(color = "red")+
    geom_point(color= "red")+  
    theme_linedraw() +
    theme(
      panel.border = element_rect(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )+
    geom_errorbar(aes(ymin= normData - normDataSD, ymax = normData + normDataSD, color = "Trail Camera"),width=0.2, alpha = .5)+
  #  geom_line(color = "black", data = realData2019, aes(x = as.POSIXct(Date), y = real_Snow_norm, color = "Snow Pinger"))+
    scale_x_datetime(limits = c(as.POSIXct(startDate3),as.POSIXct(endDate3)))+    
    xlab("Date")+
    ylab("Snow Trend Normalized")

  
    #+   
  #    scale_x_datetime(date_breaks = "1 month", date_labels = "%m")
  bottom_row <- plot_grid(p1,p2, ncol = 2)
  
  plot_grid(bottom_row,p3, nrow = 2, rel_heights = c(1,1))
}





#with ribbons
plot.all.trees2 <- function(subset.data) {
  
  ggplot(data = subset.data, 
    aes(x = Date, y = normData)) +
    geom_ribbon(aes(ymin= normData - normDataSD, ymax = normData + normDataSD))+
    geom_point() +  
    theme_gray(base_size = 15)

}


# plot function for tree comparison
plot.tree.vs.tree <- function(tree_a, tree_b) {
  
  data.a <- data.input %>% filter(Tree == tree_a)
  colnames(data.a) <- paste0("a.", colnames(data.a))
  data.b <- data.input %>% filter(Tree == tree_b)
  colnames(data.b) <- paste0("b.", colnames(data.b))
  data.ab <- cbind(data.a, data.b)
  
  ggplot(data = data.ab) +
    geom_point(aes(x = a.mean, y = b.mean)) +
    labs(x = paste("Tree", tree_a), y = paste("Tree", tree_b))+
    geom_smooth(aes(x = a.mean, y = b.mean), method = "lm", se = FALSE) +theme_gray(base_size = 15) 
  
}
########################################################################
# Plot things
########################################################################

# plot tree x timeseries
########################
TreeNumb = 1

#pdf(file = paste("Tree",TreeNumb,".pdf"), width = 10, height = 8)


subset.data <- data.input %>% filter(Tree == TreeNumb# , 
                                     #Contrast > 0.02,
                                     #Contrast < 0.2 
                                     )   # specify which tree to plot



store <- subset.data

store$Hours <- hour(store$Date)
store$Date <- as.Date(store$Date)
#store$Day <- day(store$Date)
#store$Month <- month(store$Date)


store <- store %>% filter(Hours >= 9 & Hours <= 15)
store <- store %>%  filter(Hours!= lag(Hours,1))

store$Date <- as.POSIXct(store$Date)

subset.data <- store %>% #mutate(Date = as.POSIXct(as.Date(as.numeric(realData2019$TIMESTAMP_2), origin = "2018-12-31"))) %>% 
  group_by(Date = as.Date(Date))%>%
  summarise(meanSnow = mean(mean)) %>%
  mutate(normData = (meanSnow/max(meanSnow))) #%>% 
  #mutate(normData = normData+(1-max(normData)))


p <- plot.a.tree(subset.data)

p + labs(title = paste("Trend in Snowdepth based on Tree",TreeNumb), 
         subtitle = "sigma value of 3, Daytime",
         caption = paste0(data.dir),
         x="Date",
         y="Average"
         )

#dev.off()

##############################
#Average all trees
############################
#tree_data_mean <- data.input %>% group_by(Picture) %>%
#  mutate(mean_snow = mean(mean))

tree_data_mean <- data.input

store <- tree_data_mean

store$Hours <- hour(store$Date)
store$Date <- as.Date(store$Date)


store <- store %>% filter(Hours >= 9 & Hours <= 15)
#store <- store %>%  filter(Hours!= lag(Hours,1))

store$Date <- as.POSIXct(store$Date)

store_norm <- store %>% mutate(mean = mean*1) %>%
                        group_by(Tree) %>%
                        mutate(all_tree_norm = mean/max(mean)) %>%
                      
                        group_by(Date = as.POSIXct(Date))%>%
                        summarise(normData = mean(all_tree_norm),
                                  normDataSD = sd(all_tree_norm))


#average.data <- store %>% #mutate(Date = as.POSIXct(as.Date(as.numeric(realData2019$TIMESTAMP_2), origin = "2018-12-31"))) %>% 
#  group_by(Date = as.Date(Date))%>%
#  summarise(meanSnow = mean(mean)) %>%
#  mutate(normData = (meanSnow/max(meanSnow))) %>% 
#  mutate(normDataSD = sd(normData))



require(cowplot)


p <- plot.all.trees(store_norm,
                    startDate1 = "2019-04-01",
                    endDate1   = "2019-05-01",
                    
                    startDate2 = "2019-10-01",
                    endDate2   = "2019-12-31",
                    
                    startDate3 = "2019-03-27",
                    endDate3   = "2019-12-31") 
#p <- plot.all.trees(average.data) 

p


p + labs(title = paste("Trend in Snowdepth based on Tree Average"), 
         subtitle = "sigma value of 3, Daytime",
         caption = paste0(data.dir),
         x="Date",
         y="Average"
)





#pdf()
#dev.off()



# plot tree a vs tree b
########################
tree_a <- 2; tree_b <- 3   # specify which two trees to plot against one anothe
#pdf(file = paste("Mean of Tree",tree_a,"as a function of Tree",tree_b,".pdf"))
v <-plot.tree.vs.tree(tree_a, tree_b)

v + labs(title = paste("Mean of Tree",tree_a,"as a function of Tree",tree_b), 
         subtitle = "for the year 2019",
         caption = "(based on data from images from Chimney Park, Wyoming)",
         x=paste("Tree",tree_a),
         y=paste("Tree",tree_b)
)
#dev.off()




#Gettting snow pinger data
###########
realData <- read.csv(paste0(master.folder,"/Data/","Biomet_Data_For_EddyPro.csv"),stringsAsFactors = FALSE)

#filter for 2019
realData2019 <- realData %>% filter(TIMESTAMP_1 == 2019) %>% 
  mutate(Date = as.POSIXct(parse_date_time(paste0(TIMESTAMP_1, "-", TIMESTAMP_2), orders = "Y-j"))) %>%
  group_by(Date) %>%
  summarise(real_Snow_mean = mean(SnowDepth_1_1_1)) %>%
  mutate(real_Snow_norm = real_Snow_mean/max(real_Snow_mean))




p1 <- ggplot(realData2019) +
  geom_line(aes(x = as.POSIXct(Date),
                y = real_Snow_norm
                )) + 
  theme_linedraw() +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_datetime(limits = c(as.POSIXct("2019-01-15"),as.POSIXct("2019-12-30"))) %>% 
  labs(title = paste("snowDepth1"))

p1

#where sd is realsnow, sd = NA

#color 
scale_color_manual(values = c(black,red))






#merge1R <- merge(subset.data, realData2019, by.x = "Date", all = T)


merge1R <- merge(store_norm, realData2019, by.x = "Date", all = T)

merge1R <- merge1R %>% mutate(Timestamp = Date, photo_Snow_norm = normData)


all_data_long2 <- merge1R  %>%
  pivot_longer(cols = c(normData, real_Snow_norm),
               names_to = "Dataset", 
               values_to = "Snow_norm")


########################################################################################################################


p1 <- ggplot(data = all_data_long2) +
  geom_line( 
            aes(x = Date, y = Snow_norm, color = as.factor(Dataset)),
            )+
  geom_point(data = all_data_long2 , 
             aes(x = Date, y = Snow_norm, color = as.factor(Dataset)),
             )+  
  theme_linedraw() +
  theme(
    panel.border = element_rect(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )+
  geom_errorbar(data = all_data_long2, aes(ymin= Snow_norm - normDataSD, 
                                           ymax = Snow_norm + normDataSD, 
                                           x = as.POSIXct(Date) , 
                                           color = as.factor(Dataset)),
                                           width=0.2, alpha = .5)+
#  geom_line(data = all_data_long2 %>% filter (Dataset == "real_Snow_norm"), aes(x = as.POSIXct(Date), y = Snow_norm))+
  scale_x_datetime(limits = c(as.POSIXct("2019-03-15"),as.POSIXct("2019-12-31")))+    
  xlab("Date")+
  ylab("Snow Trend Normalized")



p1





p1 <- ggplot(all_data_long2) +
  geom_line(aes(x = as.POSIXct(Date),
                y = Snow_norm, 
                color= Dataset)) + 
  theme_linedraw() +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-15"),as.POSIXct("2019-12-30")))

p1


####################

ggplot(subset.data)+
  geom_line(aes(x=as.POSIXct(Date),
                y=normData))


ggplot(store_norm)+
  geom_line(aes(x=as.POSIXct(Date),
                y=normData))








############################################################

############################################################
p1 <- ggplot(all_data_long2) +
  geom_line(aes(x = as.POSIXct(Date),
                y = Snow_norm, 
                color= Dataset)) + 
  theme_linedraw() +
  theme(
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_datetime(limits = c(as.POSIXct("2019-04-01"),as.POSIXct("2019-05-01")))
  
p1



p2 <- ggplot(all_data_long2) +
  geom_line(aes(x = as.POSIXct(Date),
                y = Snow_norm, 
                color = Dataset)) + 
  theme_linedraw() +
  theme(
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-15"),as.POSIXct("2019-6-30")))+ 
  labs(title = paste("snowDepth1"))


p2



plot.graphs2(all_data_long2)


#pdf()
#dev.off()


#########################
##Linear Model
########################



linear.model(merge1R, 
             range.min= "2019-04-01",
             range.max= "2019-05-01",
             photo_Snow_norm, 
             real_Snow_norm)


linear.model(merge1R, 
             range.min= "2019-10-01",
             range.max= "2019-12-31",
             photo_Snow_norm, 
             real_Snow_norm)

linear.model(merge1R, 
             range.min= "2019-03-15",
             range.max= "2019-12-31",
             photo_Snow_norm, 
             real_Snow_norm)
#################################

merge1r_clear <- merge1R[complete.cases(all_data_long2),]


mergeData2 <- merge1r_clear %>% filter(Timestamp >= "2019-04-01" & Timestamp <= "2019-05-01")


ggplot(mergeData2, aes(x = photo_Snow_norm,
                    y = real_Snow_norm))+
  geom_smooth(formula = y ~ x, method = "lm", fullrange = T) +
  geom_abline(slope = 1, intercept = 0) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, NA))

print(summary(lm_out <- lm(real_Snow_norm ~ photo_Snow_norm, data = mergeData2)))
