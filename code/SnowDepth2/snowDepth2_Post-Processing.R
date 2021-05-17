# create a blank slate
#rm(list = ls(all.names = TRUE))

master.folder <- "~/OneDrive - Kennesaw State University/Students/Brandon Crasto"
PictureDirectory <- "~/OneDrive - Kennesaw State University/Research/Chimney/Data/BB-UF/CamPhotos"

master.folder <- "C:/Users/branc/Desktop/New folder (4)/SnowDepthProject"
PictureDirectory <- "C:/Users/branc/Desktop/New folder (3)/snow"

#Set Directories
working.dir          <- paste0(master.folder,"/code/SnowDepth2")      # location of R code files
DataDirectory        <- paste0(master.folder,"/Data/snowDepth2/v10")      # location of output
CalibrationDirectory <- paste0(master.folder,"/Calibration")   # location of calibration images
setwd(working.dir)

###################################################
#Plotting latest attempt
###################################################

bla <- read.csv(paste0(DataDirectory,"/","MetaData_filtered.csv"))
data.raw = bla

data.in <- time.filter(data.raw,
                       min_hours = 0,
                       max_hours = 24)

plot.latest(Pct.min = 0.2,
            B.min = 0.2,
            mean_Pct.min = 0.2)


##########################################################################
#plotting real data with latest attempt
##########################################################################

#read csv for real data
realData <- read.csv(paste0(master.folder,"/Data/","Biomet_Data_For_EddyPro.csv"),stringsAsFactors = FALSE)

#filter for 2019
realData2019 <- realData %>% filter(TIMESTAMP_1 == 2019) %>% 
  mutate(Timestamp = parse_date_time(paste0(TIMESTAMP_1, "-", TIMESTAMP_2), orders = "Y-j")) %>%
  group_by(Timestamp) %>%
  summarise(real_Snow_mean = mean(SnowDepth_1_1_1)) %>%
  mutate(real_Snow_norm = real_Snow_mean/max(real_Snow_mean))

mergeData <- merge(data.v3, realData2019, by.x = "Timestamp", all = T)

all_data_long <- mergeData  %>%
  pivot_longer(cols = c(photo_Snow_norm, real_Snow_norm),
               names_to = "Dataset", 
               values_to = "Snow_norm")



plot.graphs(all_data_long,
            startDate1 = "2019-04-01",
            endDate1   = "2019-05-01",
            
            startDate2 = "2019-10-01",
            endDate2   = "2019-12-31",
            
            startDate3 = "2019-03-27",
            endDate3   = "2019-12-31")




linear.model(mergeData, 
             range.min= "2019-04-01",
             range.max= "2019-05-01",
             photo_Snow_norm, 
             real_Snow_norm)


linear.model(mergeData, 
             range.min= "2019-10-01",
             range.max= "2019-12-31",
             photo_Snow_norm, 
             real_Snow_norm)


linear.model(mergeData, 
             range.min= "2019-03-27",
             range.max= "2019-12-31",
             photo_Snow_norm, 
             real_Snow_norm)




























##########################################################################
#plotting real data with latest attempt
##########################################################################

store <- subset.data

store$Hours <- hour(store$Date)
store$Date <- as.Date(store$Date)
#store$Day <- day(store$Date)
#store$Month <- month(store$Date)


store <- store %>% filter(Hours >= 9 & Hours <= 15)

store <- store %>%  filter(Hours!= lag(Hours,1)) %>%
  group_by(Date)%>%
  summarise(mean_snow = mean(normData)) 

store$Date <- as.POSIXct(store$Date)

mergeData2 <- merge(mergeData,subset.data, by.x = "Timestamp", by.y = "Date", all = T)

#Plot Year
ggplot(mergeData2) +geom_line(aes(x = Timestamp, y = normData2),color = 'red') +geom_line(aes(x=Timestamp, y = normData))
x

#Plot Mar - May
ggplot(mergeData2) +
  geom_line(aes(x = Timestamp, y = normData2),color = 'red') +
  geom_line(aes(x = Timestamp, y = normData) ,color = 'black') + 
  geom_line(aes(x = Timestamp, y = mean_snow),color = 'blue') + 
  scale_x_datetime(limits = c(as.POSIXct("2019-03-15"),as.POSIXct("2019-05-01")))

#Plot Oct - Dec
ggplot(mergeData2) +
  geom_line(aes(x = Timestamp, y = normData2),color = 'red') +
  geom_line(aes(x = Timestamp, y = normData) ,color = 'black') + 
  geom_line(aes(x = Timestamp, y = mean_snow),color = 'blue') + 
  scale_x_datetime(limits = c(as.POSIXct("2019-10-01"),as.POSIXct("2019-12-31")))



ggplot(mergeData2) +
  geom_line(aes(x = Timestamp, y = normData2),color = 'red') +
  geom_line(aes(x = Timestamp, y = normData) ,color = 'black') + 
  geom_line(aes(x = Timestamp, y = mean_snow),color = 'blue') 



ggplot(mergeData2 %>% filter(Timestamp >= "2019-03-15" & Timestamp<= "2019")) +
  
  geom_point(aes(x = normData, y = normData2),color = 'red') + scale_x_continuous(limits = c(0,1))+ scale_y_continuous(limits = c(0,1)) + geom_abline(slope = 1,intercept = 0) 





