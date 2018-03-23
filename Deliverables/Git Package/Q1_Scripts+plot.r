# Q.1 Over the year, frequency of occurrence of event type on the basis of location-
# Set working directory to the location where the datafile is saved

# Reading the csv file and making subset
Dataset=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv')
Dataset_new <- Dataset[,c(9,13)]


# Added new column Named region for States-
Dataset_new$Region[Dataset_new$STATE %in% c("CONNECTICUT","MAINE","MASSACHUSETTS", "NEW HAMPSHIRE","RHODE ISLAND", "VERMONT", "NEW JERSEY", "NEW YORK", "PENNSYLVANIA")] <- "Northeast"

Dataset_new$Region[Dataset_new$STATE%in% c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN", "IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA","NORTH DAKOTA", "SOUTH DAKOTA")] <- "Midwest"

Dataset_new$Region[Dataset_new$STATE %in% c("DELAWARE", "FLORIDA", 
                                      "GEORGIA", "MARYLAND", 
                                      "NORTH CAROLINA", "SOUTH CAROLINA", 
                                      "VIRGINIA", "DISTRICT OF COLUMBIA", 
                                      "WEST VIRGINIA", "ALABAMA", "KENTUCKY",
                                      "MISSISSIPPI", "TENNESSEE", "ARKANSAS", 
                                      "LOUISIANA", "OKLAHOMA", "TEXAS"
)] <- "South"

Dataset_new$Region[Dataset_new$STATE %in% c("ARIZONA", "COLORADO", "IDAHO", 
                                      "MONTANA", "NEVADA", "NEW MEXICO", 
                                      "UTAH", "WYOMING", "ALASKA", 
                                      "CALIFORNIA", "HAWAII", "OREGON", 
                                      "WASHINGTON"
)] <- "West"       

south_data <-Dataset_new[Dataset_new$Region == 'South',]
north_data <-Dataset_new[Dataset_new$Region == 'Northeast',]
midwest_data <-Dataset_new[Dataset_new$Region == 'Midwest',]
west_data <-Dataset_new[Dataset_new$Region == 'West',]




# Intensity plotting for events vs state-- each point is scaled according to number of occurence
# South Region-
south_table <- table(south_data)
View(south_table)
table_s<- as.data.frame(south_table)
names(table_s)<- c("State","Event","Region","Frequency")
summary(table_s$Frequency)
p<- subset(table_s,subset=Frequency>50, select= c(State,Event,Frequency))


qplot(p$State, p$Event, data=p, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in south Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))

# North Region-
north_table <- table(north_data)
View(south_table)
table_n<- as.data.frame(north_table)
names(table_n)<- c("State","Event","Region","Frequency")
summary(table_n$Frequency)
p2<- subset(table_n,subset=Frequency>10, select= c(State,Event,Frequency))

qplot(p2$State, p2$Event, data=p2, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in North Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))

# MidWest Region-
midwest_table <- table(midwest_data)
View(midwest_table)
table_mw<- as.data.frame(midwest_table)
names(table_mw)<- c("State","Event","Region","Frequency")
summary(table_mw$Frequency)
p1<- subset(table_mw,subset=Frequency>50, select= c(State,Event,Frequency))

qplot(p1$State, p1$Event, data=p1, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in Mid-West Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))
# West Region-
west_data <- na.omit(west_data)
west_table <- table(west_data)
View(west_table)
table_w<- as.data.frame(west_table)
names(table_w)<- c("State","Event","Region","Frequency")
summary(table_w$Frequency)
p4<- subset(table_w,subset=Frequency>0, select= c(State,Event,Frequency))

qplot(p4$State, p4$Event, data=p4, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in West Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))

# Plotting logitute and latitute on US Map-
# Load the followng Library
library("Rcpp")
library("ggmap")

# Making subset according to events, taking top 5 damage causing events
thunder_plot<- subset(Dataset2,EVENT_TYPE=="Thunderstorm Wind")
thunder_plot<- na.omit(thunder_plot)

FFlood_plot<- subset(Dataset2,EVENT_TYPE=="Flash Flood")
FFlood_plot<- na.omit(FFlood_plot)

Flood_plot<- subset(Dataset2,EVENT_TYPE=="Flood")
Flood_plot<- na.omit(Flood_plot)

hail_plot<- subset(Dataset2,EVENT_TYPE=="Hail")
hail_plot<- na.omit(hail_plot)

hurricane_plot<- subset(Dataset2,EVENT_TYPE=="Tornado")
hurricane_plot<- na.omit(hurricane_plot)




# Plotting-
usmap<-qmap("United States",zoom = 4)

usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=thunder_plot)
plot(thunder_plot$BEGIN_LON,thunder_plot$BEGIN_LAT)+ggtitle("Thunderstorm Plot")


usmap<-qmap("United States",zoom = 4)
usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=FFlood_plot)+ggtitle("Flash Flood Plot")

usmap<-qmap("United States",zoom = 4)
usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=Flood_plot)+ggtitle("Flood Plot")

usmap<-qmap("United States",zoom = 4)
usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=hail_plot)+ggtitle("Hail Plot")

usmap<-qmap("United States",zoom = 4)
usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=hurricane_plot)+ggtitle("Hurricane Plot")
