---
title: "Impact and Analysis of Weather Events"
author: "CSV Pitchers"
date: "December 5, 2017"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,fig.width = 8 )
```

## Storm Event Database

### About Dataset
The National Centers for Environmental Information (NCEI) regularly receives Storm Data from the National Weather Service (NWS). This data set consists of state wise occurrence of Event type in USA like Tornado, Thunderstorm Wind and Hail, Marine strong wind, Flash flood, Heavy rain, Heavy snow, Funnel Cloud, Extensive Heat and also contains data regarding locations, fatalities, injuries, damage, narratives and any other event specific information which can be used for information and analysis by business sectors, insurance companies, hazard mitigation, policy makers etc. (NCDC, 2017)

### Data for Analysis
We are using the data of 2016 to analyze the effect of event types on economy (property damage and crop damage) and casualties (injuries and deaths). Furthermore, We have used data of 2011-2016 to see the trends of top events we have found in 2016.Overall there are 51 attributes/columns in each data file.

### License
The information on NOAA web page is in the public domain unless specifically annotated otherwise (copyright may be held elsewhere) and may therefore be used freely by the public.  
Additional information disclaimer and copyright notice can be found in [Disclaimer and Copyright Notice](https://www.nodc.noaa.gov/about/disclaimer.html) .

### Metadata : Information available to understand and interprete data
Storm events database contains the records with information on

* Occurrence of Storms and other significant events which is likely to cause more damage, death and injuries.
* Other rare, unusual weather phenomenon like snow flurries

The data is entered by NOAA's National Weather Service(NWS). Overall 48 event types are recorded as metnioned in [NWS Directive 10-1605](https://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf).

51 Columns/datafields are there in data which can be defined as shown in [Storm Data Export Format](http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx)

### Issues Encountered with data
* Columns having similar properties are there such as BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME and BEGIN_DATE_TIME. Similar case is found for END DATE as well.
* When looking into 2016 data, 11690 out of 55959  have blank values in DAMAGE_PROPERTY though it also have values 0.00 K in other rows. 
* 11355 out of 55959 have blank values in DAMAGE_CROPS in 2016 data. Like in DAMAGE_PROPERTY, this field also contains value 0.00K.
* Negligible row i.e. 45 out of 55959 data in 2016 have source UNKNOWN.

### Remediation steps

* We are using different set of columns for each of the research question so that our analysis is oriented towards a particular audience and our result can be implemented easily . For our intitial analysis we are removing 'none' value because R automatically excludes all cases in which any of the inputs are missing; this can limit the amount of information available in the analysis.

* We are dropping the columns which have redundant data such as 'BEGIN_YEARMONTH', 'END_YEARMONTH',  'state_fips' as these values are combination of columns present in the data set and for the analysis we can keep the data granular so that we can represent the result in more granular way.

* We are also removing the columns 'EPISODE_NARRATIVE', 'event_narrative' and 'DATA_SOURCE' as these are nominal data and it will hard to analyize them in R, but we will be using them seperately as remarks. We have checked for the duplicate values and removed the duplicate values for each question.

### Data Cleaning & Analysis Steps


####Loading the data
We have used the necessary functions and loaded the data from 2011-2016 into the respective dataframe as:
```{r , include = TRUE, message = FALSE, warning = FALSE}
dataset2016 <- read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv',header = TRUE,stringsAsFactors = F, na.strings = c("",NULL,'NA'))
dataset2015 <- read.csv('StormEvents_details-ftp_v1.0_d2015_c20170918.csv',header = TRUE,stringsAsFactors = F,na.strings = c("",NULL,'NA'))
dataset2014 <- read.csv('StormEvents_details-ftp_v1.0_d2014_c20170718.csv',header = TRUE,stringsAsFactors = F,na.strings = c("",NULL,'NA'))
dataset2013 <- read.csv('StormEvents_details-ftp_v1.0_d2013_c20170519.csv',header = TRUE,stringsAsFactors = F,na.strings = c("",NULL,'NA'))
dataset2012 <- read.csv('StormEvents_details-ftp_v1.0_d2012_c20170519.csv',header = TRUE,stringsAsFactors = F,na.strings = c("",NULL,'NA'))
dataset2011 <- read.csv('StormEvents_details-ftp_v1.0_d2011_c20170519.csv',header = TRUE,stringsAsFactors = F,na.strings = c("",NULL,'NA'))

```
These data are then combined into one dataframe as :

```{r , include = TRUE, message = FALSE, warning = FALSE}
dataAll <- rbind(dataset2016,dataset2015,dataset2014,dataset2013,dataset2012,dataset2011)

```
Internal structure of data has been checked
```{r , include = TRUE, message = FALSE, warning = FALSE}
str(dataAll)

```


Duplicates have been checked

```{r , include = TRUE, message = FALSE, warning = FALSE}
dupAll<- unique(dataAll)
nrow(dataAll)
nrow(dupAll)
```

NA is replaced with 0
```{r , include = TRUE, message = FALSE, warning = FALSE}
dataAll_na <- dataAll
dataAll[is.na(dataAll)] <- 0
```

Values of amount in damage property and damage crops containing M in data is multiplied by 1000 to turn it into K and "K" and "M are then removed from data 
```{r , include = TRUE, message = FALSE, warning = FALSE}
dataall_update <- dataAll

index1 <- (substr(dataall_update$DAMAGE_PROPERTY,nchar(dataall_update$DAMAGE_PROPERTY),nchar(dataall_update$DAMAGE_PROPERTY)))=="M"
index2 <- (substr(dataall_update$DAMAGE_CROPS,nchar(dataall_update$DAMAGE_CROPS),nchar(dataall_update$DAMAGE_CROPS)))=="M"
index3 <- (substr(dataall_update$DAMAGE_PROPERTY,nchar(dataall_update$DAMAGE_PROPERTY),nchar(dataall_update$DAMAGE_PROPERTY)))=="K"
index4 <- (substr(dataall_update$DAMAGE_CROPS,nchar(dataall_update$DAMAGE_CROPS),nchar(dataall_update$DAMAGE_CROPS)))=="K"


dataall_update$DAMAGE_PROPERTY[index1] <- 
  as.numeric(substr(dataall_update$DAMAGE_PROPERTY[index1],1,nchar(dataall_update$DAMAGE_PROPERTY[index1])-1))*1000  

dataall_update$DAMAGE_CROPS[index2] <- 
  as.numeric(substr(dataall_update$DAMAGE_CROPS[index2],1,nchar(dataall_update$DAMAGE_CROPS[index2])-1))*1000  

dataall_update$DAMAGE_PROPERTY[index3] <- 
  as.numeric(substr(dataall_update$DAMAGE_PROPERTY[index3],1,nchar(dataall_update$DAMAGE_PROPERTY[index3])-1))

dataall_update$DAMAGE_CROPS[index4] <- 
  as.numeric(substr(dataall_update$DAMAGE_CROPS[index4],1,nchar(dataall_update$DAMAGE_CROPS[index4])-1))


```
Fields Damage property and Damage crops have to be changed into numeric datatypto allow any further analysis
```{r , include = TRUE, message = FALSE, warning = FALSE}
dataall_update$DAMAGE_PROPERTY <- as.numeric(as.character(dataall_update$DAMAGE_PROPERTY))
dataall_update$DAMAGE_CROPS <- as.numeric(as.character(dataall_update$DAMAGE_CROPS))

```
Replacing NA with 0
```{r , include = TRUE, message = FALSE, warning = FALSE}
dataall_update_na <- dataall_update
dataall_update[is.na(dataall_update)] <- 0
```

Relevant Statistics are being checked, in this case,summary is taken.

We have to mainly focus on damage property, damage crops, direct injuries, indirect injuries, direct death and indirect deaths.
```{r , include = TRUE, message = FALSE, warning = FALSE}
summary(dataall_update)
```

Now we will start working on data and transforming it accordingly to answer the research questions for analysis. 
#### 1. Over the year 2016, what is frequency of occurrence of event type on the basis of location?

For this analysis 2016 data has been used to determine the number of even types that occurred on the basis of location. In the data state, longitudes and latitudes are available for the information on location.

```{r , include = TRUE, message = FALSE, warning = FALSE}
library(knitr)
library(ggplot2)
# making subset to extract state and event type
Dataset_new <- dataset2016[,c(9,13)]

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
south_table <- table(midwest_data)

table_s<- as.data.frame(south_table)
names(table_s)<- c("State","Event","Region","Frequency")
summary(table_s$Frequency)
p<- subset(table_s,subset=Frequency>50, select= c(State,Event,Frequency))

qplot(p$State, p$Event, data=p, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in Mid-West Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))

```
Here We can see that number or frequency of occurrence of event types are represented by the size of the circle as shown in Frequency. This graph gives the information on number of events occurring in a state. We can see that Thunderstorm and Hail are occurring in every state. Comparison is possible of any particular event types between state.
Similar graphs are drawn for other regions as well.
```{r , include = TRUE, message = FALSE, warning = FALSE}
# North Region-
north_table <- table(north_data)

table_n<- as.data.frame(north_table)
names(table_n)<- c("State","Event","Region","Frequency")
summary(table_n$Frequency)
p2<- subset(table_n,subset=Frequency>10, select= c(State,Event,Frequency))

qplot(p2$State, p2$Event, data=p2, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in North Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))

# MidWest Region-

midwest_table <- table(midwest_data)

table_mw<- as.data.frame(midwest_table)
names(table_mw)<- c("State","Event","Region","Frequency")
summary(table_mw$Frequency)
p1<- subset(table_mw,subset=Frequency>50, select= c(State,Event,Frequency))

qplot(p1$State, p1$Event, data=p1, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in Mid-West Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 45, hjust =1))
# West Region-
west_data <- na.omit(west_data)
west_table <- table(west_data)
table_w<- as.data.frame(west_table)
names(table_w)<- c("State","Event","Region","Frequency")
summary(table_w$Frequency)
p4<- subset(table_w,subset=Frequency>0, select= c(State,Event,Frequency))

qplot(p4$State, p4$Event, data=p4, size=Frequency,col= Event, alpha=I(6/10), main="Intensity of Events in West Region", 
      xlab="State", ylab="Events Type")+theme(axis.text.x = element_text(angle = 90, hjust =1))
```

Below are the plots in US map showing the similar distribution as shown above. Here top events are taken :Falsh flood, flood, hail, tornado and thunderstorm. Though Hurricane(Typhoon), Tropical Storm, Storm surge/Tide, wildfire are also topmost damaging events, longitudes and latitudes are missing in data. Thus we haven't plotted these events in US map.

```{r , include = TRUE, message = FALSE, warning = FALSE}
library("Rcpp")
library("ggmap")
library("qmap")

Dataset1<-dataset2016[,c(8,9,11,13,45,46,47,48)]
Dataset2<- na.omit(Dataset1)

# Making subset according to events, taking top 5 damage causing events
thunder_plot<- subset(Dataset2,EVENT_TYPE=="Thunderstorm Wind")
thunder_plot<- na.omit(thunder_plot)

FFlood_plot<- subset(Dataset2,EVENT_TYPE=="Flash Flood")
FFlood_plot<- na.omit(FFlood_plot)

Flood_plot<- subset(Dataset2,EVENT_TYPE=="Flood")
Flood_plot<- na.omit(Flood_plot)

hail_plot<- subset(Dataset2,EVENT_TYPE=="Hail")
hail_plot<- na.omit(hail_plot)

tornado_plot<- subset(Dataset2,EVENT_TYPE=="Tornado")
tornado_plot<- na.omit(tornado_plot)


```

Here we can see which region is more affected by these events clearly. Thunderstorm is more prevalent and distributed over large area. 

```{r , include = TRUE, message = FALSE, warning = FALSE}

usmap<-qmap("United States",zoom = 4)
usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=FFlood_plot)+ggtitle("Thunderstorm Plot")

usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=FFlood_plot)+ggtitle("Flash Flood Plot")

usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=Flood_plot)+ggtitle("Flood Plot")

usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=hail_plot)+ggtitle("Hail Plot")

usmap+geom_point(aes(x=BEGIN_LON,y=BEGIN_LAT),data=tornado_plot)+ggtitle("Tornado Plot")


```

#### 2. What is the amunt of damage (property & Crops) caused by each event for year 2016?
For this analysis, we have used the column event types, property damage and crop damage. To focus more on the events which are causing the most damages, we have taken top 10 events instead of considering all the events available in data.

```{r , include = TRUE, message = FALSE, warning = FALSE}
library(dplyr)
library(ggplot2)
library(scales)
library(reshape)

#extract 2016 data 
data_2016 = subset (dataall_update, YEAR == 2016)
Dataset4_na_addcol <- data_2016

# adding column TOTAL_DAMAGE
Dataset4_na_addcol$TOTAL_DAMAGE <- (Dataset4_na_addcol$DAMAGE_PROPERTY + Dataset4_na_addcol$DAMAGE_CROPS)

## topten events
Damage <- aggregate(cbind(DAMAGE_PROPERTY, DAMAGE_CROPS,TOTAL_DAMAGE) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum)

#topten events by property damage

propertyDamage <- Damage[order(Damage$DAMAGE_PROPERTY, decreasing = T), ]
topTen_propertyDamage <- propertyDamage[1:10, ]
topTen_propertyDamage1 <- data.frame(apply(topTen_propertyDamage, 2, unclass))

#topten events by crop damage
cropDamage <- Damage[order(Damage$DAMAGE_CROPS, decreasing = T), ]
topTen_cropDamage <- cropDamage[1:10, ]
topTen_cropDamage1 <- data.frame(apply(topTen_cropDamage, 2, unclass))


#topten events by total(property+ crop) damage
totalDamage <- Damage[order(Damage$TOTAL_DAMAGE, decreasing = T), ]
topTen_totalDamage <- totalDamage[1:10, ]
topTen_totalDamage1 <- data.frame(apply(topTen_totalDamage, 2, unclass))
```


Plots for above data have been created as follows:
```{r , include = TRUE, message = FALSE, warning = FALSE}
black.bold.italic.text <- element_text(face = "bold.italic", color = "black")
#plot for damage property

p <-  ggplot(topTen_propertyDamage1, aes(x=EVENT_TYPE, y = DAMAGE_PROPERTY))+geom_bar(stat = "identity", width = 0.1, fill="black")

p+ theme(axis.text.x = element_text(angle = 45, hjust =1),axis.line = element_line(size = 1, colour = "black", linetype = "dashed"),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "black", linetype = "dotted"), panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
         title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Damage of property by events",
       x="Event Types", y="Amount of Damage $ (in K)")
```


Here FlashFlood is the most devastating for property in 2016

```{r , include = TRUE, message = FALSE, warning = FALSE}

#plot for damage crop

p1 <-  ggplot(topTen_cropDamage1, aes(x=EVENT_TYPE, y = DAMAGE_CROPS))+geom_bar(stat = "identity", width = 0.1, fill ="orange")
p1+ theme(axis.text.x = element_text(angle = 45, hjust =1),axis.line = element_line(size = 1, colour = "black", linetype = "dashed"),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "black", linetype = "dotted"), panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Damage of crops by events",
       x="Event Types", y="Amount of Damage $ (in K)") 
```


Flood is the most devastating for crops

Summarized plot of total damage is shown below
```{r , include = TRUE, message = FALSE, warning = FALSE}

#plot for Total damage 

p2 <-  ggplot(topTen_totalDamage1, aes(x=EVENT_TYPE, y = TOTAL_DAMAGE))+geom_bar(stat = "identity", width = 0.1)

p2+ theme(axis.text.x = element_text(angle = 45, hjust =1),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Total Damage by events",
       x="Event Types", y="Amount of Damage $ (in K)")

#plot for overall damage with respective damage in property and crops
p3 <- melt(topTen_totalDamage[, c("EVENT_TYPE", "TOTAL_DAMAGE", "DAMAGE_PROPERTY", "DAMAGE_CROPS")], id.vars = 1)

ggplot(p3, aes(x = reorder(EVENT_TYPE, -value), y = value)) + geom_bar(stat = "identity", aes(fill = variable), position = "dodge")+scale_y_continuous(labels = comma)+ theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_line(size = 1, colour = "black", linetype = "dashed"), 
                                                                                                                                                                              panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "black", linetype = "dotted"),
                                                                                                                                                                              panel.grid.minor = element_line(colour = "black", linetype = "dotted")) + xlab("Event Type") + ylab("Total amount $ (in K)") + ggtitle("Top ten harmful weather event types in 2016") + 
  scale_fill_manual(name = "Damage Type", values = c("forestgreen", "skyblue", "orange"), 
                    labels = c("Total Damage",  "Property Damage", "Crop Damage"))

```

From the above graph we can see that event types which are most devastating has more effects on property than crop when compared on the basis of amount.

#### 3. What is trend of top 5 events occurred in 2016 troughout the duration of 2011 -2016

We have taken top 5 events on the basis of total damage on property and crop. We are trying to look into the trend of these top eventsfrom 2011 - 2016. We have top 5 events : FlashFlood, Flood, Hail, Hurricane, Tropical Storm. Along with these, thunderstorm is also taken since it is one of the most happening uniform event in 2016.


```{r , include = TRUE, message = FALSE, warning = FALSE}

#Add column which contains the sum of amount of damage property and damage crops
dataall_na_addcol <- dataall_update[,c(8,9,11,12,13,18,25,26)]
dataall_na_addcol$DAMAGE_OVERALL <- (dataall_na_addcol$DAMAGE_PROPERTY + dataall_na_addcol$DAMAGE_CROPS)


# plotting

dataall_na_addcol$date <- as.POSIXct(dataall_na_addcol$BEGIN_DATE_TIME,format = "%d-%b-%y %H:%M:%S")

# making subset of the damage as thuderstorm is the most occuring event each year-
dataset_thunder <- subset(dataall_na_addcol,EVENT_TYPE=="Thunderstorm Wind")

dataset_thunder <- dataset_thunder[c(9,10)]
dataset_thunder$date <- as.Date(dataset_thunder$date)

# for relative scaling, removed data with damage 0 and greater than 50000
dataset_thunder1 <- subset(dataset_thunder,subset = DAMAGE_OVERALL>0 & DAMAGE_OVERALL<50000 , select = c(DAMAGE_OVERALL,date))

# plot time series
ts_thunder<- ts(dataset_thunder1$DAMAGE_OVERALL,start=c(2011,1),end=c(2016,12),frequency = 12)

plot(ts_thunder, type="l",col="blue", xlab = "Date",ylab="Relative Damage", main = "Time series of Thunderstorm")
```

We can see that thunderstorm is almost occurring in every year through out the months. Here 0 is relative rather than the absolute value.

Similar trend plots are done for top 5 events as below

```{r , include = TRUE, message = FALSE, warning = FALSE}
# Creating plot for top 5 damage causing events- 1. Flash Flood
dataset_flashflood <- subset(dataall_na_addcol,EVENT_TYPE=="Flash Flood")
dataset_flashflood <- dataset_flashflood[c(9,10)]
dataset_flashflood$date <- as.Date(dataset_flashflood$date)
dataset_flashflood1 <- subset(dataset_flashflood,subset = DAMAGE_OVERALL>0  , select = c(DAMAGE_OVERALL,date))

ts_flashflood<- ts(dataset_flashflood1$DAMAGE_OVERALL,start=c(2011,1),end=c(2016,12),frequency = 12)

plot(ts_flashflood, type="l",col="blue", xlab = "Date",ylab="Relative Damage", main = "Time series of Flash Flood")

# 2. Flood
dataset_flood <- subset(dataall_na_addcol,EVENT_TYPE=="Flood")
dataset_flood <- dataset_flood[c(9,10)]
dataset_flood$date <- as.Date(dataset_flood$date)
dataset_flood1 <- subset(dataset_flood,subset = DAMAGE_OVERALL>0  , select = c(DAMAGE_OVERALL,date))

ts_flood<- ts(dataset_flood1$DAMAGE_OVERALL,start=c(2011,1),end=c(2016,12),frequency = 12)

plot(ts_flood, type="l",col="blue", xlab = "Date",ylab="Relative Damage", main = "Time series of Flood")


# 3. Hail
dataset_hail <- subset(dataall_na_addcol,EVENT_TYPE=="Hail")
dataset_hail <- dataset_hail[c(9,10)]
dataset_hail$date <- as.Date(dataset_hail$date)
dataset_hail1 <- subset(dataset_hail,subset = DAMAGE_OVERALL>0  , select = c(DAMAGE_OVERALL,date))


ts_hail<- ts(dataset_hail1$DAMAGE_OVERALL,start=c(2011,1),end=c(2016,12),frequency = 12)

plot(ts_hail, type="l",col="blue", xlab = "Date",ylab="Relative Damage", main = "Time series of Hail")


# 4. Hurricane

dataset_hurricane <- subset(dataall_na_addcol,EVENT_TYPE=="Hurricane")
dataset_hurricane <- dataset_hurricane[c(9,10)]
dataset_hail$date <- as.Date(dataset_hail$date)
dataset_hurricane1 <- subset(dataset_hurricane,subset = DAMAGE_OVERALL>0  , select = c(DAMAGE_OVERALL,date))



ts_hurricane<- ts(dataset_hurricane1$DAMAGE_OVERALL,start=c(2011,1),end=c(2016,12),frequency = 12)

plot(ts_hurricane, type="l",col="blue", xlab = "Date",ylab="Relative Damage", main = "Time series of Hurricane")


# 5. Tropical Storm

dataset_TropicalStorm <- subset(dataall_na_addcol,EVENT_TYPE=="Tropical Storm")
dataset_TropicalStorm <- dataset_TropicalStorm[c(9,10)]
dataset_TropicalStorm$date <- as.Date(dataset_TropicalStorm$date)
dataset_TropicalStorm1 <- subset(dataset_TropicalStorm,subset = DAMAGE_OVERALL>0  , select = c(DAMAGE_OVERALL,date))



ts_TropicalStorm<- ts(dataset_TropicalStorm1$DAMAGE_OVERALL,start=c(2011,1),end=c(2016,12),frequency = 12)

plot(ts_TropicalStorm, type="l", col="blue", xlab = "Date",ylab="Relative Damage", main = "Time series of Tropical storm")


```


#### 4.What is the frequency of event occurrence on the monthly basis (month wise occurrence) for year 2016?

```{r , include = TRUE, message = FALSE, warning = FALSE}
library(knitr)
library(ggplot2)
require(gridExtra)
Dataset=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv')
Dataset3_month <- Dataset[,c(12,13)]
# Create event table.
event_table=table(Dataset3_month$EVENT_TYPE)
event_table_df<- as.data.frame(event_table)
names(event_table_df)<- c("Event","Frequency")
ggplot(event_table_df, aes(x = Frequency)) + geom_density() +scale_y_continuous(labels = comma) # this plot is to find out which event should be considered for  plotting in clustered graph
# So event above 5000 frequency should be considered while plotting
# Making subset of the graphs with frequency above the median of the EVenet Frequency so that we only consider the higher occuring events
event_table_df<- subset(event_table_df,subset=Frequency>median(event_table_df$Frequency), select= c(Event))

#Showing the month with frequency of event occurance-
month_table <- table(Dataset3_month)
table_M<- as.data.frame(month_table)
names(table_M)<- c("Month","Event","Frequency")
mean(table_M$Frequency)
summary(table_M)

highfreq_event<- subset(table_M,subset=Frequency>350, select= c(Month,Event,Frequency))
highfreq_event1p<- subset(table_M,subset=Frequency>48, select= c(Month,Event,Frequency))

q1<- c("January", "February","March")
q2<- c("April", "May","June")
q3<- c("July", "August","September")
q4<- c("October","November", "December")


highfreq_eventq1<- subset(highfreq_event,subset=Month %in% q1, select= c(Month,Event,Frequency))
highfreq_eventq2<- subset(highfreq_event,subset=Month %in% q2, select= c(Month,Event,Frequency))
highfreq_eventq3<- subset(highfreq_event,subset=Month %in% q3, select= c(Month,Event,Frequency))
highfreq_eventq4<- subset(highfreq_event,subset=Month %in% q4, select= c(Month,Event,Frequency))

#Creating Subset of data for each month-

jan_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'January',]
feb_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'February',]
mar_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'March',]
apr_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'April',]
may_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'May',]
jun_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'June',]
jul_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'July',]
aug_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'August',]
sep_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'September',]
oct_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'October',]
nov_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'November',]
dec_data <-Dataset3_month[Dataset3_month$MONTH_NAME == 'December',]

# Creating Plot For each month-

table_16 <- table(jan_data$EVENT_TYPE)
df16<- as.data.frame(table_16)
names(df16)<- c("EventType","Frequency")
summary(df16)# Check for Q3 So as to select the event greater than third Quartile
df16.1<- subset(df16,subset=Frequency>50, select= c(EventType,Frequency))
pjan <-  ggplot(df16.1, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "January Events",x="Event", y="Frequency")

table_feb <- table(feb_data$EVENT_TYPE)
data_feb<- as.data.frame(table_feb)
names(data_feb)<- c("EventType","Frequency")
summary(data_feb)# Check for Q3 So as to select the event greater than third Quartile
data_feb<- subset(data_feb,subset=Frequency>51, select= c(EventType,Frequency))
pfeb <-  ggplot(data_feb, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "February Events",x="Event", y="Frequency")

table_mar <- table(mar_data$EVENT_TYPE)
data_mar<- as.data.frame(table_mar)
names(data_mar)<- c("EventType","Frequency")
summary(data_mar)# Check for Q3 So as to select the event greater than third Quartile
data_mar<- subset(data_mar,subset=Frequency>38, select= c(EventType,Frequency))
pmar <-  ggplot(data_mar, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "March Events",x="Event", y="Frequency")

table_apr <- table(apr_data$EVENT_TYPE)
data_apr<- as.data.frame(table_apr)
names(data_apr)<- c("EventType","Frequency")
summary(data_apr)# Check for Q3 So as to select the event greater than third Quartile
data_apr<- subset(data_apr,subset=Frequency>54, select= c(EventType,Frequency))
papr <-  ggplot(data_apr, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "April Events",x="Event", y="Frequency")

grid.arrange(pjan, pfeb, ncol = 2)
grid.arrange(pmar, papr, ncol = 2)
```

In above graph we can see the frequency of events and which event is occurring most in the particular months.
Plots are created for other months till december.

```{r , include = TRUE, message = FALSE, warning = FALSE}
table_may <- table(may_data$EVENT_TYPE)
data_may<- as.data.frame(table_may)
names(data_may)<- c("EventType","Frequency")
summary(data_may)# Check for Q3 So as to select the event greater than third Quartile
data_may<- subset(data_may,subset=Frequency>18, select= c(EventType,Frequency))
pmay <-  ggplot(data_may, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "May Events",x="Event", y="Frequency")


table_jun<- table(jun_data$EVENT_TYPE)
data_jun<- as.data.frame(table_jun)
names(data_jun)<- c("EventType","Frequency")
summary(data_jun)# Check for Q3 So as to select the event greater than third Quartile
data_jun<- subset(data_jun,subset=Frequency>32, select= c(EventType,Frequency))
pjun <-  ggplot(data_jun, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "June Events",x="Event", y="Frequency")

grid.arrange(pmay, pjun, ncol = 2)

table_jul<- table(jul_data$EVENT_TYPE)
data_jul<- as.data.frame(table_jul)
names(data_jul)<- c("EventType","Frequency")
summary(data_jul)# Check for Q3 So as to select the event greater than third Quartile
data_jul<- subset(data_jul,subset=Frequency>53, select= c(EventType,Frequency))
pjul <-  ggplot(data_jul, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "July Events",x="Event", y="Frequency")



table_aug<- table(aug_data$EVENT_TYPE)
data_aug<- as.data.frame(table_aug)
names(data_aug)<- c("EventType","Frequency")
summary(data_aug)# Check for Q3 So as to select the event greater than third Quartile
data_aug<- subset(data_aug,subset=Frequency>39, select= c(EventType,Frequency))
paug <-  ggplot(data_aug, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "August Events",x="Event", y="Frequency")

grid.arrange(pjul, paug, ncol = 2)

table_sep<- table(sep_data$EVENT_TYPE)
data_sep<- as.data.frame(table_sep)
names(data_sep)<- c("EventType","Frequency")
summary(data_sep)# Check for Q3 So as to select the event greater than third Quartile
data_sep<- subset(data_sep,subset=Frequency>27, select= c(EventType,Frequency))
psep <-  ggplot(data_sep, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "September Events",x="Event", y="Frequency")


table_oct<- table(oct_data$EVENT_TYPE)
data_oct<- as.data.frame(table_oct)
names(data_oct)<- c("EventType","Frequency")
summary(data_oct)# Check for Q3 So as to select the event greater than third Quartile
data_oct<- subset(data_oct,subset=Frequency>49, select= c(EventType,Frequency))
poct <-  ggplot(data_oct, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "October Events",x="Event", y="Frequency")

grid.arrange(psep, poct, ncol = 2)

table_nov<- table(nov_data$EVENT_TYPE)
data_nov<- as.data.frame(table_nov)
names(data_nov)<- c("EventType","Frequency")
summary(data_nov)# Check for Q3 So as to select the event greater than third Quartile
data_nov<- subset(data_nov,subset=Frequency>39, select= c(EventType,Frequency))
pnov <-  ggplot(data_nov, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "November Events",x="Event", y="Frequency")


table_dec<- table(dec_data$EVENT_TYPE)
data_dec<- as.data.frame(table_dec)
names(data_dec)<- c("EventType","Frequency")
summary(data_dec)# Check for Q3 So as to select the event greater than third Quartile
data_dec<- subset(data_dec,subset=Frequency>71, select= c(EventType,Frequency))
pdec <-  ggplot(data_dec, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 45, hjust =1))+labs(title = "December Events",x="Event", y="Frequency")

grid.arrange(pnov, pdec, ncol = 2)

```

Following graph shows the occurrence of total events in each month. We can see that July has the highest number of events occurring followed by June and May. This graph helps in understanding of event counts against month of 2016.
```{r , include = TRUE, message = FALSE, warning = FALSE}
ggplot(Dataset3_month, aes(x = MONTH_NAME)) + geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Number of Events Per Month",x="Month", y="Event Count")
```

Following graph shows the nnumber of events in each month with the distribution/frequency of all events. Here, in July (month having the most events) has thunderstorm and hail as predominant events. Throughout the months, these two events are mostly occurring.
```{r , include = TRUE, message = FALSE, warning = FALSE}
# THIS SHOWS WHICH MONTH HAS HOW MANY EVENTS OCCURING

ggplot(Dataset3_month, aes(x = MONTH_NAME, fill = EVENT_TYPE)) + geom_bar()+
   theme(axis.text.x = element_text(angle = 90, hjust =1))+
  labs(title = "Clustered Graph Showing Month and all Events",x="Month", y="Event Count")

```

Following graph is same as the above one, only difference is only the high occurring event types are taken on the basis of the analysis above. Here higher frequency greater than 350 is taken. Thus in october, not any event has occured more than 350 times. Thus its not seen in graph.


```{r , include = TRUE, message = FALSE, warning = FALSE}

ggplot(highfreq_event, aes(x = Month, fill = Event)) + geom_bar()+
   theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Clustered Graph Showing Month and high occuringEvents",x="Month", y="Event Count")

```

In the graph below distribution of months is shown per event. We can see clearly thunderstorm is occurring  in each month, followed by hail,flood.

```{r , include = TRUE, message = FALSE, warning = FALSE}

ggplot(highfreq_event, aes(x = Month, fill = Event)) + geom_bar()+
   theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Clustered Graph Showing Month and high occuringEvents",x="Month", y="Event Count")

```


Following graphs are refined version of graphs made earlier. Events with higher frequency of occurrence are only shown, thus less number of events can be seen thatn in previous graphs, and will be easy to analyze.
```{r , include = TRUE, message = FALSE, warning = FALSE}


## Graphing for higher occuring events-
sub_event<- event_table_df[["Event"]]

#making subset using event_table_df
highevents<- subset(Dataset3_month, EVENT_TYPE %in% sub_event )
# Clustered Graphs
ggplot(highevents, aes(x = MONTH_NAME, fill = EVENT_TYPE)) + geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Cluster of Events per Month",x="Month", y="Event Count")


ggplot(highevents, aes(x = EVENT_TYPE, fill = MONTH_NAME)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Cluster of months per event type",x="Events", y="Month")
```

Following 4 graphs are shown to validate the analysis and graphs above. Here if we focus n thunderstorm and hail, they are occurring in each month while other events are discrete. Here higher frequency greater than 350 is taken. Thus in october, not any event has occured more than 350 times. Thus its not seen in graph.

```{r , include = TRUE, message = FALSE, warning = FALSE}

# Line Graph Showing the frequency of Events by month (Showing Seasonality of Event Types)
ggplot(data=highfreq_event, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Seasonality of Events",x="Month", y="Frequency")


# for quater 1-
ggplotq1<-ggplot(data=highfreq_eventq1, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 1",x="Month", y="Frequency")

# Plot for  quater 2-
ggplotq2 <- ggplot(data=highfreq_eventq2, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 2",x="Month", y="Frequency")


grid.arrange(ggplotq1, ggplotq2, ncol = 2)
# Plot for Quater 3-
ggplotq3 <- ggplot(data=highfreq_eventq3, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 3",x="Month", y="Frequency")

# Plot for Quater 4-
ggplotq4 <- ggplot(data=highfreq_eventq4, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 4",x="Month", y="Frequency")

grid.arrange(ggplotq3, ggplotq4, ncol = 2)

```

#### 5.What is the total number of casualties (injuries and deaths ) in 2016 by each events?
Here we have considered top 10 events on the basis of injuries and deaths and eventaully casualties.
Injuries is considered as sum of direct and indirect injuries in data and deaths is considered combining direct and indirect death columns of data. Casualties is calculated adding up both injuries and casualties.

```{r , include = TRUE, message = FALSE, warning = FALSE}

Dataset4_na_addcol$TOTAL_INJURIES <- (Dataset4_na_addcol$INJURIES_DIRECT + Dataset4_na_addcol$INJURIES_INDIRECT)

Dataset4_na_addcol$TOTAL_DEATHS <- (Dataset4_na_addcol$DEATHS_DIRECT + Dataset4_na_addcol$DEATHS_INDIRECT)

Dataset4_na_addcol$TOTAL_CASUALTIES <- (Dataset4_na_addcol$TOTAL_INJURIES + Dataset4_na_addcol$TOTAL_DEATHS)

# topten events for each case
Injuries <- aggregate(cbind(INJURIES_DIRECT,INJURIES_INDIRECT,TOTAL_INJURIES) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum )

Deaths <- aggregate(cbind(DEATHS_DIRECT,DEATHS_INDIRECT,TOTAL_DEATHS) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum)

Casualties <- aggregate(cbind(TOTAL_INJURIES,TOTAL_DEATHS,TOTAL_CASUALTIES) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum)

totalInjuries <- Injuries[order(Injuries$TOTAL_INJURIES, decreasing = T), ]
topTen_totalInjuries <- totalInjuries[1:10, ]

totalDeaths <- Deaths[order(Deaths$TOTAL_DEATHS, decreasing = T), ]
topTen_totalDeaths <- totalDeaths[1:10, ]

totalCasualties <- Casualties[order(Casualties$TOTAL_CASUALTIES, decreasing = T), ]
topTen_totalCasualties <- totalCasualties[1:10, ]

# Plots
#plot for injuries

p4 <- ggplot(topTen_totalInjuries, aes(x=EVENT_TYPE, y = TOTAL_INJURIES))+geom_bar(stat = "identity", width = 0.1, fill = "blue")

p4+ theme(axis.text.x = element_text(angle = 45, hjust =1),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Total injuries by events",
       x="Event Types", y="Total Number of Injuries")

```

In above graph, Torando is the most devastating event causing injuries in 2016

```{r , include = TRUE, message = FALSE, warning = FALSE}
#plot for Deaths


p5 <- ggplot(topTen_totalDeaths, aes(x=EVENT_TYPE, y = TOTAL_DEATHS))+geom_bar(stat = "identity", width = 0.1, fill = "forestgreen")

p5+ theme(axis.text.x = element_text(angle = 45, hjust =1),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Total deaths by events",
       x="Event Types", y="Total Number of Deaths")
```

In the above graph, Flash flood and flood are causing most deaths in 2016

```{r , include = TRUE, message = FALSE, warning = FALSE}
#plot for Total Casualties

p6 <- melt(topTen_totalCasualties[, c("EVENT_TYPE", "TOTAL_CASUALTIES", "TOTAL_INJURIES", "TOTAL_DEATHS")], id.vars = 1)

ggplot(p6, aes(x = reorder(EVENT_TYPE, -value), y = value)) + geom_bar(stat = "identity", aes(fill = variable), position = "dodge")+scale_y_continuous(labels = comma)+ theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_line(size = 1, colour = "black", linetype = "dashed"), 
                                                                                                                                                                              panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "black", linetype = "dotted"),
                                                                                                                                                                              panel.grid.minor = element_line(colour = "black", linetype = "dotted")) + xlab("Event Type") + ylab("Total Number of Damage") + ggtitle("Top ten harmful weather event types in 2016") + 
  scale_fill_manual(name = "Damage Type", values = c("forestgreen", "skyblue", "orange"), 
                    labels = c("Total casualties",  "injuries", " deaths"))


```

Above graph shows the total casualties considering deaths and injuries above. We can see that some event like tornado has more  injuries than deaths while events like flash flood, rip current and flood are causing more deaths than injuries.

So the takeaway from these analyses are:

* In 2016, Tornado is the most devastating event on the basis of casualties - however injuries way higher than deaths for tornado.

* In 2016, Flash Flood is the most devastating event on the basis of property damage.

* In 2016, Flood is the major cause for crop damage.

* Looking at the trend of topmost events of 2016, thunderstorm and hail are occurring almost each month.

* Looking at the trend of topmost events of 2016 for duration 2011-2016, thunderstorm, hurricane are more uniform than other events.
