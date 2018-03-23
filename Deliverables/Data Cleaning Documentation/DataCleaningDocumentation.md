### 1.  1–2 paragraph text description of the data source/s (how much, where from, what it contains,etc.)  with a properly formatted citation for each data source.  This should include how manyrows & columns (&/or tables) and sample column headers.

### **Storm Event Dataset**
#### About dataset

The National Centers for Environmental Information (NCEI) regularly receives Storm Data from the National Weather Service (NWS). This data set consists of state wise occurrence of Event type in USA like Tornado, Thunderstorm Wind and Hail, Marine strong wind, Flash flood, Heavy rain, Heavy snow, Funnel Cloud, Extensive Heat and also contains data regarding locations, fatalities, injuries, damage, narratives and any other event specific information which can be used for information and analysis by business sectors, insurance companies, hazard mitigation, policy makers etc. (NCDC, 2017)

#### Data for our analysis

We are going to use the data collected over a period of 6 consecutive years i.e. from 2011-2016 and refine it through data cleaning using R for our analysis.

The Storm Event dataset is having 51 attributes overall, few of which are State_FIPS, Storm magnitude with storm events type in particular location, and this data can be used in personal, educational, and academic purposes.

#### The file format is CSV (Comma-Separated Values) text files which represent a dump or export of the Storm Events Database

#### CSV file info
All the csv files have 51 columns (attributes)
* StormEvents_details-ftp_v1.0_d2011_c20170519.csv file has 79092 rows
* StormEvents_details-ftp_v1.0_d2012_c20170519.csv file has 64504 rows
* StormEvents_details-ftp_v1.0_d2012_c20170519.csv file has 59986 rows
* StormEvents_details-ftp_v1.0_d2012_c20170519.csv file has 11975977040 rows
* StormEvents_details-ftp_v1.0_d2012_c20170519.csv file has 11644627304 rows
* StormEvents_details-ftp_v1.0_d2012_c20170519.csv file has 11281684247 rows

#### Some of the sample headers.

   > EVENT_ID	STATE	STATE_FIPS	YEAR	MONTH_NAME	EVENT_TYPE	CZ_TYPE	CZ_FIPS	CZ_NAME	WFO	BEGIN_DATE_TIME	CZ_TIMEZONE	END_DATE_TIME	INJURIES_DIRECT  	INJURIES_INDIRECT	DEATHS_DIRECT	DEATHS_INDIRECT	DAMAGE_PROPERTY	DAMAGE_CROPS	SOURCE	          MAGNITUDE

   678791 NEW JERSEY	34	2017	April	Thunderstorm Wind	C	15	GLOUCESTER	PHI	4/6/2017      15:09	EST-5	4/6/2017      15:09	           0	              0	                       0	  0			                              Trained Spotter	          52
   679188	ALABAMA	    1	2017	April	 Hail	            C	89	MADISON	HUN	    4/5/2017     15:55	CST-6	4/5/2017      15:55	           0	              0	                       0	  0			                              Broadcast Media	           1    
####  For more information about attributes click [here](https://www.ncdc.noaa.gov/stormevents/ftp.jsp).

 ### 2. Specifically identify any intellectual policy constraints, or lack thereof (licensing).
The information on NOAA web page is in the public domain unless specifically annotated otherwise (copyright may be held elsewhere) and may therefore be used freely by the public.  
Additional information disclaimer and copyright notice can be found in [Disclaimer and Copyright Notice](https://www.nodc.noaa.gov/about/disclaimer.html) .  

 ### 3. 1 paragraph description of the metadata: what information is available to help you interpret and understand the data?
Storm events database contains the records with information on

* Occurrence of Storms and other significant events which is likely to cause more damage, death and injuries.
* Other rare, unusual weather phenomenon like snow flurries
* Other significant meterological events in connection with another event

The data is entered by NOAA's National Weather Service(NWS). Overall 48 event types are recorded as metnioned in [NWS Directive 10-1605](https://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf).

51 Columns/datafields are there in data which can be defined as shown in [Storm Data Export Format](http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx)

 ### 4. Identify any issues you have encountered with the data: missing values, unstandardized content, entity matching, etc.

* Columns having similar properties are there such as BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME and BEGIN_DATE_TIME. Similar case is found for END DATE as well.
* When looking into 2016 data, 11690 out of 55959  have blank values in DAMAGE_PROPERTY though it also have values 0.00 K in other rows. 
* 11355 out of 55959 have blank values in DAMAGE_CROPS in 2016 data. Like in DAMAGE_PROPERTY, this field also contains value 0.00K.
* Negligible row i.e. 45 out of 55959 data in 2016 have source UNKNOWN.
* Category has values 1,2,4,5 but as per data export format this field has unknown property. So it seems it can't be included in any kind of analysis.


#### Reference
National Centers for Environmental Information.(2017) Strom Event database 2017, from https://www.ncdc.noaa.gov/stormevents/


 ### 5. 1 paragraph description of your rationale for the steps you’re taking to remediate data. For example, if you need to fill in empty fields, specify what value you chose and why.
* We are using different set of columns for each of the research question so that our analysis is oriented towards a particular audience and our result can be implemented easily . For our intitial analysis we are removing 'none' value because R automatically excludes all cases in which any of the inputs are missing; this can limit the amount of information available in the analysis. We are dropping the columns which have redundant data such as 'BEGIN_YEARMONTH', 'BEGIN_YEARMONTH', 'BEGIN_DATE_TIME', 'END_DATE_TIME', 'state_fips' as these values are combination of columns present in the data set and for the analysis we can keep the data granular so that we can represent the result in more granular way. We are also removing the columns 'EPISODE_NARRATIVE', 'event_narrative' and 'DATA_SOURCE' as these are nominal data and it will hard to analyize them in R, but we will be using them seperately as remarks. We have checked for the duplicate values and removed the duplicate values for each question.
 ### 6. A script or step-by-step textual description (or a combination) that documents your data cleaning process with enough detail for replication.
 * Scripts-
 ### 1. Over the year, frequency of occurrence of event type on the basis of location
##### Reading the file from raw dataset and assigning it to a variable Dataset.  
> Dataset=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv')
##### Reading the columns State, Year, Event Id,Event Type, BEGIN_LAT, BEGIN_LON, END_LAT, END_LON and assigning it as Dataset1
> Dataset1<-Dataset[,c(8,9,11,13,45,46,47,48)]
##### Get a data frame that has only the cases with no missing values for any variable and assigning it to Dataset2
> Dataset2<- na.omit(Dataset1)

#####  Checking duplicates 
nrow(Dataset2)  --36422
dup2<-unique(Dataset2)
nrow(dup2) --36422
### 2. Over the five years specific event type occurrence, let say- Thunderstorm.

##### Read the data set from 2011 to 2015
dataset2015 <- read.csv('StormEvents_details-ftp_v1.0_d2015_c20170918.csv',header = TRUE,na.strings = c("",NULL,'NA'))
dataset2014 <- read.csv('StormEvents_details-ftp_v1.0_d2014_c20170718.csv',header = TRUE,na.strings = c("",NULL,'NA'))
dataset2013 <- read.csv('StormEvents_details-ftp_v1.0_d2013_c20170519.csv',header = TRUE,na.strings = c("",NULL,'NA'))
dataset2012 <- read.csv('StormEvents_details-ftp_v1.0_d2012_c20170519.csv',header = TRUE,na.strings = c("",NULL,'NA'))
dataset2011 <- read.csv('StormEvents_details-ftp_v1.0_d2011_c20170519.csv',header = TRUE,na.strings = c("",NULL,'NA'))

##### Reading columns Event_id, State,Year, Month name, Event type from datasets 2011 to 2016
 dataset2_thunder <- Dataset[,c(8,9,11,12,13)]
 dataset2015_thunder <- dataset2015[,c(8,9,11,12,13)]
 dataset2014_thunder <- dataset2014[,c(8,9,11,12,13)]
 dataset2013_thunder <- dataset2013[,c(8,9,11,12,13)]
 dataset2012_thunder <- dataset2012[,c(8,9,11,12,13)]
 dataset2011_thunder <- dataset2011[,c(8,9,11,12,13)]

##### Extracting rows for Event type Thunderstorm Wind
dataset2_thunder1 <- subset(dataset2_thunder,EVENT_TYPE=="Thunderstorm Wind")
dataset2015_thunder1 <- subset(dataset2015_thunder,EVENT_TYPE=="Thunderstorm Wind")
dataset2014_thunder1 <- subset(dataset2014_thunder,EVENT_TYPE=="Thunderstorm Wind")
dataset2013_thunder1 <- subset(dataset2013_thunder,EVENT_TYPE=="Thunderstorm Wind")
dataset2012_thunder1 <- subset(dataset2012_thunder,EVENT_TYPE=="Thunderstorm Wind")
dataset2011_thunder1 <- subset(dataset2011_thunder,EVENT_TYPE=="Thunderstorm Wind")

##### Combining all data from 2011 to 2016
dataall_thunder <- rbind(dataset2_thunder1,dataset2015_thunder1,dataset2014_thunder1,dataset2013_thunder1,dataset2012_thunder1,dataset2011_thunder1)

##### Counting rows
nrow(dataset2_thunder1) --15657
nrow(dataset2015_thunder1)  --14400
nrow(dataset2014_thunder1)  --13830
nrow(dataset2013_thunder1)  --14355
nrow(dataset2012_thunder1)  --16235
nrow(dataset2011_thunder1)  --21282
nrow(dataall_thunder) --95759

##### checking and removing duplicates if any
datauniquall_thunder <-unique(dataall_thunder)
nrow(datauniquall_thunder) --95759

### 3. Which event is more likely to occur on the basis of month (monthwise occurrence)?

##### Reading the  Event Id,Event Type, Month and assigning it as Dataset3_month
Dataset3_month <- Dataset[,c(8,12,13)]

#####  Checking duplicates 
nrow(Dataset3_month) --55959
dup3<- unique(Dataset3_month)
nrow(dup3)  --55959

### 4. What is estimated amount of damage (damage property & damage crops) caused by each event? which event is most likely to cause maximum damage?

##### Reading the  Event Id,Event Type, Month, damage property, damage crops and assigning it as Dataset4_damage
Dataset4_damage <- Dataset[,c(8,12,13,25,26)]

##### Replacing null with 0.00K
Dataset4_damage[is.na(Dataset4_damage)] <- "0.00K"

#####  Checking duplicates 
nrow(Dataset4_damage) --55959
dup4<- unique(Dataset4_damage)
nrow(dup4)  --55959

### 5. Which event is the most severe taking into consideration both fatalities and damage?


##### Reading the  Event Id,Event Type, Month, damage property, injuries direct,injuries indirect, deaths indirect, deaths direct and assigning it as Dataset4_damage
Dataset5_severity <- Dataset[,c(8,12,13,25,21,22,24,23)]

##### Replacing null with 0.00K
Dataset5_severity[is.na(Dataset5_severity)] <- "0.00K"

#####  Checking duplicates 
nrow(Dataset5_severity) --55959
dup5<- unique(Dataset5_severity)
nrow(dup5)  --55959

#### Contributors-
* Chandra
* Shravya
* Pradeep
* Vaibhav
