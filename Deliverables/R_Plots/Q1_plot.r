#Reading and subsetting the Dataset

Dataset=read.csv('./../../RawDataset/project_data/StormEvents_details-ftp_v1.0_d2016_c20170918.csv', stringsAsFactors = TRUE)
Dataset1<-Dataset[,c(8,9,11,13,45,46,47,48)]
Dataset2<- na.omit(Dataset1)

# Insert a new Column named Region and categorize states into different Regions ("South", "West", "Midwest", "Northeast", "USA")
Dataset2$Region[Dataset2$STATE %in% c("CONNECTICUT","MAINE","MASSACHUSETTS", "NEW HAMPSHIRE","RHODE ISLAND", "VERMONT", "NEW JERSEY", "NEW YORK", "PENNSYLVANIA")] <- "Northeast"

Dataset2$Region[Dataset2$STATE%in% c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN", "IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA","NORTH DAKOTA", "SOUTH DAKOTA")] <- "Midwest"

Dataset2$Region[Dataset2$STATE %in% c("DELAWARE", "FLORIDA", 
                                      "GEORGIA", "MARYLAND", 
                                      "NORTH CAROLINA", "SOUTH CAROLINA", 
                                      "VIRGINIA", "DISTRICT OF COLUMBIA", 
                                      "WEST VIRGINIA", "ALABAMA", "KENTUCKY",
                                      "MISSISSIPPI", "TENNESSEE", "ARKANSAS", 
                                      "LOUISIANA", "OKLAHOMA", "TEXAS"
)] <- "South"

Dataset2$Region[Dataset2$STATE %in% c("ARIZONA", "COLORADO", "IDAHO", 
                                      "MONTANA", "NEVADA", "NEW MEXICO", 
                                      "UTAH", "WYOMING", "ALASKA", 
                                      "CALIFORNIA", "HAWAII", "OREGON", 
                                      "WASHINGTON"
)] <- "West"

# Making Subset of Data According to the Region

south_data <-Dataset2[Dataset2$Region == 'South',]
north_data <-Dataset2[Dataset2$Region == 'Northeast',]
midwest_data <-Dataset2[Dataset2$Region == 'Midwest',]
west_data <-Dataset2[Dataset2$Region == 'West',]


# Omitting values with na values
south_data<- na.omit(south_data)
# Left With 15,778 entries
north_data<- na.omit(north_data)
# Left with 2742 entries
midwest_data<- na.omit(midwest_data)
#Left with 12,128 entries
west_data<- na.omit(west_data)
#Left with 3148 entries

# Creating the plot under ggplot for north region
# loading the library
library("ggplot2", lib.loc="D:/SOFTWARE/R-3.3.2/library")
# North Data Plotting
mapping<- north_data
mapping$EVENT_TYPE<- factor(north_data$EVENT_TYPE)
plot <- ggplot(mapping,aes(STATE, EVENT_TYPE))
plot + geom_point(alpha=1/2, size=5, aes(color=factor(EVENT_TYPE)))# The graph shows the state on horizontal axis and events on vertical axis and the points shows states having events

# south Data Plotting
mapping<- south_data
mapping$EVENT_TYPE<- factor(south_data$EVENT_TYPE)
plot <- ggplot(mapping,aes(STATE, EVENT_TYPE))
plot + geom_point(alpha=1/2, size=5, aes(color=factor(EVENT_TYPE)))

# West Data Plotting
mapping<- west_data
mapping$EVENT_TYPE<- factor(west_data$EVENT_TYPE)
plot <- ggplot(mapping,aes(STATE, EVENT_TYPE))
plot + geom_point(alpha=1/2, size=5, aes(color=factor(EVENT_TYPE)))

# MidWest Data Plotting
mapping<- midwest_data
mapping$EVENT_TYPE<- factor(midwest_data$EVENT_TYPE)
plot <- ggplot(mapping,aes(STATE, EVENT_TYPE))
plot + geom_point(alpha=1/2, size=5, aes(color=factor(EVENT_TYPE)))
