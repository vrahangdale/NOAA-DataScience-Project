# Reading The data set and selecting the desired column.

Dataset=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv', stringsAsFactors = TRUE)
Dataset1<-Dataset[,c(8,9,11,13,45,46,47,48)]
Dataset2<- na.omit(Dataset1)

# Finding which is the state with maximum numbber of events
y<- Dataset2$STATE # Converting state as factor
table(y)
max(table(y)) #3082

z<- table(y)# converting y table as data frame
df_z<- as.data.frame(z)
View(df_z)# shows state-wise occurance of events
# Printing the State which has maximum number of Events = Texas
max_state <- df_z$y[df_z$Freq == max(table(y))]
max_state

#Currently the data is divided into categories and worked on for single category-
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



# Showing the values of event in each region-
x<- Dataset2$Region
table(x)

# South Region has the maximum number of events

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


# Finding Region wise which state is having maximum event-

y<- north_data$STATE
table(y)
max(table(y))
z<- table(y)# converting y table as data frame
df_z<- as.data.frame(z)
View(df_z)# shows state-wise occurance of events
max_state <- df_z$y[df_z$Freq == max(table(y))]
max_state #NewYork

y<- south_data$STATE
table(y)
max(table(y))
z<- table(y)
df_z<- as.data.frame(z)
max_state <- df_z$y[df_z$Freq == max(table(y))]
max_state# Texas


y<- west_data$STATE
table(y)
max(table(y))
z<- table(y)
df_z<- as.data.frame(z)
max_state <- df_z$y[df_z$Freq == max(table(y))]
max_state# Colorado

y<- midwest_data$STATE
table(y)
max(table(y))
z<- table(y)
df_z<- as.data.frame(z)
max_state <- df_z$y[df_z$Freq == max(table(y))]
max_state#Kansas


# Showing which event is maximum in  each region with their count-
y<- north_data$EVENT_TYPE
table(y)
max(table(y))# Thunderstorm winds

y<- south_data$EVENT_TYPE
table(y)
max(table(y))#Thunderstorm winds

y<- midwest_data$EVENT_TYPE
table(y)
max(table(y)) #Thunderstorm winds

y<- west_data$EVENT_TYPE
table(y)
max(table(y))# Hail
