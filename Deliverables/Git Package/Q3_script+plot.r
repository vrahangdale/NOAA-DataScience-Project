##Reading the file from working directory 
# Reading the Event Id,Event Type, Month and assigning it as Dataset3_month
Dataset=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv')
Dataset3_month <- Dataset[,c(12,13)]

# Create event table.
event_table=table(Dataset3_month$EVENT_TYPE)
View(event_table)
event_table_df<- as.data.frame(event_table)
names(event_table_df)<- c("Event","Frequency")
ggplot(event_table_df, aes(x = Frequency)) + geom_density() # this plot is to find out which event should be considered for  plotting in clustered graph
# So event above 5000 frequency should be considered while plotting
# Making subset of the graphs with frequency above the median of the EVenet Frequency so that we only consider the higher occuring events
event_table_df<- subset(event_table_df,subset=Frequency>median(event_table_df$Frequency), select= c(Event))


#Showing the month with frequency of event occurance-
month_table <- table(Dataset3_month)
View(month_table)
table_M<- as.data.frame(table_M)
names(table_M)<- c("Month","Event","Frequency")
mean(table_M$Frequency)
summary(table_M)

highfreq_event<- subset(table_M,subset=Frequency>350, select= c(Month,Event,Frequency))

highfreq_event1p<- subset(table_M,subset=Frequency>48, select= c(Month,Event,Frequency))
q1<- c("January", "February","March","April")
q2<- c("May", "June","July","August")
q3<- c("September", "October","November","December")

highfreq_eventq1<- subset(highfreq_event,subset=Month %in% q1, select= c(Month,Event,Frequency))
highfreq_eventq2<- subset(highfreq_event,subset=Month %in% q2, select= c(Month,Event,Frequency))
highfreq_eventq3<- subset(highfreq_event,subset=Month %in% q3, select= c(Month,Event,Frequency))

View(table_M)
summary(Dataset3_month$MONTH_NAME)

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
pjan <-  ggplot(df16.1, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "January Events",x="Event", y="Frequency")
pjan


table_feb <- table(feb_data$EVENT_TYPE)
data_feb<- as.data.frame(table_feb)
names(data_feb)<- c("EventType","Frequency")
summary(data_feb)# Check for Q3 So as to select the event greater than third Quartile
data_feb<- subset(data_feb,subset=Frequency>51, select= c(EventType,Frequency))
pfeb <-  ggplot(data_feb, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "February Events",x="Event", y="Frequency")
pfeb

table_mar <- table(mar_data$EVENT_TYPE)
data_mar<- as.data.frame(table_mar)
names(data_mar)<- c("EventType","Frequency")
summary(data_mar)# Check for Q3 So as to select the event greater than third Quartile
data_mar<- subset(data_mar,subset=Frequency>38, select= c(EventType,Frequency))
pmar <-  ggplot(data_mar, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "March Events",x="Event", y="Frequency")
pmar

table_apr <- table(apr_data$EVENT_TYPE)
data_apr<- as.data.frame(table_apr)
names(data_apr)<- c("EventType","Frequency")
summary(data_apr)# Check for Q3 So as to select the event greater than third Quartile
data_apr<- subset(data_apr,subset=Frequency>54, select= c(EventType,Frequency))
papr <-  ggplot(data_apr, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "April Events",x="Event", y="Frequency")
papr

table_may <- table(may_data$EVENT_TYPE)
data_may<- as.data.frame(table_may)
names(data_may)<- c("EventType","Frequency")
summary(data_may)# Check for Q3 So as to select the event greater than third Quartile
data_may<- subset(data_may,subset=Frequency>18, select= c(EventType,Frequency))
pmay <-  ggplot(data_may, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "May Events",x="Event", y="Frequency")
pmay

table_jun<- table(jun_data$EVENT_TYPE)
data_jun<- as.data.frame(table_jun)
names(data_jun)<- c("EventType","Frequency")
summary(data_jun)# Check for Q3 So as to select the event greater than third Quartile
data_jun<- subset(data_jun,subset=Frequency>32, select= c(EventType,Frequency))
pjun <-  ggplot(data_jun, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "June Events",x="Event", y="Frequency")
pjun


table_jul<- table(jul_data$EVENT_TYPE)
data_jul<- as.data.frame(table_jul)
names(data_jul)<- c("EventType","Frequency")
summary(data_jul)# Check for Q3 So as to select the event greater than third Quartile
data_jul<- subset(data_jul,subset=Frequency>53, select= c(EventType,Frequency))
pjul <-  ggplot(data_jul, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "July Events",x="Event", y="Frequency")
pjul

table_aug<- table(aug_data$EVENT_TYPE)
data_aug<- as.data.frame(table_aug)
names(data_aug)<- c("EventType","Frequency")
summary(data_aug)# Check for Q3 So as to select the event greater than third Quartile
data_aug<- subset(data_aug,subset=Frequency>39, select= c(EventType,Frequency))
paug <-  ggplot(data_aug, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "August Events",x="Event", y="Frequency")
paug

table_sep<- table(sep_data$EVENT_TYPE)
data_sep<- as.data.frame(table_sep)
names(data_sep)<- c("EventType","Frequency")
summary(data_sep)# Check for Q3 So as to select the event greater than third Quartile
data_sep<- subset(data_sep,subset=Frequency>27, select= c(EventType,Frequency))
psep <-  ggplot(data_sep, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "September Events",x="Event", y="Frequency")
psep

table_oct<- table(oct_data$EVENT_TYPE)
data_oct<- as.data.frame(table_oct)
names(data_oct)<- c("EventType","Frequency")
summary(data_oct)# Check for Q3 So as to select the event greater than third Quartile
data_oct<- subset(data_oct,subset=Frequency>49, select= c(EventType,Frequency))
poct <-  ggplot(data_oct, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "October Events",x="Event", y="Frequency")
poct

table_nov<- table(nov_data$EVENT_TYPE)
data_nov<- as.data.frame(table_nov)
names(data_nov)<- c("EventType","Frequency")
summary(data_nov)# Check for Q3 So as to select the event greater than third Quartile
data_nov<- subset(data_nov,subset=Frequency>39, select= c(EventType,Frequency))
pnov <-  ggplot(data_nov, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "November Events",x="Event", y="Frequency")
pnov

table_dec<- table(dec_data$EVENT_TYPE)
data_dec<- as.data.frame(table_dec)
names(data_dec)<- c("EventType","Frequency")
summary(data_dec)# Check for Q3 So as to select the event greater than third Quartile
data_dec<- subset(data_dec,subset=Frequency>71, select= c(EventType,Frequency))
pdec <-  ggplot(data_dec, aes(x=EventType, y = Frequency))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "December Events",x="Event", y="Frequency")
pdec



## NEW PLOTTING


ggplot(Dataset3_month, aes(x = MONTH_NAME)) + geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Number of Events Per Month",x="Month", y="Event Count")
# THIS SHOWS WHICH MONTH HAS HOW MANY EVENTS OCCURING
ggplot(Dataset3_month, aes(x = MONTH_NAME, fill = EVENT_TYPE)) + geom_bar()+labs(title = "Clustered Graph Showing Month and all Events",x="Month", y="Event Count")

ggplot(highfreq_event, aes(x = Month, fill = Event)) + geom_bar()+labs(title = "Clustered Graph Showing Month and high occuringEvents",x="Month", y="Event Count")
  #THIS GRAPH SHOWS THE MONTH WISE DISTRIBUTION 

ggplot(highfreq_event1p, aes(x = Event, fill =Month ))+ geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Cluster of Months per Event",x="Events", y="Count")



## Graphing for higher occuring events-
sub_event<- event_table_df[["Event"]]

#making subset using event_table_df
highevents<- subset(Dataset3_month, EVENT_TYPE %in% sub_event )
# Clustered Graphs
ggplot(highevents, aes(x = MONTH_NAME, fill = EVENT_TYPE)) + geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Cluster of Events per Month",x="Month", y="Event Count")

 
ggplot(highevents, aes(x = EVENT_TYPE, fill = MONTH_NAME)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Cluster of months per event type",x="Events", y="Month")

# Line Graph Showing the frequency of Events by month (Showing Seasonality of Event Types)
ggplot(data=highfreq_event, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Seasonality of Events",x="Month", y="Frequency")


# for quater 1-
ggplot(data=highfreq_eventq1, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 1",x="Month", y="Frequency")

# Plot for  quater 2-
ggplot(data=highfreq_eventq2, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 2",x="Month", y="Frequency")

# Plot for Quater 3-
ggplot(data=highfreq_eventq3, aes(x=Month, y=Frequency, group=Event)) +
  geom_line(aes(color=Event))+
  geom_point(aes(color=Event))+theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Events in Quater 3",x="Month", y="Frequency")

