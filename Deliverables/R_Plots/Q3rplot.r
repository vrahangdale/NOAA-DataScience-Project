##Reading the file from working directory 
# Reading the Event Id,Event Type, Month and assigning it as Dataset3_month
Dataset=read.csv('./../../RawDataset/project_data/StormEvents_details-ftp_v1.0_d2016_c20170918.csv')
Dataset3_month <- Dataset[,c(8,12,13)]
# Create Month_Name as the factor.
Dataset3_month$MONTH_NAME=as.factor(Dataset3_month$MONTH_NAME)

#Showing the month with frequency of event occurance-
table_month <- table(Dataset3_month$MONTH_NAME)
View(table_month)
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

# Creating Table for jan data and assigning it to new dataframe-
table_jan <- table(jan_data$EVENT_TYPE)
data_jan<- as.data.frame(table_jan)

table_feb <- table(feb_data$EVENT_TYPE)
data_feb<- as.data.frame(table_feb)

table_mar <- table(mar_data$EVENT_TYPE)
data_mar<- as.data.frame(table_mar)

table_apr <- table(apr_data$EVENT_TYPE)
data_apr<- as.data.frame(table_apr)

table_may <- table(may_data$EVENT_TYPE)
data_may<- as.data.frame(table_may)

table_jun <- table(jun_data$EVENT_TYPE)
data_jun<- as.data.frame(table_jun)

table_jul <- table(jul_data$EVENT_TYPE)
data_jul<- as.data.frame(table_jul)

table_aug <- table(aug_data$EVENT_TYPE)
data_aug<- as.data.frame(table_aug)

table_sep <- table(sep_data$EVENT_TYPE)
data_sep<- as.data.frame(table_sep)

table_oct <- table(oct_data$EVENT_TYPE)
data_oct<- as.data.frame(table_oct)

table_nov <- table(nov_data$EVENT_TYPE)
data_nov<- as.data.frame(table_nov)

table_dec <- table(dec_data$EVENT_TYPE)
data_dec<- as.data.frame(table_dec)


# Creating plot-

p1 <-  ggplot(data_jan, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "January Data Plot",x="Events", y="Number of Events")
p1

p2 <-  ggplot(data_feb, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "Feb Data Plot",x="Events", y="Number of Events")
p2

p3 <-  ggplot(data_mar, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "March Data Plot",x="Events", y="Number of Events")
p3
p4 <-  ggplot(data_apr, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "April Data Plot",x="Events", y="Number of Events")
p4
p5 <-  ggplot(data_may, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "May Data Plot",x="Events", y="Number of Events")
p5
p6 <-  ggplot(data_jun, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "June Data Plot",x="Events", y="Number of Events")
p6

p7 <-  ggplot(data_jul, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "July Data Plot",x="Events", y="Number of Events")
p7

p8 <-  ggplot(data_aug, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "August Data Plot",x="Events", y="Number of Events")
p8

p9 <-  ggplot(data_sep, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "September Data Plot",x="Events", y="Number of Events")
p9

p10 <-  ggplot(data_oct, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "October Data Plot",x="Events", y="Number of Events")
p10

p11 <-  ggplot(data_nov, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "November Data Plot",x="Events", y="Number of Events")
p11

p12 <-  ggplot(data_dec, aes(x=Var1, y = Freq))+geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust =1))+labs(title = "December Data Plot",x="Events", y="Number of Events")
p12
