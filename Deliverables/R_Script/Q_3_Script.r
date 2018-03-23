# Which event is more likely to occur on the basis of month (monthwise occurrence)?
# Install package plyr


# Reading the file from working directory and 
# Reading the Event Id,Event Type, Month and assigning it as Dataset3_month
Dataset=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv')
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

# df_all <- data.frame(jan_data,feb_data,mar_data,apr_data,may_data, jun_data ,jul_data,aug_data,sep_data,oct_data,nov_data,dec_data, check.rows = FALSE)

# Counting the Frequency for Event_Type for each month &
# Storing the table in a variable and Naming the Frequency as Column as Total_Count-

max_jan_event <- count(jan_data, 'EVENT_TYPE')
colnames(max_jan_event) <- c("Event_Type", "Total_count")

max_feb_event <- count(feb_data, 'EVENT_TYPE')
colnames(max_feb_event) <- c("Event_Type", "Total_count")

max_mar_event <- count(mar_data, 'EVENT_TYPE')
colnames(max_mar_event) <- c("Event_Type", "Total_count")

max_apr_event <- count(apr_data, 'EVENT_TYPE')
colnames(max_apr_event) <- c("Event_Type", "Total_count")

max_may_event <- count(may_data, 'EVENT_TYPE')
colnames(max_may_event) <- c("Event_Type", "Total_count")

max_jun_event <- count(jun_data, 'EVENT_TYPE')
colnames(max_jun_event) <- c("Event_Type", "Total_count")

max_jul_event <- count(jul_data, 'EVENT_TYPE')
colnames(max_jul_event) <- c("Event_Type", "Total_count")

max_aug_event <- count(aug_data, 'EVENT_TYPE')
colnames(max_aug_event) <- c("Event_Type", "Total_count")

max_sep_event <- count(sep_data, 'EVENT_TYPE')
colnames(max_sep_event) <- c("Event_Type", "Total_count")

max_oct_event <- count(oct_data, 'EVENT_TYPE')
colnames(max_oct_event) <- c("Event_Type", "Total_count")

max_nov_event <- count(nov_data, 'EVENT_TYPE')
colnames(max_nov_event) <- c("Event_Type", "Total_count")

max_dec_event <- count(dec_data, 'EVENT_TYPE')
colnames(max_dec_event) <- c("Event_Type", "Total_count")


# Printing the Value for maximum Event-


max_jan_event <- max_jan_event[max_jan_event$Total_count == max(max_jan_event$Total_count),]
View(max_jan_event)

max_feb_event <- max_feb_event[max_feb_event$Total_count == max(max_feb_event$Total_count),]
View(max_feb_event)

max_mar_event <- max_mar_event[max_mar_event$Total_count == max(max_mar_event$Total_count),]
View(max_mar_event)

max_apr_event <- max_apr_event[max_apr_event$Total_count == max(max_apr_event$Total_count),]
View(max_apr_event)

max_may_event <- max_may_event[max_may_event$Total_count == max(max_may_event$Total_count),]
View(max_may_event)

max_jun_event <- max_jun_event[max_jun_event$Total_count == max(max_jun_event$Total_count),]
View(max_jun_event)

max_jul_event <- max_jul_event[max_jul_event$Total_count == max(max_jul_event$Total_count),]
View(max_jul_event)

max_aug_event <- max_aug_event[max_aug_event$Total_count == max(max_aug_event$Total_count),]
View(max_aug_event)

max_sep_event <- max_sep_event[max_sep_event$Total_count == max(max_sep_event$Total_count),]
View(max_sep_event)

max_oct_event <- max_oct_event[max_oct_event$Total_count == max(max_oct_event$Total_count),]
View(max_oct_event)

max_nov_event <- max_noc_event[max_nov_event$Total_count == max(max_nov_event$Total_count),]
View(max_nov_event)

max_dec_event <- max_dec_event[max_dec_event$Total_count == max(max_dec_event$Total_count),]
View(max_dec_event)

# Creating Plot of Each month for Event_Type-

plot(table_month)
