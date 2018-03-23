Q4. What is estimated amount of damage (damage property & damage crops) caused by each event? which event is most likely to cause maximum damage?


#Read  the dataset of 2016---------------------------------
Dataset1=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv')

#Extract only Event Id,Event Type, Month, damage property, damage crops and assigning it as Dataset4_damage---------

Dataset4_damage <- Dataset1[,c(8,12,13,25,26)]


# Sql script to multiply the values of amount in damage_property and damage_crops containing M in data by 1000 to turn into K and remove M from data
sql1 <- "UPDATE Dataset4_damage 
SET  DAMAGE_PROPERTY=SUBSTR (DAMAGE_PROPERTY,1,length(DAMAGE_PROPERTY)-1)* 1000,
DAMAGE_CROPS = SUBSTR (DAMAGE_CROPS,1,length(DAMAGE_CROPS)-1)* 1000
WHERE DAMAGE_PROPERTY LIKE '%M'"

sql2 <- "SELECT * FROM main.Dataset4_damage"

# Sql script to remove K from data
sql3<- "UPDATE DT SET DAMAGE_PROPERTY = SUBSTR (DAMAGE_PROPERTY,1,length(DAMAGE_PROPERTY)-1),
DAMAGE_CROPS = SUBSTR (DAMAGE_CROPS,1,length(DAMAGE_CROPS)-1) 
WHERE DAMAGE_PROPERTY LIKE '%K'"

sql4 <- "SELECT * FROM main.DT"

# dataset where damage_property, damage_crops have data in M turned to K by above sql script
DT <- sqldf(c(sql1,sql2))

# dataset where damage_property, damage_crops have data in M turned to K by above sql script
DT1 <- sqldf(c(sql3,sql4))


#validating changes-------------------
sqldf("SELECT count(*) FROM DT WHERE DAMAGE_PROPERTY LIKE '%K'")  --43902
sqldf("SELECT count(*) FROM Dataset4_damage WHERE DAMAGE_PROPERTY LIKE '%K'")--43902
sqldf("SELECT count(*) FROM DT WHERE DAMAGE_PROPERTY NOT LIKE '%K'") -- 12507

sqldf("SELECT count(*) FROM Dataset4_damage WHERE DAMAGE_PROPERTY NOT LIKE '%K'") --12507
sqldf("SELECT * FROM Dataset4_damage WHERE DAMAGE_PROPERTY LIKE '%M'") --362  
sqldf("SELECT * FROM DT WHERE EVENT_ID ='657127'")

# changing fields into numeric
DT1$DAMAGE_PROPERTY <- as.numeric(as.character(DT1$DAMAGE_PROPERTY))
DT1$DAMAGE_CROPS <- as.numeric(as.character(DT1$DAMAGE_CROPS))

# replacing NA with 0
DT2 <- DT1
DT2[is.na(DT2)] <- 0

#descriptive statistic

tapply(DT1$DAMAGE_PROPERTY,DT1$DAMAGE_CROPS,mean,na.rm=TRUE)


mean(DT1$DAMAGE_PROPERTY,na.rm=TRUE) -- 242.8481
mean(DT2$DAMAGE_PROPERTY,na.rm=TRUE) --192.0947

mean(DT1$DAMAGE_CROPS,na.rm=TRUE) -- 106.8749
mean(DT2$DAMAGE_CROPS,na.rm=TRUE) --82.40175







