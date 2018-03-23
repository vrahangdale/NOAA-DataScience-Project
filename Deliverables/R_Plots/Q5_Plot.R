#Q5. Which event is the most severe taking into consideration both fatalities and damage?

#Read  the dataset of 2016

Dataset1=read.csv('./../../RawDataset/project_data/StormEvents_details-ftp_v1.0_d2016_c20170918.csv',stringsAsFactors = F)

#Extract only Event Id,Month name, Event Type, damage property, Injuries indirect, injuries direct, deaths indirect, deaths direct and assigning it as Dataset5_severity

Dataset5_severity <- Dataset1[,c(8,12,13,25,21,22,24,23)]

# Multiply the values of amount in damage_property containing M in data by 1000 to turn into K and remove M from data
# Remove K from data

Dataset5_updateSeverity <- Dataset5_severity
index1 <- (substr(Dataset5_updateSeverity$DAMAGE_PROPERTY,nchar(Dataset5_updateSeverity$DAMAGE_PROPERTY),nchar(Dataset5_updateSeverity$DAMAGE_PROPERTY)))=="M"

index2 <- (substr(Dataset5_updateSeverity$DAMAGE_PROPERTY,nchar(Dataset5_updateSeverity$DAMAGE_PROPERTY),nchar(Dataset5_updateSeverity$DAMAGE_PROPERTY)))=="K"


Dataset5_updateSeverity$DAMAGE_PROPERTY[index1] <- 
  as.numeric(substr(Dataset5_updateSeverity$DAMAGE_PROPERTY[index1],1,nchar(Dataset5_updateSeverity$DAMAGE_PROPERTY[index1])-1))*1000  

 
Dataset5_updateSeverity$DAMAGE_PROPERTY[index2] <- 
  as.numeric(substr(Dataset5_updateSeverity$DAMAGE_PROPERTY[index2],1,nchar(Dataset5_updateSeverity$DAMAGE_PROPERTY[index2])-1))

#changing fields into numeric
Dataset5_updateSeverity$DAMAGE_PROPERTY <- as.numeric(as.character(Dataset5_updateSeverity$DAMAGE_PROPERTY))

Dataset5_updateSeverity$DAMAGE_PROPERTY[is.na(Dataset5_updateSeverity$DAMAGE_PROPERTY) ] <- 0

# Plots
  library(dplyr)
  library(ggplot2)
  library(scales)
  

blue.bold.italic.text <- element_text(face = "bold.italic", color = "blue")


#plot for damage property
p <-  ggplot(Dataset5_updateSeverity, aes(x=EVENT_TYPE, y = DAMAGE_PROPERTY))+geom_line()
 p+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Damage of property by events",
        x="Event Types", y="Amount of Damage $ (in K)")
		
#plot for INJURIES_INDIRECT

p <-  ggplot(Dataset5_updateSeverity, aes(x=EVENT_TYPE, y = INJURIES_INDIRECT))+geom_line()
 p+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Indirect injuries by events",
        x="Event Types", y="Amount of INJURIES_INDIRECT $ (in K)")		

#plot for INJURIES_DIRECT

p <-  ggplot(Dataset5_updateSeverity, aes(x=EVENT_TYPE, y = INJURIES_DIRECT))+geom_line()
 p+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Direct injuries by events",
        x="Event Types", y="Amount of INJURIES_DIRECT $ (in K)")
		
#plot for DEATHS_DIRECT

p <-  ggplot(Dataset5_updateSeverity, aes(x=EVENT_TYPE, y = DEATHS_DIRECT))+geom_line()
 p+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Direct Deaths by events",
        x="Event Types", y="Amount of DEATHS_DIRECT $ (in K)")	

#plot for DEATHS_INDIRECT

p <-  ggplot(Dataset5_updateSeverity, aes(x=EVENT_TYPE, y = DEATHS_INDIRECT))+geom_line()
 p+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Indirect Deaths by events",
        x="Event Types", y="Amount of DEATHS_INDIRECT $ (in K)")