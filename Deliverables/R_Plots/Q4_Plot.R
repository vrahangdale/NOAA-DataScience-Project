
#Read  the dataset of 2016
Dataset1=read.csv('./../../RawDataset/project_data/StormEvents_details-ftp_v1.0_d2016_c20170918.csv',stringsAsFactors = F)

#Extract only Event Id,Event Type, Month, damage property, damage crops and assigning it as Dataset4_damage
Dataset4_damage <- Dataset1[,c(8,12,13,25,26)]

# Multiply the values of amount in damage_property and damage_crops containing M in data by 1000 to turn into K and remove M from data
# Remove K from data
Dataset4_update <- Dataset4_damage
index1 <- (substr(Dataset4_update$DAMAGE_PROPERTY,nchar(Dataset4_update$DAMAGE_PROPERTY),nchar(Dataset4_update$DAMAGE_PROPERTY)))=="M"
index2 <- (substr(Dataset4_update$DAMAGE_CROPS,nchar(Dataset4_update$DAMAGE_CROPS),nchar(Dataset4_update$DAMAGE_CROPS)))=="M"
index3 <- (substr(Dataset4_update$DAMAGE_PROPERTY,nchar(Dataset4_update$DAMAGE_PROPERTY),nchar(Dataset4_update$DAMAGE_PROPERTY)))=="K"
index4 <- (substr(Dataset4_update$DAMAGE_CROPS,nchar(Dataset4_update$DAMAGE_CROPS),nchar(Dataset4_update$DAMAGE_CROPS)))=="K"


Dataset4_update$DAMAGE_PROPERTY[index1] <- 
  as.numeric(substr(Dataset4_update$DAMAGE_PROPERTY[index1],1,nchar(Dataset4_update$DAMAGE_PROPERTY[index1])-1))*1000  

Dataset4_update$DAMAGE_CROPS[index2] <- 
  as.numeric(substr(Dataset4_update$DAMAGE_CROPS[index2],1,nchar(Dataset4_update$DAMAGE_CROPS[index2])-1))*1000  

Dataset4_update$DAMAGE_PROPERTY[index3] <- 
  as.numeric(substr(Dataset4_update$DAMAGE_PROPERTY[index3],1,nchar(Dataset4_update$DAMAGE_PROPERTY[index3])-1))

Dataset4_update$DAMAGE_CROPS[index4] <- 
  as.numeric(substr(Dataset4_update$DAMAGE_CROPS[index4],1,nchar(Dataset4_update$DAMAGE_CROPS[index4])-1)) 

#changing fields into numeric
Dataset4_update$DAMAGE_PROPERTY <- as.numeric(as.character(Dataset4_update$DAMAGE_PROPERTY))
Dataset4_update$DAMAGE_CROPS <- as.numeric(as.character(Dataset4_update$DAMAGE_CROPS))

#creating dataset removing data having NA in value
Dataset4_update_rm<- na.omit(Dataset4_update)

#creating dataset replacing data having NA with 0
Dataset4_update_na<- Dataset4_update
Dataset4_update_na$DAMAGE_PROPERTY[is.na(Dataset4_update_na$DAMAGE_PROPERTY) ] <- 0

Dataset4_update_na$DAMAGE_CROPS[is.na(Dataset4_update_na$DAMAGE_CROPS) ] <- 0

#Add column which contains the sum of amount of damage property and damage crops
Dataset4_na_addcol <- Dataset4_update_na
Dataset4_na_addcol$DAMAGE_OVERALL <- (DAMAGE_PROPERTY + DAMAGE_CROPS)

# Plots
  library(dplyr)
  library(ggplot2)
  library(scales)
  

blue.bold.italic.text <- element_text(face = "bold.italic", color = "blue")


#plot for damage property
p <-  ggplot(Dataset4_na_addcol, aes(x=EVENT_TYPE, y = DAMAGE_PROPERTY))+geom_line()
 p+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Damage of property by events",
        x="Event Types", y="Amount of Damage $ (in K)")

#plot for damage crop

 c <-  ggplot(Dataset4_na_addcol, aes(x=EVENT_TYPE, y = DAMAGE_CROPS))+geom_line()
 c+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Damage of crops by events",
        x="Event Types", y="Amount of Damage $ (in K)")

 #plot for overall damage 
 o <-  ggplot(Dataset4_na_addcol, aes(x=EVENT_TYPE, y = DAMAGE_OVERALL))+geom_line()
 o+ theme(axis.text.x = element_text(angle = 90, hjust =1),
          title = blue.bold.italic.text, axis.title = blue.bold.italic.text)+
   labs(title = "Damage of crops by events",
        x="Event Types", y="Amount of Damage $ (in K)")
 
 




