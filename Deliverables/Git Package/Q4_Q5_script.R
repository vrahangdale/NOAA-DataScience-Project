#Read  the dataset of 2016
dataset2016=read.csv('StormEvents_details-ftp_v1.0_d2016_c20170918.csv',stringsAsFactors = F)

#Extract only Event Id,Event Type, Month, damage property, damage crops, Direct & Indirect Injuries, Direct & Indirect Deaths and assigning it as Dataset4_damage
Dataset4_damage <- dataset2016[,c(8,12,13,21,22,23,24,25,26)]

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
Dataset4_na_addcol$TOTAL_DAMAGE <- (Dataset4_na_addcol$DAMAGE_PROPERTY + Dataset4_na_addcol$DAMAGE_CROPS)

Dataset4_na_addcol$TOTAL_INJURIES <- (Dataset4_na_addcol$INJURIES_DIRECT + Dataset4_na_addcol$INJURIES_INDIRECT)

Dataset4_na_addcol$TOTAL_DEATHS <- (Dataset4_na_addcol$DEATHS_DIRECT + Dataset4_na_addcol$DEATHS_INDIRECT)

Dataset4_na_addcol$TOTAL_CASUALTIES <- (Dataset4_na_addcol$TOTAL_INJURIES + Dataset4_na_addcol$TOTAL_DEATHS)

# topten 
Damage <- aggregate(cbind(DAMAGE_PROPERTY, DAMAGE_CROPS,TOTAL_DAMAGE) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum)

Injuries <- aggregate(cbind(INJURIES_DIRECT,INJURIES_INDIRECT,TOTAL_INJURIES) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum )

Deaths <- aggregate(cbind(DEATHS_DIRECT,DEATHS_INDIRECT,TOTAL_DEATHS) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum)

Casualties <- aggregate(cbind(TOTAL_INJURIES,TOTAL_DEATHS,TOTAL_CASUALTIES) ~ EVENT_TYPE, data = Dataset4_na_addcol, FUN = sum)


propertyDamage <- Damage[order(Damage$DAMAGE_PROPERTY, decreasing = T), ]
topTen_propertyDamage <- propertyDamage[1:10, ]
topTen_propertyDamage1 <- data.frame(apply(topTen_propertyDamage, 2, unclass))

cropDamage <- Damage[order(Damage$DAMAGE_CROPS, decreasing = T), ]
topTen_cropDamage <- cropDamage[1:10, ]
topTen_cropDamage1 <- data.frame(apply(topTen_cropDamage, 2, unclass))

totalDamage <- Damage[order(Damage$TOTAL_DAMAGE, decreasing = T), ]
topTen_totalDamage <- totalDamage[1:10, ]
topTen_totalDamage1 <- data.frame(apply(topTen_totalDamage, 2, unclass))

totalInjuries <- Injuries[order(Injuries$TOTAL_INJURIES, decreasing = T), ]
topTen_totalInjuries <- totalInjuries[1:10, ]

totalDeaths <- Deaths[order(Deaths$TOTAL_DEATHS, decreasing = T), ]
topTen_totalDeaths <- totalDeaths[1:10, ]

totalCasualties <- Casualties[order(Casualties$TOTAL_CASUALTIES, decreasing = T), ]
topTen_totalCasualties <- totalCasualties[1:10, ]








# Plots
library(dplyr)
library(ggplot2)
library(scales)
library(reshape)

black.bold.italic.text <- element_text(face = "bold.italic", color = "black")


#plot for damage property
p <-  ggplot(topTen_propertyDamage, aes(x=EVENT_TYPE, y = DAMAGE_PROPERTY))+geom_bar(stat = "identity", width = 0.1, fill="black")+scale_y_continuous(labels = comma)

p+ theme(axis.text.x = element_text(angle = 45, hjust =1),axis.line = element_line(size = 1, colour = "black", linetype = "dashed"),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "black", linetype = "dotted"), panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
         title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Damage of property by events",
       x="Event Types", y="Amount of Damage $ (in K)")


p <-  ggplot(topTen_propertyDamage1, aes(x=EVENT_TYPE, y = DAMAGE_PROPERTY))+geom_bar(stat = "identity", width = 0.1, fill="black")

p+ theme(axis.text.x = element_text(angle = 45, hjust =1),axis.line = element_line(size = 1, colour = "black", linetype = "dashed"),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "black", linetype = "dotted"), panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
         title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Damage of property by events",
       x="Event Types", y="Amount of Damage $ (in K)")

#plot for damage crop

p1 <-  ggplot(topTen_cropDamage, aes(x=EVENT_TYPE, y = DAMAGE_CROPS))+geom_bar(stat = "identity", width = 0.1, fill ="orange")+scale_y_continuous(labels = comma)
p1+ theme(axis.text.x = element_text(angle = 45, hjust =1),axis.line = element_line(size = 1, colour = "black", linetype = "dashed"),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "black", linetype = "dotted"), panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Damage of crops by events",
       x="Event Types", y="Amount of Damage $ (in K)") 


p1 <-  ggplot(topTen_cropDamage1, aes(x=EVENT_TYPE, y = DAMAGE_CROPS))+geom_bar(stat = "identity", width = 0.1, fill ="orange")
p1+ theme(axis.text.x = element_text(angle = 45, hjust =1),axis.line = element_line(size = 1, colour = "black", linetype = "dashed"),panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "black", linetype = "dotted"), panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Damage of crops by events",
       x="Event Types", y="Amount of Damage $ (in K)") 

#plot for Total damage 
p2 <-  ggplot(topTen_totalDamage, aes(x=EVENT_TYPE, y = TOTAL_DAMAGE))+geom_bar(stat = "identity", width = 0.1)+scale_y_continuous(labels = comma)

p2+ theme(axis.text.x = element_text(angle = 45, hjust =1),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Total Damage by events",
       x="Event Types", y="Amount of Damage $ (in K)")

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


#plot for injuries

p4 <- ggplot(topTen_totalInjuries, aes(x=EVENT_TYPE, y = TOTAL_INJURIES))+geom_bar(stat = "identity", width = 0.1, fill = "blue")

p4+ theme(axis.text.x = element_text(angle = 45, hjust =1),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Total injuries by events",
       x="Event Types", y="Total Number of Injuries")

#plot for Deaths


p5 <- ggplot(topTen_totalDeaths, aes(x=EVENT_TYPE, y = TOTAL_DEATHS))+geom_bar(stat = "identity", width = 0.1, fill = "forestgreen")

p5+ theme(axis.text.x = element_text(angle = 45, hjust =1),
          title = black.bold.italic.text, axis.title = black.bold.italic.text)+
  labs(title = "Total deaths by events",
       x="Event Types", y="Total Number of Deaths")

#plot for Total Casualties

p6 <- melt(topTen_totalCasualties[, c("EVENT_TYPE", "TOTAL_CASUALTIES", "TOTAL_INJURIES", "TOTAL_DEATHS")], id.vars = 1)

ggplot(p6, aes(x = reorder(EVENT_TYPE, -value), y = value)) + geom_bar(stat = "identity", aes(fill = variable), position = "dodge")+scale_y_continuous(labels = comma)+ theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_line(size = 1, colour = "black", linetype = "dashed"), 
                                                                                                                                                                              panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "black", linetype = "dotted"),
                                                                                                                                                                              panel.grid.minor = element_line(colour = "black", linetype = "dotted")) + xlab("Event Type") + ylab("Total Number of Damage") + ggtitle("Top ten harmful weather event types in 2016") + 
  scale_fill_manual(name = "Damage Type", values = c("forestgreen", "skyblue", "orange"), 
                    labels = c("Total casualties",  "injuries", " deaths"))


