
library(dplyr)

Contacts <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Contacts.csv", stringsAsFactors=FALSE)

Contacts <-Contacts %>% arrange(desc(Contacts)) 

Contacts <- Contacts[!duplicated(Contacts$CM_FILENO),]

write.csv(Contacts,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Contacts.csv",row.names=F)

