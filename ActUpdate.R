library(dplyr)
library(data.table)

NINE <- fread("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/NINE.csv", stringsAsFactors=FALSE)
NINE <- data.frame(NINE)
NINE$ED_ACCT_ACT_DATE <- as.Date(NINE$ED_ACCT_ACT_DATE,"%m/%d/%Y")
NINE$Month <- NINE$ED_ACCT_ACT_DATE
NINE$Month <- format(NINE$Month,"%B %Y")
NINE$Month2 <- as.Date(NINE$ED_NOTICE_TO_BORR_DT,"%m/%d/%Y")
NINE$Month2 <- format(NINE$Month2,"%B %Y")

NINE <- NINE %>%
  mutate(Previous_Two = ifelse(ED_ACCT_ACT_DATE >= (Sys.Date()-90),"Yes","No"),
         Month2 = ifelse(is.na(Month2) & Previous_Two == "Yes","No WG19 Sent",Month ))


NINE$ED_COND_VER_DT <- as.Date(NINE$ED_COND_VER_DT, "%m/%d/%Y")
NINE$RHBMONTH <- format(NINE$ED_COND_VER_DT, "%B %Y")
NINE$RHBMONTH[is.na(NINE$RHBMONTH)] <- ""
NINE$ED_ORDER_TO_EMP_DT <- as.Date(NINE$ED_ORDER_TO_EMP_DT, "%m/%d/%Y")
NINE$ED_NOTICE_TO_BORR_DT <- as.Date(NINE$ED_NOTICE_TO_BORR_DT, "%m/%d/%Y")
NINE$ED_ORDER_TO_EMP_DT2 <- as.Date(NINE$ED_ORDER_TO_EMP_DT2, "%m/%d/%Y")


NINE <- NINE %>%
  mutate(RHBMONTH = ifelse(ED_CUR_COND == "REHAB3" | ED_CUR_COND =="REHAB2" | ED_CUR_COND == "REHAB",RHBMONTH,""),
         WG19_SENT = ifelse(ED_NOTICE_TO_BORR_DT >= ED_ACCT_ACT_DATE, "Yes", "No"), 
         OW1_SENT = ifelse(ED_ORDER_TO_EMP_DT >= ED_ACCT_ACT_DATE, "Yes", "No"), 
         OW2_SENT = ifelse(ED_ORDER_TO_EMP_DT2 >= ED_ACCT_ACT_DATE, "Yes", "No")) 


NINE$WG19_SENT[is.na(NINE$WG19_SENT)] <- "No"
NINE$OW1_SENT[is.na(NINE$OW1_SENT)] <- "No"
NINE$OW2_SENT[is.na(NINE$OW2_SENT)] <- "No"

NINE$ED_REJD_DATE <- as.Date(NINE$ED_REJD_DATE,"%m/%d/%Y")

RGR.Database <- read.csv("//KNX3IT/AWG Management/RGR/RGR Database.csv", stringsAsFactors=FALSE)








FILE <- select(NINE,CM_FILENO,ED_ACCT_ACT_DATE) %>%
  rename(Debtor = CM_FILENO,DATE = ED_ACCT_ACT_DATE) 
FILE$Date <- format(FILE$DATE,"%m/%d/%Y")


TRACKERFILE <- read.delim("//KNX3IT/AWG Management/Lowhorn/MTD Database/TRACKERFILE.txt", stringsAsFactors=FALSE)
FILE$Debtor <- as.character(FILE$Debtor)
TRACKERFILE$Debtor <- as.character(TRACKERFILE$Debtor)
files <- FILE$Debtor
TRACKERFILE <- TRACKERFILE[TRACKERFILE$Debtor %in% files,]
TRACKERFILE <- left_join(TRACKERFILE,FILE,by="Debtor")
rm(FILE)
rm(files)
TRACKERFILE <- TRACKERFILE[TRACKERFILE$FundsType %in% c("RGVO","RGWG","SB"),]
TRACKERFILE$EffDt <- as.Date(TRACKERFILE$EffDt,"%Y-%m-%d")
TRACKERFILE$DATE <- as.Date(TRACKERFILE$DATE,"%m/%d/%Y")

TRACKERFILE <- TRACKERFILE %>%
  mutate(Logical = ifelse(EffDt >= DATE,1,0)) %>%
  filter(Logical==1)

TRACKERFILE <- TRACKERFILE %>%
  select(Debtor,EffDt,TransAmt,FundsType)

TRACKERFILE <- TRACKERFILE %>%
  group_by(Debtor) %>%
  summarize(GAR_Dollars = sum(TransAmt[FundsType=="RGWG"]),
            Total_Voluntary = sum(TransAmt[FundsType=="RGVO"])-sum(TransAmt[FundsType=="SB"]),
            Total_Direct = GAR_Dollars + Total_Voluntary)

NINE$CM_FILENO <- as.character(NINE$CM_FILENO)

NINE <- NINE %>% rename(Debtor=CM_FILENO)

NINE <- left_join(NINE,TRACKERFILE,by="Debtor")
rm(TRACKERFILE)

NINE$GAR_Dollars[is.na(NINE$GAR_Dollars)] <- 0
NINE$Total_Voluntary[is.na(NINE$Total_Voluntary)] <- 0
NINE$Total_Direct[is.na(NINE$Total_Direct)] <- 0

NINE <- NINE %>%
  mutate(ED_AWG_CREDIT_AR = ifelse(ED_AWG_CREDIT_AR <= 830,811,ED_AWG_CREDIT_AR))

NSABAN <- NINE%>%
  group_by(ED_AWG_CREDIT_AR, Month2)%>%
  summarize(MonthlyActivations = n()   
  ) 
NSABAN2 <- NINE%>%
  group_by(ED_AWG_CREDIT_AR, Month)%>%
  summarize(InitiatedActivations = n()   
  ) 
NSABAN <- rename(NSABAN,Month=Month2)

NSABAN <- full_join(NSABAN,NSABAN2,by=c("Month","ED_AWG_CREDIT_AR"))

NSABAN[is.na(NSABAN)] <- 0

NSABAN$InitiatedActivations[is.na(NSABAN$InitiatedActivations)] <- 0

rm(NSABAN2)  


DTRUMP <- NINE %>%
  group_by(ED_AWG_CREDIT_AR)%>%
  summarize(TotalActivations=n(),
            WG19_Sent = sum(WG19_SENT == "Yes"),
            OW1_Sent = sum(OW1_SENT == "Yes"),
            OW2_Sent = sum(OW2_SENT == "Yes"), 
            TotalGAR = sum(GAR_Dollars),
            TotalVol = sum(Total_Voluntary),
            TotalCollected = sum (Total_Direct),
            Resolutions = sum(ED_CUR_COND == "REHAB3" | ED_CUR_COND == "REHAB2" | ED_CUR_COND == "REHAB" | ED_CUR_COND == "PPA" | ED_CUR_COND =="COMP" | ED_CUR_COND == "BIF"), 
            SuccessRate = Resolutions / TotalActivations   
  )

NSABAN <- left_join(NSABAN, DTRUMP,by="ED_AWG_CREDIT_AR")
rm(DTRUMP)

VPUTIN <- NINE %>%
  group_by(CM_CLIENT, Month)%>%
  summarize(MonthlyActivations = n()   
  )

RREGAN <- NINE%>%
  group_by(CM_CLIENT)%>%
  summarize(TotalActivations=n(),
            WG19_Sent = sum(WG19_SENT == "Yes"),
            OW1_Sent = sum(OW1_SENT == "Yes"),
            OW2_Sent = sum(OW2_SENT == "Yes"), 
            TotalGAR = sum(GAR_Dollars),
            TotalVol = sum(Total_Voluntary),
            TotalCollected = sum (Total_Direct),
            Resolutions = sum(ED_CUR_COND == "REHAB3" | ED_CUR_COND == "REHAB2" | ED_CUR_COND == "REHAB" | ED_CUR_COND == "PPA" | ED_CUR_COND =="COMP" | ED_CUR_COND == "BIF"), 
            SuccessRate = Resolutions / TotalActivations
  )

VPUTIN <- left_join(VPUTIN, RREGAN,by="CM_CLIENT")
rm(RREGAN)

ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv", stringsAsFactors=FALSE)
ARMASTER <- select(ARMASTER,A.R, desk, manager)

NSABAN$ED_AWG_CREDIT_AR <- as.character(NSABAN$ED_AWG_CREDIT_AR)
ARMASTER$desk <- as.character(ARMASTER$desk)

ARMASTER <- rename(ARMASTER, ED_AWG_CREDIT_AR = desk)

NSABAN <- left_join(NSABAN, ARMASTER, by="ED_AWG_CREDIT_AR")
NSABAN <- NSABAN %>%
  mutate(A.R = ifelse(A.R == "","NLE", A.R))

Knoxville <- NINE %>% 
  filter(CM_CLIENT%in%c("7222","8035")) %>%
  group_by(Month) %>%
  summarize(Knoxville = n())
Columbus <- NINE %>% 
  filter(CM_CLIENT%in%c("C7222","C8035")) %>%
  group_by(Month) %>%
  summarize(Columbus = n())
Columbus2 <- NINE %>% 
  filter(CM_CLIENT%in%"B7222") %>%
  group_by(Month) %>%
  summarize(Columbus2 = n())
Schuerger <- NINE %>% 
  filter(CM_CLIENT%in%"S7222") %>%
  group_by(Month) %>%
  summarize(Schuerger = n())
Westlake <- NINE %>% 
  filter(CM_CLIENT%in%"W7222") %>%
  group_by(Month) %>%
  summarize(Westlake = n())

d3 <- left_join(Knoxville,Columbus,by="Month")
d3 <- left_join(d3,Columbus2, by="Month")
d3 <- left_join(d3,Westlake, by="Month")
d3 <- left_join(d3,Schuerger, by="Month")

rm(Schuerger);rm(Knoxville);rm(Columbus);rm(Columbus2); rm(Westlake)

BCLINTON <- NSABAN %>%
  group_by(manager, Month)%>%
  summarize(
    Monthly_Activations = sum(MonthlyActivations),
    TotalActivations = sum(TotalActivations),
    WG19_Sent = sum(WG19_Sent),
    OW1_Sent = sum(OW1_Sent),
    Ow2_Sent = sum(OW2_Sent),
    TotalGAR = sum(TotalGAR),
    TotalVol = sum(TotalVol),
    TotalCollected = sum(TotalCollected),
    Resolutions = sum(Resolutions),
    SuccessRate = Resolutions / TotalActivations
  ) 

time <- as.POSIXlt(Sys.time())
time <- paste("Last updated:", format(time,"%A, %x %X"))


d3$Month <- as.Date(paste("01",d3$Month),"%d %B %Y")
d3 <- d3 %>%
  arrange(Month)

d3$Month <- format(d3$Month,"%B %Y")

NSABAN$Month[is.na(NSABAN$Month)] <- "Bad"
NSABAN <- select(NSABAN, -WG19_Sent )

Lowhorn <- NINE[NINE$Month2 == "No WG19 Sent",]
Lowhorn <- Lowhorn[!is.na(Lowhorn$Debtor),]
Lowhorn <- Lowhorn[Lowhorn$CM_DESK >= 800 ,]
Lowhorn <- Lowhorn[Lowhorn$CM_DESK <= 899 ,]
Lowhorn <- Lowhorn %>% select(Debtor,ED_AWG_CREDIT_AR,ED_ACCT_ACT_DATE,ED_NOTICE_TO_BORR_DT)


NSABAN <- NSABAN %>%
  mutate(Efficiency = MonthlyActivations/InitiatedActivations)






write.csv(Lowhorn,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/WorkList.csv",row.names=F)

write.csv(d3,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Application File/www/data.csv",row.names=F)

write.csv(NSABAN,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/AWG_AR.csv",row.names=F)

write.csv(VPUTIN,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/AWG_CLIENT.csv",row.names=F)

write.csv(NINE,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Theta.csv",row.names=F)

write.csv(BCLINTON,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/MGRStats.csv",row.names=F)

write.csv(time,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/time.csv",row.names=F)


NINE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/NINE.csv", 
                 stringsAsFactors=FALSE)

TRACKERFILE <- read.delim("//Knx3fs01/ED_BA_GROUP/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/TRACKERFILE.txt", 
                          stringsAsFactors=FALSE)

TRACKERFILE <- select(TRACKERFILE,Debtor,EffDt,PostedClientCode,FundsType)
TRACKERFILE <- TRACKERFILE[TRACKERFILE$FundsType %in% c('RGWG','RVWG'),]
TRACKERFILE$Debtor <- as.character(TRACKERFILE$Debtor)
TRACKERFILE$EffDt <- as.Date(TRACKERFILE$EffDt,'%Y-%m-%d')

TRACKERFILE <- TRACKERFILE %>%
  arrange(EffDt)

NINE <- rename(NINE,Debtor = CM_FILENO)
NINE$Debtor <- as.character(NINE$Debtor)

N <- NINE %>%
  filter(ED_CUR_COND %in% c("REHAB",'REHAB2','REHAB3','COMP','BIF'))

N$ED_ACCT_ACT_DATE <- as.Date(N$ED_ACCT_ACT_DATE,'%m/%d/%Y')
N$Month <- format(N$ED_ACCT_ACT_DATE,"%B %Y")
N <- N[!N$Month %in% c("June 2017","July 2017"),]

N <- N[!duplicated(N$Debtor),]

N1 <- N %>%
  group_by(Month) %>%
  summarize(Payers = n())



NINE <- full_join(TRACKERFILE,NINE,by="Debtor")

rm(TRACKERFILE)

NINE$ED_ACCT_ACT_DATE <- as.Date(NINE$ED_ACCT_ACT_DATE,'%m/%d/%Y')

NINE <- NINE[!is.na(NINE$EffDt),]
NINE <- NINE[!is.na(NINE$ED_ACCT_ACT_DATE),]

a <- NINE %>%
  filter(EffDt > ED_ACCT_ACT_DATE) %>%
  mutate(Days_For_GAR = EffDt - ED_ACCT_ACT_DATE)

a <- a[!duplicated(a$Debtor),]

a$Days_For_GAR <- as.numeric(a$Days_For_GAR)

a$Month <- format(a$ED_ACCT_ACT_DATE,"%B %Y")
a <- a[!a$Month %in% c("June 2017","July 2017"),]

a$Month <- factor(a$Month,levels=c('October 2016','November 2016','December 2016','January 2017','February 2017',
                                   'March 2017','April 2017','May 2017'))

a <- a %>%
  filter(Days_For_GAR >= 45)

gentry <- a %>%
  group_by(Month) %>%
  summarize(Days_For_GAR = mean(Days_For_GAR),
            Accounts_W_GAR = n())

Accounts <- c(1638,3781,2331,2865,3238,6154,3809,4945)

gentry <- cbind(gentry,Accounts)

gentry <- gentry %>%
  mutate(Percent_W_GAR = Accounts_W_GAR/Accounts)



gentry <- left_join(gentry,N1,by="Month")

gentry <- gentry %>%
  mutate(Percent_Paying = Payers/Accounts)

gentry <- gentry[gentry$Month != 'NA',]
gentry <- gentry[complete.cases(gentry),]

write.csv(gentry,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/gentry.csv",row.names=F)
