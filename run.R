library(dplyr)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyr)
library(data.table)

Vendor <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Vendor.csv", stringsAsFactors=FALSE)
RGR <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/RGR.csv", stringsAsFactors=FALSE)
POE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/POE.csv", stringsAsFactors=FALSE)

Vendor$CM_FILENO <- as.character(Vendor$CM_FILENO)
RGR$CM_FILENO <- as.character(RGR$CM_FILENO)
POE$CM_FILENO <- as.character(POE$CM_FILENO)

V <- Vendor$CM_FILENO
R <- RGR$CM_FILENO

POE <- POE[!POE$CM_FILE %in% V,]
POE <- POE[!POE$CM_FILE %in% R,]

rm(R)
rm(V)

V.scrub <- select(Vendor, CM_FILENO,Batch.Date)
RGR.scrub <- select(RGR,CM_FILENO)
RGR.scrub$Batch.Date <- "1/1/2015"
POE.Scrub <- select(POE,CM_FILENO)
POE.Scrub$Batch.Date <- "1/1/2015"

knoxtalx <- rbind(V.scrub,RGR.scrub,POE.Scrub)
rm(V.scrub); rm(RGR.scrub); rm(POE.Scrub)
knoxtalx <- knoxtalx[!duplicated(knoxtalx$CM_FILENO),]
knoxtalx <- rename(knoxtalx, ACCOUNT=CM_FILENO,Date=Batch.Date)


CODE <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/CODE.csv")


df <- readRDS('C:/ACTDB/ACT_DB.rds')

DATEV <- knoxtalx
DATEV$Date <- as.Date(DATEV$Date, '%m/%d/%Y')
DATEV <- rename(DATEV,TFILE=ACCOUNT)


accountsr <- as.data.frame(as.character(RGR$CM_FILENO))
names(accountsr) <- 'TFILE'
accountsr$Date <- as.Date('2015-01-01','%Y-%m-%d')
accountsp <- as.data.frame(as.character(POE$CM_FILENO))
names(accountsp) <- 'TFILE'
accountsp$Date <- as.Date('2015-01-01','%Y-%m-%d')

accountz <- rbind(DATEV,accountsp,accountsr)
rm(accountsp); rm(accountsr); rm(DATEV)

df$TFILE <- as.character(df$TFILE)

df <- left_join(df,accountz,by='TFILE')
df <- df[!is.na(df$Date),]

df$ACT_DATE <- as.Date(df$ACT_DATE,'%m/%d/%Y')
#df <- df[df$Date >= df$ACT_DATE,]

#data <- df[!duplicated(df$TFILE),]

contacts <- c("CM","A3P","AT")
codes <- c("1C","1H","1P","215H","215W","225H","2H","2P","365W","370W","375W","380W","385W","390W","395W","400W","405W")
home <- c("1C","1H","215H","225H","2H","105H","1P","215A","310H","315H","335A","335H",
          "360H","364A","365A","365H","370H","375H","380H","385H","390H","395H","75H",
          "ACRH","ADLH","ADLW","AEHW","AKAA","AKAH","ALTA","ALTAA","ALTH","AP3H","BUSH",
          "CBCH","CBPA","CBPH","CBRH","CBRH","CIDH","CLAA","CLAH","CLIH","DNCH","EMPH","EMPA",
          "FRIH","FRNH","FTHH","HMEA","HMEH","HOH","INVH","MANA","MANH","MOTH","MU","MULTH",
          "N2PH","NB","NBYH","NLEH","OTHA","OTHERA","OTHERH","OTHH","PARH","PHNH","PLCH","PORH",
          "SLFH","SPOH","STDH","TALXH","TLOH","TRKH","TRUH","UNKH","VARH",'01H','105H','225H',
          '310H','360H','364H','370H','375H','380H','385H','390H','395H','400H','405H',
          '420H','A3CH','0A3PH','ACRH','AEH','AEHH','AEHP','AP3H','ATYH','BUSH','CIDH',
          'CMRH','COMH','FRIH','FRNH','FTHP','HMEA','HMEH','HMEP','HMEW','HOH','INVH','MOTH',
          'OTHERH','PLCH','SPOH','STDH','SUPH','TALXH','TLOH','UNKH','WLCDH')
poe1 <- c("1P","215W","2P")
poe <- c("1P","215W","2P","365W","370W","375W","380W","385W","390W","395W","400W","405W","335W",
         "TLOW","MANW","POEW","CBPW","SLFW","RELW","ADLW","TRUW","CLAW","PROW","UNKW","310W",
         "CBRW","NBYW","AKAW","CLIW","PLCW","REFW","ALTW","MULTW","EMPW","TRKW",'CBCP','CBPP',
         'CBRP','EMPA','EMPH','EMPP','EMPW','POE','POEA','POEA','POEH','POEP')

df <- select(df,-Date)

contact_df <- df[df$CODE_2 %in% contacts,]
contact_df$ACT_DATE <- as.Date(contact_df$ACT_DATE,"%m/%d/%Y")
#contact_df <- contact_df[contact_df$ACT_DATE >= DATE,]

contact_data <- contact_df %>%
  group_by(TFILE) %>%
  summarize(Contacts = n())

##############################################################
#df <- df[df$CODE_1 %in% codes,]
df$ACT_DATE <- as.Date(df$ACT_DATE,"%m/%d/%Y")

df$T <- gsub("^.*? ","",df$TIME)
df$DTE <- gsub(" 0:00:00","",df$ACT_DATE)
time <- as.data.frame(str_split_fixed(df$T,":",n=3))
time <- dplyr::rename(time, Hour = V1, Minute = V2, Second = V3)

time$Hour <- as.numeric(as.character(time$Hour))
time$Minute <- as.numeric(as.character(time$Minute))
time$Second <- as.numeric(as.character(time$Second))

df <- cbind(df,time)

df$Day <- weekdays(df$ACT_DATE) 
poedf <- df
poedf$ACT_DATE <- as.Date(poedf$ACT_DATE,"%m/%d/%Y")
knoxtalx <- rename(knoxtalx,TFILE=ACCOUNT)
poedf$TFILE <- as.character(poedf$TFILE)
poedf <- left_join(poedf,accountz,by="TFILE")
poedf$ACT_DATE <- as.Date(poedf$ACT_DATE,"%m/%d/%Y")
poedf <- poedf %>%
  mutate(flag = ifelse(ACT_DATE >= Date,1,0))
poedf <- poedf[poedf$flag >= 1,]
poedf <- poedf[poedf$CODE_1 %in% poe,]

##############################################################

mondayafternoon <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Monday") %>%
  mutate(Afternoon = ifelse(Hour>=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Monday_Afternoon = sum(Home))

mondaymorning <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Monday") %>%
  mutate(Afternoon = ifelse(Hour<=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Monday_Morning = sum(Home))

tuesdayafternoon <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Tuesday") %>%
  mutate(Afternoon = ifelse(Hour>=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Tuesday_Afternoon = sum(Home))

tuesdaymorning <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Tuesday") %>%
  mutate(Afternoon = ifelse(Hour<=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Tuesday_Morning = sum(Home))

wednesdayafternoon <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Wednesday") %>%
  mutate(Afternoon = ifelse(Hour>=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Wednesday_Afternoon = sum(Home))

wednesdaymorning <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Wednesday") %>%
  mutate(Afternoon = ifelse(Hour<=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Wednesday_Morning = sum(Home))

thursdayafternoon <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Thursday") %>%
  mutate(Afternoon = ifelse(Hour>=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Thursday_Afternoon = sum(Home))

thursdaymorning <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Thursday") %>%
  mutate(Afternoon = ifelse(Hour<=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Thursday_Morning = sum(Home))

fridayafternoon <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Friday") %>%
  mutate(Afternoon = ifelse(Hour>=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Friday_Afternoon = sum(Home))

fridaymorning <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Friday") %>%
  mutate(Afternoon = ifelse(Hour<=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Friday_Morning = sum(Home))

saturdaymorning <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  filter(Day=="Saturday") %>%
  mutate(Afternoon = ifelse(Hour<=12,1,0)) %>%
  filter(Afternoon == 1) %>%
  group_by(TFILE) %>%
  summarize(Saturday_Morning = sum(Home))

homedata <- df %>%
  mutate(Home = ifelse(CODE_1 %in% home,1,0)) %>%
  group_by(TFILE) %>%
  summarize(Home_Attempts = sum(Home==1))

poedata <- poedf %>%
  mutate(POE = ifelse(CODE_1 %in% poe1,1,0)) %>%
  group_by(TFILE) %>%
  summarize(POE_Attempts = sum(POE==1))

poemsg <- poedf %>%
  mutate(POE_MSG = ifelse(CODE_3 == "MSG",1,0)) %>%
  group_by(TFILE) %>%
  summarize(POE_Messages = sum(POE_MSG==1))

location1 <- poedf %>%
  filter(CODE_1=="365W") %>%
  group_by(TFILE) %>%
  summarize(Location1 = n())

location2 <- poedf %>%
  filter(CODE_1=="370W") %>%
  group_by(TFILE) %>%
  summarize(Location2 = n())

location3 <- poedf %>%
  filter(CODE_1=="375W") %>%
  group_by(TFILE) %>%
  summarize(Location3 = n())

location4 <- poedf %>%
  filter(CODE_1=="380W") %>%
  group_by(TFILE) %>%
  summarize(Location4 = n())

location5 <- poedf %>%
  filter(CODE_1=="385W") %>%
  group_by(TFILE) %>%
  summarize(Location5 = n())

location6 <- poedf %>%
  filter(CODE_1=="390W") %>%
  group_by(TFILE) %>%
  summarize(Location6 = n())

location7 <- poedf %>%
  filter(CODE_1=="395W") %>%
  group_by(TFILE) %>%
  summarize(Location7 = n())

location8 <- poedf %>%
  filter(CODE_1=="400W") %>%
  group_by(TFILE) %>%
  summarize(Location8 = n())

location9 <- poedf %>%
  filter(CODE_1=="405W") %>%
  group_by(TFILE) %>%
  summarize(Location9 = n())

finaldata <- left_join(homedata,contact_data,by="TFILE")
finaldata <- left_join(finaldata,mondaymorning,by="TFILE")     
finaldata <- left_join(finaldata,mondayafternoon,by="TFILE")  
finaldata <- left_join(finaldata,tuesdaymorning,by="TFILE")  
finaldata <- left_join(finaldata,tuesdayafternoon,by="TFILE")  
finaldata <- left_join(finaldata,wednesdaymorning,by="TFILE") 
finaldata <- left_join(finaldata,wednesdayafternoon,by="TFILE") 
finaldata <- left_join(finaldata,thursdaymorning,by="TFILE") 
finaldata <- left_join(finaldata,thursdayafternoon,by="TFILE") 
finaldata <- left_join(finaldata,fridaymorning,by="TFILE") 
finaldata <- left_join(finaldata,fridayafternoon,by="TFILE") 
finaldata <- left_join(finaldata,saturdaymorning,by="TFILE") 
finaldata$TFILE <- as.character(finaldata$TFILE)
finaldata <- left_join(finaldata,poedata,by="TFILE") 
finaldata <- left_join(finaldata,poemsg,by="TFILE") 
finaldata <- left_join(finaldata,location1,by="TFILE") 
finaldata <- left_join(finaldata,location2,by="TFILE") 
finaldata <- left_join(finaldata,location3,by="TFILE") 
finaldata <- left_join(finaldata,location4,by="TFILE") 
finaldata <- left_join(finaldata,location5,by="TFILE") 
finaldata <- left_join(finaldata,location6,by="TFILE") 
finaldata <- left_join(finaldata,location7,by="TFILE") 
finaldata <- left_join(finaldata,location8,by="TFILE") 
finaldata <- left_join(finaldata,location9,by="TFILE") 

finaldata[is.na(finaldata)] <- 0

Monday <- c("Monday_Afternoon","Monday_Morning")
Tuesday <- c("Tuesday_Afternoon","Tuesday_Morning")
Wednesday <- c("Wednesday_Afternoon","Wednesday_Morning")
Thursday <- c("Thursday_Afternoon","Thursday_Morning")
Friday <- c("Friday_Afternoon","Friday_Morning")
Saturday <- c("Saturday_Afternoon","Saturday_Morning")

finaldata <- finaldata %>%
  mutate(Mon_Morn = ifelse(Monday_Morning >= 1,1,0)) %>%
  mutate(Tues_Morn = ifelse(Tuesday_Morning >= 1,1,0)) %>%
  mutate(Wed_Morn = ifelse(Wednesday_Morning >= 1,1,0)) %>%
  mutate(Thur_Morn = ifelse(Thursday_Morning >= 1,1,0)) %>%
  mutate(Fri_Morn = ifelse(Friday_Morning >= 1,1,0)) %>%
  mutate(Sat_Morn = ifelse(Saturday_Morning >= 1,1,0)) %>%
  mutate(Morning = ifelse((Mon_Morn+Tues_Morn+Wed_Morn+Thur_Morn+Fri_Morn+Sat_Morn) >= 1,1,0)) %>%
  mutate(Mon_Night = ifelse(Monday_Afternoon >= 1,1,0)) %>%
  mutate(Tues_Night = ifelse(Tuesday_Afternoon >= 1,1,0)) %>%
  mutate(Wed_Night = ifelse(Wednesday_Afternoon >= 1,1,0)) %>%
  mutate(Thur_Night = ifelse(Thursday_Afternoon >= 1,1,0)) %>%
  mutate(Fri_Night = ifelse(Friday_Afternoon >= 1,1,0)) %>%
  mutate(Afternoon = ifelse((Mon_Night+Tues_Night+Wed_Night+Thur_Night+Fri_Night) >= 1,1,0)) %>%
  mutate(Time_Of_Day =ifelse((Morning+Afternoon)==2,"Yes","No")) %>%
  mutate(Num_of_Home_Attempts = ifelse(Home_Attempts >= 5,"Yes","No")) %>%
  mutate(Mon = ifelse((Monday_Morning+Monday_Afternoon)>=1,1,0)) %>%
  mutate(Tues = ifelse(sum(Tuesday_Morning,Tuesday_Afternoon)>=1,1,0)) %>%
  mutate(Wed = ifelse((Wednesday_Morning+Wednesday_Afternoon)>=1,1,0)) %>%
  mutate(Thurs = ifelse((Thursday_Morning+Thursday_Afternoon)>=1,1,0)) %>%
  mutate(Fri = ifelse((Friday_Morning+Friday_Afternoon)>=1,1,0)) %>%
  mutate(Sat = ifelse(Saturday_Morning>=1,1,0)) %>%
  mutate(HA_Multiple_Days_of_Week = ifelse((Mon+Tues+Wed+Thurs+Fri+Sat)>=3,"Yes","No")) %>%
  mutate(Location1 = ifelse(Location1 >=1,1,0)) %>%
  mutate(Location2 = ifelse(Location2 >=1,1,0)) %>%
  mutate(Location3 = ifelse(Location3 >=1,1,0)) %>%
  mutate(Location4 = ifelse(Location4 >=1,1,0)) %>%
  mutate(Location5 = ifelse(Location5 >=1,1,0)) %>%
  mutate(Location6 = ifelse(Location6 >=1,1,0)) %>%
  mutate(Location7 = ifelse(Location7 >=1,1,0)) %>%
  mutate(Location8 = ifelse(Location8 >=1,1,0)) %>%
  mutate(Location9 = ifelse(Location9 >=1,1,0)) %>%
  mutate(POE_Location_Attempts = Location1+Location2+Location3+Location4+Location5+Location6+Location7+Location8+Location9) %>%
  select(TFILE,Contacts,Num_of_Home_Attempts,Time_Of_Day,HA_Multiple_Days_of_Week,POE_Attempts,POE_Messages,POE_Location_Attempts)

finale <- left_join(knoxtalx, finaldata,by="TFILE")

rm(CODE);rm(DATE);rm(contact_data);rm(contact_df);rm(data); rm(df); rm(finaldata);rm(fridaymorning)
rm(fridayafternoon);rm(homedata);rm(knoxtalx);rm(location1);rm(location2);rm(location3)
rm(location4);rm(location5);rm(location6);rm(location7);rm(location8);rm(location9); rm(mondayafternoon)
rm(mondaymorning);rm(poedata); rm(poedf); rm(poemsg); rm(saturdaymorning); rm(thursdayafternoon)
rm(thursdaymorning); rm(time); rm(tuesdayafternoon); rm(tuesdaymorning); rm(wednesdayafternoon); rm(wednesdaymorning)
rm(Friday); rm(Monday); rm(Tuesday); rm(Wednesday); rm(Thursday); rm(Saturday); rm(accounts)
rm(codes); rm(contacts); rm(home); rm(poe); rm(poe1); rm(r); rm(s)

accounts <- as.numeric(finale$TFILE)

LETTERS_2017 <- read.delim("//Knx3it/edopsmgmt/Reports/Stamm Reports/Data_Drop/LETTERS_2017.txt", stringsAsFactors=FALSE)

LETTERS_2016 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2016.txt",
                           stringsAsFactors=FALSE)
LETTERS_2015 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2015.txt", 
                           stringsAsFactors=FALSE)

LETTERS_2014 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2014.txt", 
                           stringsAsFactors=FALSE,header=FALSE)

colnames(LETTERS_2014) <- c("Debtor","X30A.Date","X30W.Date","X30B.Date")

LETTERS_2013 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2013.txt", 
                           stringsAsFactors=FALSE)

LETTERS_2012 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2012.txt", 
                           stringsAsFactors=FALSE)

LETTERS_2011 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2011.txt", 
                           stringsAsFactors=FALSE)
LETTERS_2010 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2010.txt", 
                           stringsAsFactors=FALSE)

LETTERS_2009 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2009.txt", 
                           stringsAsFactors=FALSE)
LETTERS_2008 <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Data_Drop/LETTERS_2008.txt", 
                           stringsAsFactors=FALSE)

LETTERS <- rbind(LETTERS_2017,LETTERS_2016,LETTERS_2015,LETTERS_2014,LETTERS_2013,LETTERS_2012,LETTERS_2011,LETTERS_2010,LETTERS_2009,LETTERS_2008)

rm(LETTERS_2017);rm(LETTERS_2016); rm(LETTERS_2015);rm(LETTERS_2014);rm(LETTERS_2013);rm(LETTERS_2012);rm(LETTERS_2011);rm(LETTERS_2010);rm(LETTERS_2009);rm(LETTERS_2008)


LETTERS <- LETTERS[LETTERS$Debtor %in% accounts,]

LETTERS <- mutate(LETTERS,Letters_Sent="Yes")

LETTERS <- rename(LETTERS,TFILE=Debtor)

LETTERS$TFILE <- as.character(LETTERS$TFILE)

#LETTERS <- select(LETTERS, TFILE, Letters_Sent)
LETTERS <- LETTERS[!duplicated(LETTERS$TFILE),]

finale <- left_join(finale,LETTERS,by="TFILE")

finale <- finale[!is.na(finale$TFILE),]

finale$Letters_Sent[is.na(finale$Letters_Sent)] <- "No"  
rm(DATE)
rm(CODE)
rm(df)
rm(LETTERS)
rm(accounts)
finale <- rename(finale,CM_FILENO=TFILE)
finale <- finale[,names(finale)!=c("Contacts","Date")]

finale$Num_of_Home_Attempts[is.na(finale$Num_of_Home_Attempts)] <- "No"
finale$Time_Of_Day[is.na(finale$Time_Of_Day)] <- "No"
finale$HA_Multiple_Days_of_Week[is.na(finale$HA_Multiple_Days_of_Week)] <- "No"
finale$POE_Attempts[is.na(finale$POE_Attempts)] <- 0
finale$POE_Location_Attempts[is.na(finale$POE_Location_Attempts)] <- 0
finale$POE_Messages[is.na(finale$POE_Messages)] <- 0

Vendor <- left_join(Vendor,finale,by="CM_FILENO")
RGR <- left_join(RGR,finale,by="CM_FILENO")
POE <- left_join(POE,finale,by="CM_FILENO")
rm(finale)

Contacts <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Contacts.csv", 
                     stringsAsFactors=FALSE)
Contacts$CM_FILENO <- as.character(Contacts$CM_FILENO)
Vendor <- left_join(Vendor,Contacts,by="CM_FILENO")
RGR <- left_join(RGR,Contacts,by="CM_FILENO")
POE <- left_join(POE,Contacts,by="CM_FILENO")
rm(Contacts)
Vendor$Contacts[is.na(Vendor$Contacts)] <- 0
RGR$Contacts[is.na(RGR$Contacts)] <- 0
POE$Contacts[is.na(POE$Contacts)] <- 0

Vendor$ED_REJD_DATE <- as.Date(Vendor$ED_REJD_DATE,"%m/%d/%Y")
Vendor$ED_REJD_DATE[is.na(Vendor$ED_REJD_DATE)] <- "2010-01-01"
Vendor$ED_LAST_VOL_PAY_DATE <- as.Date(Vendor$ED_LAST_VOL_PAY_DATE,"%m/%d/%Y")
Vendor$ED_LAST_VOL_PAY_DATE[is.na(Vendor$ED_LAST_VOL_PAY_DATE)] <- "2010-01-01"

RGR$ED_REJD_DATE <- as.Date(RGR$ED_REJD_DATE,"%m/%d/%Y")
RGR$ED_REJD_DATE[is.na(RGR$ED_REJD_DATE)] <- "2010-01-01"
RGR$ED_LAST_VOL_PAY_DATE <- as.Date(RGR$ED_LAST_VOL_PAY_DATE,"%m/%d/%Y")
RGR$ED_LAST_VOL_PAY_DATE[is.na(RGR$ED_LAST_VOL_PAY_DATE)] <- "2010-01-01"
RGR$NAME1_VERIFY_POE <- as.Date(RGR$NAME1_VERIFY_POE,"%m/%d/%Y")
RGR$NAME1_VERIFY_POE[is.na(RGR$NAME1_VERIFY_POE)] <- "2010-01-01"

POE$ED_REJD_DATE <- as.Date(POE$ED_REJD_DATE,"%m/%d/%Y")
POE$ED_REJD_DATE[is.na(POE$ED_REJD_DATE)] <- "2010-01-01"
POE$ED_LAST_VOL_PAY_DATE <- as.Date(POE$ED_LAST_VOL_PAY_DATE,"%m/%d/%Y")
POE$ED_LAST_VOL_PAY_DATE[is.na(POE$ED_LAST_VOL_PAY_DATE)] <- "2010-01-01"
POE$NAME1_VERIFY_POE <- as.Date(POE$NAME1_VERIFY_POE,"%m/%d/%Y")
POE$NAME1_VERIFY_POE[is.na(POE$NAME1_VERIFY_POE)] <- "2010-01-01"


Vendor <- Vendor[!duplicated(Vendor[c("File","Vendor","Batch.Date","Office")]),]

Vendor$CM_LAST_WORK_DATE <- as.Date(Vendor$CM_LAST_WORK_DATE,"%m/%d/%Y")
Vendor$CM_LAST_WORK_DATE[is.na(Vendor$CM_LAST_WORK_DATE)] <- "2000-01-01"

Vendor$Batch.Date <- as.Date(Vendor$Batch.Date,"%m/%d/%Y")

Vendor$CM_CLI_PLC_DATE <- as.Date(Vendor$CM_CLI_PLC_DATE,'%m/%d/%Y')
RGR$CM_CLI_PLC_DATE <- as.Date(RGR$CM_CLI_PLC_DATE,'%m/%d/%Y')
POE$CM_CLI_PLC_DATE <- as.Date(POE$CM_CLI_PLC_DATE,'%m/%d/%Y')
Vendor <- Vendor %>%
  mutate(Not_Worked_in_72 = ifelse((Sys.Date()-CM_LAST_WORK_DATE) >= 3,"No","Yes"),
         #    Not_Worked_in_72 = ifelse(is.null(Not_Worked_in_72),"Yes",Not_Worked_in_72),
         Payer = ifelse(ED_CUR_COND == "COMP" | ED_CUR_COND == "REHAB" | ED_CUR_COND == "REHAB2" | ED_CUR_COND == "REHAB3" | ED_CUR_COND == "PPA",
                        "Yes","No"),
         Payer = ifelse(CM_STATUS == "FRW" | CM_STATUS == "FPW" | CM_STATUS == "FSW" | CM_STATUS == "AAP" | CM_STATUS == "ATY" | CM_STATUS == "DSP" | CM_STATUS == "BKY","Yes",Payer),
         Reject = ifelse(ED_REJD_DATE >= Sys.Date()-180,"Yes","No"),
         Letters = ifelse((ED_30A_DATE != "" & ED_30B_DATE != "" & ED_30W_DATE != "")|Letters_Sent == "Yes","Yes","No"),
         Good_Desk = ifelse(CM_DESK >= 600 | CM_DESK == 80 | CM_DESK == 1,"No","Yes"),
         Good_Desk = ifelse(CM_DESK == 613 | CM_DESK == 680 | CM_DESK == 681 | CM_DESK == 682 | CM_DESK == 683 | CM_DESK == 684 | CM_DESK == 685 | CM_DESK == 686 | CM_DESK == 991 | CM_DESK == 997,"Yes",Good_Desk),
         Payment = ifelse((Sys.Date()-45)<=ED_LAST_VOL_PAY_DATE,"Yes","No"),
         Activations_Req = ifelse((Contacts >=1 & CM_CLI_PLC_DATE < (Sys.Date()-150) &CM_CURR_BAL >= 300 & (ED_30A_DATE != '' | X30A.Date !='') & Good_Desk == "Yes" & Reject == "No" & Payer == "No" & Payment == "No" & (Batch.Date >= Sys.Date()-150)) | (Num_of_Home_Attempts == "Yes" & Time_Of_Day == "Yes" & HA_Multiple_Days_of_Week == "Yes" & (POE_Attempts >=2 | POE_Location_Attempts >= 7) & Letters == "Yes" & Good_Desk == "Yes" & Reject == "No" & Payer == "No" & CM_CURR_BAL >= 300 & CM_CLI_PLC_DATE < (Sys.Date()-60) &Payment == "No"&(Batch.Date >= Sys.Date()-150)),
                                  "Yes","No"),
         Reason = ifelse(Payer =="Yes","Paying Condition",
                         ifelse(Reject == "Yes", "Previously Rejected",
                                ifelse(Letters == "No", "Needs Letters Sent",
                                       ifelse(Good_Desk == "No", "Unsweepable Desks",
                                              ifelse(Payment == "Yes", "Voluntary Payment in Last 45 Days",
                                                     ifelse(Num_of_Home_Attempts == "No", "Needs Home Attempts",
                                                            ifelse(Time_Of_Day == "No", "Needs Calls on Different Times of Day",
                                                                   ifelse(HA_Multiple_Days_of_Week == "No","Needs Calls on Different Days of Week",
                                                                          ifelse((POE_Attempts <2 & POE_Location_Attempts <7), "Needs POE Location Attempts",
                                                                                 ifelse(POE_Attempts <2, "Needs POE Attempts",
                                                                                        ifelse(Activations_Req == "Yes","Sweepable","See AWG Management"))))))))))))


Vendor <- Vendor %>%
  mutate(Activations_Req = ifelse( (Office == 'CBUS' | Office == 'CBUS2') & Vendor == 'Verifacts' & Batch.Date == ' 2017-09-21','No',Activations_Req ))


RGR <- RGR %>%
  mutate(Payer = ifelse(ED_CUR_COND == "COMP" | ED_CUR_COND == "REHAB" | ED_CUR_COND == "REHAB2" | ED_CUR_COND == "REHAB3" | ED_CUR_COND == "PPA",
                        "Yes","No"),
         Payer = ifelse(CM_STATUS == "FRW" | CM_STATUS == "FPW" | CM_STATUS == "FSW" | CM_STATUS == "AAP" | CM_STATUS == "ATY","Yes",Payer),
         Reject = ifelse(ED_REJD_DATE >= Sys.Date()-180,"Yes","No"),
         Letters = ifelse((ED_30A_DATE != "" & ED_30B_DATE != "" & ED_30W_DATE != "")|Letters_Sent == "Yes","Yes","No"),
         Good_Desk = ifelse(CM_DESK >= 600 | CM_DESK == 80 | CM_DESK == 1,"No","Yes"),
         Good_Desk = ifelse(CM_DESK == 613 | CM_DESK == 680 | CM_DESK == 681 | CM_DESK == 682 | CM_DESK == 683 | CM_DESK == 684 | CM_DESK == 685 | CM_DESK == 991 | CM_DESK == 997,"Yes",Good_Desk),
         Payment = ifelse((Sys.Date()-45)<=ED_LAST_VOL_PAY_DATE,"Yes","No"),
         Verified = ifelse(Sys.Date()-150 <= NAME1_VERIFY_POE,"Yes","No" ),
         Activations_Req = ifelse((Verified == "Yes" & CM_CURR_BAL >= 300 & CM_CLI_PLC_DATE < (Sys.Date()-30) &Contacts >=1 & (ED_30A_DATE != '' | X30A.Date !='') & Good_Desk == "Yes" & Reject == "No" & Payer == "No" & Payment == "No") | (Verified == "Yes" & Num_of_Home_Attempts == "Yes" & Time_Of_Day == "Yes" & HA_Multiple_Days_of_Week == "Yes" & (POE_Attempts >=2 | POE_Location_Attempts >= 7) & Letters == "Yes" & Good_Desk == "Yes" & Reject == "No" & CM_CURR_BAL >= 300 & CM_CLI_PLC_DATE < (Sys.Date()-60) &Payer == "No" & Payment == "No"),
                                  "Yes","No"),
         Reason = ifelse(Payer =="Yes","Paying Condition",
                         ifelse(Reject == "Yes", "Previously Rejected",
                                ifelse(Letters == "No", "Needs Letters Sent",
                                       ifelse(Good_Desk == "No", "Unsweepable Desks",
                                              ifelse(Payment == "Yes", "Voluntary Payment in Last 45 Days",
                                                     ifelse(Num_of_Home_Attempts == "No", "Needs Home Attempts",
                                                            ifelse(Time_Of_Day == "No", "Needs Calls on Different Times of Day",
                                                                   ifelse(HA_Multiple_Days_of_Week == "No","Needs Calls on Different Days of Week",
                                                                          ifelse((POE_Attempts <2 & POE_Location_Attempts <7), "Needs POE Location Attempts",
                                                                                 ifelse(POE_Attempts <2, "Needs POE Attempts",
                                                                                        ifelse(Verified=="No","Needs 2 Screen Verification Date W/I Last 30 Days","Sweepable"))))))))))))


POE <- POE %>%
  mutate(Payer = ifelse(ED_CUR_COND == "COMP" | ED_CUR_COND == "REHAB" | ED_CUR_COND == "REHAB2" | ED_CUR_COND == "REHAB3" | ED_CUR_COND == "PPA",
                        "Yes","No"),
         Payer = ifelse(CM_STATUS == "FRW" | CM_STATUS == "FPW" | CM_STATUS == "FSW" | CM_STATUS == "AAP" | CM_STATUS == "ATY","Yes",Payer),
         Reject = ifelse(ED_REJD_DATE >= Sys.Date()-180,"Yes","No"),
         Letters = ifelse((ED_30A_DATE != "" & ED_30B_DATE != "" & ED_30W_DATE != "")|Letters_Sent == "Yes","Yes","No"),
         Good_Desk = ifelse(CM_DESK >= 600 | CM_DESK == 80 | CM_DESK == 1,"No","Yes"),
         Good_Desk = ifelse(CM_DESK == 613 | CM_DESK == 680 | CM_DESK == 681 | CM_DESK == 682 | CM_DESK == 683 | CM_DESK == 684 | CM_DESK == 685 |CM_DESK == 991 | CM_DESK == 997,"Yes",Good_Desk),
         Payment = ifelse((Sys.Date()-45)<=ED_LAST_VOL_PAY_DATE,"Yes","No"),
         Verified = ifelse(Sys.Date()-150 <= NAME1_VERIFY_POE,"Yes","No" ),
         Activations_Req = ifelse((Verified == "Yes" & Contacts >=1 & (ED_30A_DATE != '' | X30A.Date !='') & CM_CLI_PLC_DATE < (Sys.Date()-30) &Good_Desk == "Yes" & Reject == "No" & Payer == "No" & Payment == "No") | (Verified == "Yes" & Num_of_Home_Attempts == "Yes" & Time_Of_Day == "Yes" & HA_Multiple_Days_of_Week == "Yes" & (POE_Attempts >=2 | POE_Location_Attempts >= 7) & Letters == "Yes" & Good_Desk == "Yes" & Reject == "No" & Payer == "No" & CM_CLI_PLC_DATE < (Sys.Date()-60) &Payment == "No"),
                                  "Yes","No"),
         Reason = ifelse(Payer =="Yes","Paying Condition",
                         ifelse(Reject == "Yes", "Previously Rejected",
                                ifelse(Letters == "No", "Needs Letters Sent",
                                       ifelse(Good_Desk == "No", "Unsweepable Desks",
                                              ifelse(Payment == "Yes", "Voluntary Payment in Last 45 Days",
                                                     ifelse(Num_of_Home_Attempts == "No", "Needs Home Attempts",
                                                            ifelse(Time_Of_Day == "No", "Needs Calls on Different Times of Day",
                                                                   ifelse(HA_Multiple_Days_of_Week == "No","Needs Calls on Different Days of Week",
                                                                          ifelse((POE_Attempts <2 & POE_Location_Attempts <7), "Needs POE Location Attempts",
                                                                                 ifelse(POE_Attempts <2, "Needs POE Attempts",
                                                                                        ifelse(Verified=="No","Needs 2 Screen Verification Date W/I Last 30 Days","Sweepable"))))))))))))


ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv", stringsAsFactors=FALSE)
ARMASTER <- select(ARMASTER,EMPNUM,desk,off)
ARMASTER <- ARMASTER %>% filter(EMPNUM != 0) %>% filter(desk !=1)
ARMASTER$EMPNUM <- as.character(ARMASTER$EMPNUM)

SUP <- read.csv("//knx3it/AWG Management/RGR/Activation Needs Dashboard/Data/SUP.csv", stringsAsFactors=FALSE)
SUP <- rename(SUP,EMPNUM = Employee.ID)
SUP <- select(SUP, EMPNUM,Name, Supervisor.ID, Supervisor.Name)
SUP$EMPNUM <- as.character(SUP$EMPNUM)

ARMASTER <- left_join(ARMASTER,SUP,by="EMPNUM")
ARMASTER$Supervisor.ID <- as.character(ARMASTER$Supervisor.ID)

SUPN <- ARMASTER$Supervisor.Namea
SUPI <- ARMASTER$Supervisor.ID


ARMASTER <- ARMASTER %>%
  mutate(Supervisor.Name = ifelse(Name %in% SUPN,Name,Supervisor.Name),
         Supervisor.ID = ifelse(EMPNUM %in% SUPI,EMPNUM,Supervisor.ID)
  )



SUP <- rename(SUP, CM_NAME = Supervisor.Name,CM_ID=Supervisor.ID,Supervisor.ID=EMPNUM)
SUP$CM_ID <- as.character(SUP$CM_ID)
SUP <- SUP %>% select(Supervisor.ID,CM_ID,CM_NAME)
ARMASTER$Supervisor.ID <- as.character(ARMASTER$Supervisor.ID)

ARMASTER <- left_join(ARMASTER,SUP,by="Supervisor.ID")
rm(SUP)

#####BUILD THE SUBS#####



#######################
ARMASTER <- select(ARMASTER,desk,off,Name,Supervisor.Name,CM_NAME)
ARMASTER <- ARMASTER[!is.na(ARMASTER$Name),]
ARMASTER <- rename(ARMASTER,Manager.Name=CM_NAME)
ARMASTER <- rename(ARMASTER,CM_DESK=desk)
ARMASTER$CM_DESK <- as.character(ARMASTER$CM_DESK)
Vendor$CM_DESK <- as.character(Vendor$CM_DESK)
RGR$CM_DESK <- as.character(RGR$CM_DESK)
POE$CM_DESK <- as.character(POE$CM_DESK)
ARMASTER <- ARMASTER %>%
  mutate(Manager.Name = ifelse(Manager.Name == "Tiffany Strickenberger",Supervisor.Name,Manager.Name))




Vendor <- left_join(Vendor,ARMASTER,by="CM_DESK")
RGR <- left_join(RGR,ARMASTER,by="CM_DESK")
POE <- left_join(POE,ARMASTER,by="CM_DESK")

Vendor$Manager.Name[is.na(Vendor$Manager.Name)] <- "Hold Desk"
Vendor$Supervisor.Name[is.na(Vendor$Supervisor.Name)] <- "Hold Desk"
Vendor$Name[is.na(Vendor$Name)] <- "Hold Desk"


VendorPivot <- Vendor %>% filter(CM_DESK <= 599 | CM_DESK>=900)
VendorPivot <- VendorPivot %>%
  select(File,CM_DESK,Office,Batch.Date,CM_STATUS,Not_Worked_in_72,ED_CUR_COND,CM_VALID_HOME_PHN,CM_VALID_POE_PHN,Activations_Req,Reason,Name,Supervisor.Name,Manager.Name)

RGR$Manager.Name[is.na(RGR$Manager.Name)] <- "Hold Desk"
RGR$Supervisor.Name[is.na(RGR$Supervisor.Name)] <- "Hold Desk"
RGR$Name[is.na(RGR$Name)] <- "Hold Desk"

RGR$off <- as.factor(RGR$off)
RGR$off <- plyr::revalue(RGR$off,c("C"="Columbus","AGY"="Knoxville","B"="Columbus 2","K"="Knoxville"))

RGRPivot <- RGR %>% filter(CM_DESK <- 599 | CM_DESK>=900)
RGRPivot <- RGRPivot %>%
  select(CM_FILENO,CM_DESK,CM_VALID_HOME_PHN,CM_VALID_POE_PHN,ED_AWG_CREDIT_AR,Activations_Req,Reason,off,Name,Supervisor.Name,Manager.Name)

Vendor$ED_ACCT_ACT_DATE <- as.Date(Vendor$ED_ACCT_ACT_DATE,"%m/%d/%Y")
Vendor$ED_ACCT_ACT_DATE[is.na(Vendor$ED_ACCT_ACT_DATE)] <- "2010-01-01"
#Vendor$Batch.Date <- as.Date(Vendor$Batch.Date,"%m/%d/%Y")
Vendor$Batch.Date[is.na(Vendor$Batch.Date)] <- "2010-01-01"
Vendor$ED_LAST_GARN_PMT_DT <- as.Date(Vendor$ED_LAST_GARN_PMT_DT,"%m/%d/%Y")
Vendor$ED_LAST_GARN_PMT_DT[is.na(Vendor$ED_LAST_GARN_PMT_DT)] <- "2010-01-01"
Vendor$ED_ORDER_TO_EMP_DT <- as.Date(Vendor$ED_ORDER_TO_EMP_DT,"%m/%d/%Y")
Vendor$ED_ORDER_TO_EMP_DT[is.na(Vendor$ED_ORDER_TO_EMP_DT)] <- "2010-01-01"



Summary <- Vendor %>%
  group_by(Office, Vendor, Batch.Date) %>%
  summarize(Files_Received = n(),
            Files_Activated = sum(ED_ACCT_ACT_DATE>=Batch.Date),
            Percent_Activated = Files_Activated/Files_Received,
            Voluntary_Payers = sum(Payer=="Yes"),
            Percent_Paying = Voluntary_Payers/Files_Received,
            Orders_Sent = sum(ED_ORDER_TO_EMP_DT>=Batch.Date),
            Activated_With_Order = Orders_Sent/Files_Activated,
            GAR_Pay_Last_30 = sum((Sys.Date()-30)<=ED_LAST_GARN_PMT_DT),
            GAR_Pay_Last_90 = sum((Sys.Date()-90)<=ED_LAST_GARN_PMT_DT)
  )%>%
  ungroup() %>% 
  arrange((Batch.Date))

Summary <- plyr::rename(Summary,c("Batch.Date"="Batch Date","Files_Received"="Files Received",
                                  "Files_Activated"="Files Activated","Percent_Activated"='Percent Activated',
                                  "Voluntary_Payers"="Voluntary Payers","Percent_Paying"="Percent Paying",
                                  "Orders_Sent"="Orders Sent","Activated_With_Order"="Activated With Order",
                                  "GAR_Pay_Last_30"="GAR Payment in Last 30 Days",
                                  "GAR_Pay_Last_90"="GAR Payment in Last 90 Days"))

POE$Manager.Name[is.na(POE$Manager.Name)] <- "Hold Desk"
POE$Supervisor.Name[is.na(POE$Supervisor.Name)] <- "Hold Desk"
POE$Name[is.na(POE$Name)] <- "Hold Desk"

POE <- POE[!duplicated(POE$CM_FILENO),]
POE <- POE[POE$CM_CLIENT != "",]
POE <- POE[POE$CM_STATUS != "",]
POE <- POE[POE$CM_CURR_BAL != "",]
POE <- POE[POE$CM_DESK != "",]
POE <- POE[POE$CM_FILENO != "",]
POE <- POE[POE$NAME1_EMP_NAME != "",]


VendorA <- Vendor %>% select(CM_FILENO,Reason) %>% mutate(Type = "Vendor")
RGRA <- RGR %>% select(CM_FILENO,Reason) %>% mutate(Type = "RGR")
POEA <- POE %>% select(CM_FILENO,Reason) %>% mutate(Type = "POE")

SearchD <- rbind(VendorA,RGRA,POEA)
rm(VendorA); rm(RGRA); rm(POEA)

SearchD <- SearchD[!duplicated(SearchD$CM_FILENO),]

SearchD$CM_FILENO <- as.numeric(SearchD$CM_FILENO)


Summary$Office <- as.factor(Summary$Office)
Summary$Vendor <- as.factor(Summary$Vendor)
Summary$"Batch Date" <- as.factor(Summary$"Batch Date")

write.csv(Summary,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Summary.csv",row.names=F)

POEPivot <- POE %>% filter(CM_DESK <= 599 | CM_DESK>=900) %>%
  select(CM_FILENO,CM_DESK,CM_CLIENT,CM_STATUS,ED_CUR_COND,Activations_Req,Reason,Name,Supervisor.Name,Manager.Name,Activations_Req)

Vendor$Batch.Date <- as.character(Vendor$Batch.Date)
ARMAST <- select(ARMASTER,CM_DESK,Name,Supervisor.Name,Manager.Name)
ARMAST$Name <- as.factor(ARMAST$Name)
ARMAST$Supervisor.Name <- as.factor(ARMAST$Supervisor.Name)
ARMAST$Manager.Name <- as.factor(ARMAST$Manager.Name)
ARMAST <- rename(ARMAST,Credit_AR=CM_DESK)
#####

rgrsweep <- RGR[RGR$Activations_Req == "Yes",]
rgrs <- RGR$CM_FILENO

doublecheck <- RGR %>% select(CM_FILENO,ED_AWG_CREDIT_AR)

vendorsweep <- Vendor[Vendor$Activations_Req == "Yes",]
vendorz <- vendorsweep$CM_FILENO

responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
responseout <- responses[responses$Checked == "Yes",]
responseout$Date <- as.Date(responseout$Date,"%m/%d/%Y")
responseout$Date <- format(responseout$Date,"%m/1/%Y")
currmonth <- format(Sys.Date(),"%m/1/%Y")
responseout <- responseout[responseout$Date %in% currmonth,]

P <- POE %>% select(CM_FILENO,CM_DESK)
responseout$File <- as.character(responseout$File)
#responseout <- left_join(responseout,P,by='File')


rgrsweep <- rgrsweep %>%
  mutate(Vendor = ifelse(CM_FILENO %in% vendorz,"Yes","No"))

rgrsweep <- select(rgrsweep,CM_FILENO,ED_AWG_CREDIT_AR,Vendor)

vendorsweep <- vendorsweep %>% select(CM_FILENO) %>% mutate(RGR = ifelse(CM_FILENO %in% rgrs,"Yes","No"))
vendorsweep <- left_join(vendorsweep,select(rgrsweep,CM_FILENO,ED_AWG_CREDIT_AR),by="CM_FILENO")
vendorsweep$ED_AWG_CREDIT_AR[is.na(vendorsweep$ED_AWG_CREDIT_AR)] <- ""

vendrgr <- filter(vendorsweep,RGR=="Yes")
vendrgr <- rename(vendrgr,Vendor = RGR)

out <- select(responseout,File,Credit_AR)

out <- out %>%
  mutate(Vendor = ifelse(File %in% vendorz,"Yes","No")) %>%
  rename(CM_FILENO = File,ED_AWG_CREDIT_AR = Credit_AR)

rgrsweep <- rbind(rgrsweep,vendrgr)
rgrsweep <- rbind(rgrsweep, out)
rm(vendrgr);rm(rgrs);rm(vendorz);rm(out)

vendorsweep <- filter(vendorsweep,RGR=="No")
rgrsweep$ED_AWG_CREDIT_AR <- as.character(rgrsweep$ED_AWG_CREDIT_AR)
rgrsweep <- rgrsweep %>%
  mutate(ED_AWG_CREDIT_AR = ifelse(is.na(ED_AWG_CREDIT_AR),"",ED_AWG_CREDIT_AR))

doublecheck <- rename(doublecheck,CRAR = ED_AWG_CREDIT_AR)

rgrsweep <- left_join(rgrsweep,doublecheck,by='CM_FILENO')

rgrsweep <- rgrsweep %>%
  mutate(ED_AWG_CREDIT_AR = ifelse(ED_AWG_CREDIT_AR == '',CRAR,ED_AWG_CREDIT_AR))

rgrsweep$ED_AWG_CREDIT_AR[is.na(rgrsweep$ED_AWG_CREDIT_AR)] <- ''
rgrsweep <- rgrsweep[,!names(rgrsweep) %in% 'CRAR']

RGR.Database <- read.csv("//KNX3IT/AWG Management/RGR/RGR Database.csv", stringsAsFactors=FALSE)

rgrsweep$Month <- currmonth

currmonth <- as.Date(currmonth,"%m/%d/%Y")

RGR.Database$Month <- as.Date(RGR.Database$Month,"%m/%d/%Y")
files <- RGR.Database[RGR.Database$Month %in% currmonth,]
RGR.Database$Month <- format(RGR.Database$Month,"%m/%d/%Y")

files <- as.character(files$Debtor)
rgrsweep$CM_FILENO <- as.character(rgrsweep$CM_FILENO)

rgrsweep <- rgrsweep[!duplicated(rgrsweep$CM_FILENO),]


lo <- left_join(rgrsweep,P,by='CM_FILENO')
lo <- lo[!is.na(lo$CM_DESK),]

rgrsweep<- rgrsweep[!rgrsweep$CM_FILENO %in% files,]


join <- rename(rgrsweep,Debtor = CM_FILENO,CRAR=ED_AWG_CREDIT_AR,Vendor.File.=Vendor)

RGR.Database$CRAR <- as.character(RGR.Database$CRAR)
RGR.Database$Debtor <- as.character(RGR.Database$Debtor)

join <- join %>% select(Month,Debtor,CRAR,Vendor.File.)

RGR.Database <- rbind(RGR.Database,join)
RGR.Database$CRAR[is.na(RGR.Database$CRAR)] <- ""

RGR.Database <- RGR.Database[!duplicated(RGR.Database[c("Month","Debtor","CRAR")]) ,]


rgrsweep <- rgrsweep[!duplicated(rgrsweep$CM_FILENO),]
vendorsweep <- vendorsweep[!duplicated(vendorsweep$CM_FILENO),]


rgrdbase <- RGR.Database$Debtor
sweptrgr <- rgrsweep$CM_FILENO

responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
responses <- rbind(Graves,responses)
responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
responses$Date <- format(responses$Date,"%m/%d/%Y")
responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]

responses$File <- as.character(responses$File)

responses <- responses %>%
  mutate(Response = ifelse(File %in% rgrdbase | File %in% sweptrgr,"Yes",Response),
         Reason_Denied = ifelse(File %in% rgrdbase | File %in% sweptrgr,"NULL",Reason_Denied))


write.csv(responses,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv",row.names=F)
write.csv(responses,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves",row.names=F)

RGR <- RGR[!duplicated(RGR$CM_FILENO),]

POE <- POE[POE$CM_CLIENT != "",]
POE <- POE[POE$CM_STATUS != "",]
POE <- POE[POE$CM_CURR_BAL != "",]
POE <- POE[POE$CM_DESK != "",]
POE <- POE[POE$CM_FILENO != "",]
POE <- POE[POE$NAME1_EMP_NAME != "",]


write.csv(ARMAST,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/ARMAST.csv",row.names=F)
write.csv(POE,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/POE_.csv",row.names=F)
write.csv(POEPivot,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/POEPivot.csv",row.names=F)
write.csv(RGR,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/RGR_.csv",row.names=F)
write.csv(RGRPivot,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/RGRPivot.csv",row.names=F)
write.csv(SearchD,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/SearchD.csv",row.names=F)
write.csv(Vendor,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/Vendor_.csv",row.names=F)
write.csv(VendorPivot,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/VendorPivot.csv",row.names=F)
write.csv(rgrsweep,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/rgrsweep.csv",row.names=F)
write.csv(vendorsweep,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/vendorsweep.csv",row.names=F)

write.csv(lo,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/LowManualSweep.csv",row.names=F)

