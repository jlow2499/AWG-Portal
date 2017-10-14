library(dplyr)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyr)
library(data.table)

Vendor <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Vendor.csv", stringsAsFactors=FALSE)
RGR <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/RGR.csv", stringsAsFactors=FALSE)
POE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/POE.csv", stringsAsFactors=FALSE)

Vendor <- Vendor %>% select(-CM_CLI_PLC_DATE)

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
  select(TFILE,Contacts,Home_Attempts,Time_Of_Day,HA_Multiple_Days_of_Week,POE_Attempts,POE_Messages,Location1,Location2,Location3,Location4,Location5,Location6,Location7,Location8,Location9)





finale <- left_join(knoxtalx, finaldata,by="TFILE")

rm(CODE);rm(DATE);rm(contact_data);rm(contact_df);rm(data); rm(df); rm(fridaymorning)
rm(fridayafternoon);rm(homedata);rm(knoxtalx);rm(location1);rm(location2);rm(location3)
rm(location4);rm(location5);rm(location6);rm(location7);rm(location8);rm(location9); rm(mondayafternoon)
rm(mondaymorning);rm(poedata); rm(poedf); rm(poemsg); rm(saturdaymorning); rm(thursdayafternoon)
rm(thursdaymorning); rm(time); rm(tuesdayafternoon); rm(tuesdaymorning); rm(wednesdayafternoon); rm(wednesdaymorning)
rm(Friday); rm(Monday); rm(Tuesday); rm(Wednesday); rm(Thursday); rm(Saturday); rm(accounts)
rm(codes); rm(home); rm(poe); rm(poe1); rm(r); rm(s)

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
rm(accounts)
LETTERS$Debtor <- as.character(LETTERS$Debtor)
LETTERS <- rename(LETTERS,TFILE=Debtor)
finaldata <- left_join(finaldata,LETTERS,by='TFILE')
rm(LETTERS)
#finaldata <- rename(finaldata,Home_Attempts=Num_of_Home_Attempts)

finaldata <- select(finaldata,TFILE,Home_Attempts,Contacts,POE_Attempts,POE_Messages,Location1,Location2,Location3,Location4,Location5,Location6,Location7,Location8,Location9,X30A.Date,X30W.Date,X30B.Date)

finaldata <- rename(finaldata,FileNumber = TFILE)
POE <- rename(POE,FileNumber=CM_FILENO)
RGR <- rename(RGR,FileNumber=CM_FILENO)
Vendor <- rename(Vendor,FileNumber=File)
Vendor$FileNumber <- as.character(Vendor$FileNumber)

POE <- left_join(POE,finaldata,by="FileNumber")
Vendor <- left_join(Vendor,finaldata,by="FileNumber")
RGR <- left_join(RGR,finaldata,by="FileNumber")

POE <- POE[!duplicated(POE$FileNumber),]
Vendor <- Vendor[!duplicated(Vendor$FileNumber),]
RGR <- RGR[!duplicated(RGR$FileNumber),]

rm(finaldata)

POE <- select(POE, -ED_NOTICE_TO_BORR_DT,-ED_ORDER_TO_EMP_DT,-ED_ORDER_TO_EMP_DT2,-CM_STATUS_CHANGE_DATE,-ED_ACCT_ACT_DATE)

POE <- POE %>%
  mutate('30A' = ifelse(ED_30A_DATE=='' & (X30A.Date != '' | !is.na(X30A.Date)),X30A.Date,
                        ifelse(ED_30A_DATE != '',ED_30A_DATE,''
                          )),
         '30B' = ifelse(ED_30B_DATE=='' & (X30B.Date != '' | !is.na(X30B.Date)),X30B.Date,
                        ifelse(ED_30B_DATE != '',ED_30B_DATE,''
                        )),
         '30W' = ifelse(ED_30W_DATE=='' & (X30W.Date != '' | !is.na(X30W.Date)),X30W.Date,
                        ifelse(ED_30W_DATE != '',ED_30W_DATE,''
                        ))
         )

POE$'30A' <- as.Date(POE$'30A',"%m/%d/%Y")
POE$'30B' <- as.Date(POE$'30B',"%m/%d/%Y")
POE$'30W' <- as.Date(POE$'30W',"%m/%d/%Y")

POE <- POE %>% select(-ED_30A_DATE,-ED_30B_DATE,-ED_30W_DATE,-X30A.Date,-X30B.Date,-X30W.Date)
POE <- POE[!POE$ED_CUR_COND %in% c('BIF','COMP','CONS','CSCH','DEATH','DIS','FCER','LIT','PPA','REHAB3'),]
POE <- POE %>% select(-ED_CUR_COND)
POE$Worked <- FALSE
POE <- POE[,c(29,1:2,11,3:10,12:28)]
POE$ED_LAST_VOL_PAY_AMT[is.na(POE$ED_LAST_VOL_PAY_AMT)] <- 0

Vendor <- select(Vendor, -ED_ORDER_TO_EMP_DT,-ED_ACCT_ACT_DATE)

Vendor <- Vendor %>%
  mutate('30A' = ifelse(ED_30A_DATE=='' & (X30A.Date != '' | !is.na(X30A.Date)),X30A.Date,
                        ifelse(ED_30A_DATE != '',ED_30A_DATE,''
                        )),
         '30B' = ifelse(ED_30B_DATE=='' & (X30B.Date != '' | !is.na(X30B.Date)),X30B.Date,
                        ifelse(ED_30B_DATE != '',ED_30B_DATE,''
                        )),
         '30W' = ifelse(ED_30W_DATE=='' & (X30W.Date != '' | !is.na(X30W.Date)),X30W.Date,
                        ifelse(ED_30W_DATE != '',ED_30W_DATE,''
                        ))
  )

Vendor$'30A' <- as.Date(Vendor$'30A',"%m/%d/%Y")
Vendor$'30B' <- as.Date(Vendor$'30B',"%m/%d/%Y")
Vendor$'30W' <- as.Date(Vendor$'30W',"%m/%d/%Y")

Vendor <- Vendor %>% select(-ED_30A_DATE,-ED_30B_DATE,-ED_30W_DATE,-X30A.Date,-X30B.Date,-X30W.Date)
 

####




Vendor$ED_REJD_DATE <- as.Date(Vendor$ED_REJD_DATE,"%m/%d/%Y")
Vendor$ED_LAST_VOL_PAY_DATE <- as.Date(Vendor$ED_LAST_VOL_PAY_DATE,"%m/%d/%Y")
Vendor$Batch.Date <- as.Date(Vendor$Batch.Date,"%m/%d/%Y")
Vendor$CM_LAST_WORK_DATE <- as.Date(Vendor$CM_LAST_WORK_DATE,"%m/%d/%Y")


Vendor <- Vendor %>% select(-Contacts)

Contacts <- read.csv("//knx3it/AWG Management/RGR/Activation Needs Dashboard/Data/Contacts.csv", stringsAsFactors=FALSE)

VCON <- rename(Contacts,FileNumber=CM_FILENO)
VCON$FileNumber <- as.character(VCON$FileNumber)
Vendor <- left_join(Vendor,VCON,by="FileNumber")
Vendor$Contacts[is.na(Vendor$Contacts)] <- 0

Vendor$ED_LAST_VOL_PAY_DATE[is.na(Vendor$ED_LAST_VOL_PAY_DATE)] <- "2000-01-01"
Vendor$ED_REJD_DATE[is.na(Vendor$ED_REJD_DATE)] <- "2000-01-01"

Vendor <- Vendor %>%
  mutate(Not_Worked_in_72 = ifelse((Sys.Date()-CM_LAST_WORK_DATE) >= 3,"No","Yes"),
         #    Not_Worked_in_72 = ifelse(is.null(Not_Worked_in_72),"Yes",Not_Worked_in_72),
         Payer = ifelse(ED_CUR_COND == "COMP" | ED_CUR_COND == "REHAB" | ED_CUR_COND == "REHAB2" | ED_CUR_COND == "REHAB3" | ED_CUR_COND == "PPA",
                        "Yes","No"),
         Payer = ifelse(CM_STATUS == "FRW" | CM_STATUS == "FPW" | CM_STATUS == "FSW" | CM_STATUS == "AAP" | CM_STATUS == "ATY" | CM_STATUS == "DSP" | CM_STATUS == "BKY","Yes",Payer),
         Reject = ifelse(ED_REJD_DATE >= Sys.Date()-180,"Yes","No"),
         Letters = ifelse(('30A' != "" & '30B' != "" & '30W' != ""),"Yes","No"),
         Good_Desk = ifelse(CM_DESK >= 600 | CM_DESK == 80 | CM_DESK == 1,"No","Yes"),
         Good_Desk = ifelse(CM_DESK == 613 | CM_DESK == 680 | CM_DESK == 681 | CM_DESK == 682 | CM_DESK == 683 | CM_DESK == 684 | CM_DESK == 991 | CM_DESK == 997,"Yes",Good_Desk),
         POE_Location_Attempts = Location1+Location2+Location3+Location4+Location5+Location6+Location7+Location8+Location9,
         Payment = ifelse((Sys.Date()-45)<=ED_LAST_VOL_PAY_DATE,"Yes","No"),
         Activations_Req = ifelse((Contacts >=1 & Letters == "Yes" & Good_Desk == "Yes" & Reject == "No" & Payer == "No" & Payment == "No" & (Batch.Date >= Sys.Date()-30)) | (Home_Attempts == "Yes" & (POE_Attempts >=2 | POE_Location_Attempts >= 7) & Letters == "Yes" & Good_Desk == "Yes" & Reject == "No" & Payer == "No" & Payment == "No"&(Batch.Date >= Sys.Date()-30)),
                                  "Yes","No"),
         Reason1 = ifelse(Payer =="Yes","Paying Condition",''),
         Reason2 = ifelse(Reject == "Yes", "Previously Rejected",''),
         Reason3 = ifelse(Letters == "No", "Needs Letters Sent",''),
         Reason4 = ifelse(Good_Desk == "No", "Unsweepable Desks",''),
         Reason5 = ifelse(Payment == "Yes", "Voluntary Payment in Last 45 Days",''),
         Reason6 = ifelse(Home_Attempts < 5, "Needs Home Attempts",''),
         Reason7 = ifelse((POE_Attempts <2 & POE_Location_Attempts <7), "Needs POE Location Attempts",''),
         Reason8 = ifelse(POE_Attempts <2, "Needs POE Attempts",''),
         Reason9 = ifelse(Activations_Req == "Yes","Sweepable",""),
         Reason = paste(Reason1,',',Reason2,',',Reason3,',',Reason4,',',Reason5,',',Reason6,',',Reason7,',',Reason8,',',Reason9))

Vendor <- select(Vendor,-Reason1,-Reason2,-Reason3,-Reason4,-Reason5,-Reason6,-Reason7,-Reason8,-Reason9)


Vendor$Reason <- gsub(x=Vendor$Reason,pattern='  ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=',,',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ,',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='\\s+$, ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=' , +$',replacement='')


#Vendor <- Vendor[!Vendor$ED_CUR_COND %in% c('BIF','COMP','CONS','CSCH','DEATH','DIS','FCER','LIT','PPA','REHAB3'),]
#Vendor <- Vendor %>% select(-ED_CUR_COND)
Vendor$Worked <- FALSE

Vendor <- select(Vendor, -Not_Worked_in_72,-Payer,-Reject,-Letters,-Good_Desk,-Payment)


#Vendor <- Vendor[,c(39,38,1:37)]

Vendor <- Vendor %>% select(-CM_FILENO,-ED_AWG_OPEN_DATE,-ED_LAST_GARN_PMT_AMT,-ED_LAST_GARN_PMT_DT,-CM_HOME_CONTACTS,-CM_POE_CONTACTS,-ED_LAST_VOL_PAY_DATE)


Vendor$FileNumber <- as.numeric(Vendor$FileNumber)


Vendor <- Vendor[Vendor$Batch.Date >= (Sys.Date()-180),]
Vendor <- Vendor[Vendor$CM_DESK < 600 | Vendor$CM_DESK >=996, ]
Vendor <- Vendor[Vendor$CM_DESK != 80, ]


Vendor <- Vendor[,c(33,1:32,34)]

Vendor <- Vendor[Vendor$ED_REJD_DATE <= (Sys.Date()-90) ,]

log <- c(0)
Vendor$Reason <- gsub(x=Vendor$Reason,pattern="NA ,",replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern="NA",replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='^ ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='NA,',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='NA,',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern='\\s+$, ',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=' , +$',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=' , +$',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=' , +$',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=' +$',replacement='')
Vendor$Reason <- gsub(x=Vendor$Reason,pattern=' +$',replacement='')


Vendor$Reason <- as.factor(Vendor$Reason)
choices <- levels(Vendor$Reason)

Vendor <- Vendor[!Vendor$ED_CUR_COND %in% c('REHAB','REHAB2','REHAB3','COMP','BIF','FCER','BNKR','CSCH','PPA','FAW'),]

saveRDS(log,"//knx1fs01/ED Reporting/AWG Matrix/Data/log.rds")
write.csv(choices,'//knx1fs01/ED Reporting/AWG Matrix/Data/choices.csv',row.names=F)
write.csv(Vendor,paste0('//Knx3it/edopsmgmt/AWG Vendor Requirement Matrix/VendorMatrix_',Sys.Date(),'.csv'),row.names=F)
write.csv(Vendor,'//knx1fs01/ED Reporting/AWG Matrix/Data/Vendor.csv',row.names=F)
write.csv(POE,paste0('//knx1fs01/ED Reporting/AWG Matrix/Data/POE_',Sys.Date(),'.csv'),row.names=F)

setwd("//knx1fs01/ED Reporting/AWG Matrix/App")


