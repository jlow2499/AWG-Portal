library(dplyr)
library(stringr)
Today <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/DailyJoin/Today.csv", stringsAsFactors=FALSE)

Today$Act.TMS <- as.POSIXct(Today$Act.TMS,'%m/%d/%Y %I:%M:%S %p',tz='EST')
Today$Act.TMS <- format(Today$Act.TMS,'%m/%d/%Y %H:%M:%S')

a <- as.data.frame(str_split_fixed(Today$Act.TMS,pattern = ' ', n=2))
names(a) <- c('ACT_DATE','TIME')

Today <- cbind(Today,a)

rm(a)

Today <- rename(Today,ARNUM = Collector, TFILE = Debtor, CODE_1 = Act.What, CODE_2 = Act.Who,
                CODE_3 = Act.Result,EMPNUM = Empl.No)
Today <- select(Today,-Act.TMS)
Today <- select(Today,ARNUM,TFILE,ACT_DATE,TIME,CODE_1,CODE_2,CODE_3,EMPNUM)
Today$TFILE <- as.character(Today$TFILE)
Today$ACT_DATE <- as.character(Today$ACT_DATE)
Today$TIME <- as.character(Today$TIME)
Today <- Today[Today$ACT_DATE != "", ]

ACT_DB <- readRDS('C:/ACTDB/ACT_DB.rds')
ACT_DB <- rbind(ACT_DB,Today)
ACT_DB$ACT_DATE <- as.Date(ACT_DB$ACT_DATE,'%m/%d/%Y')
ACT_DB <- ACT_DB %>%
  arrange(ACT_DATE)
ACT_DB$ACT_DATE <- format(ACT_DB$ACT_DATE, '%m/%d/%Y')

ACT_DB$TIME[grepl(pattern='[[:alpha:]]',x=ACT_DB$TIME)] <- 'CORRUPT'
CORRUPT <- ACT_DB[ACT_DB$TIME %in% 'CORRUPT',]
ACT_DB <- ACT_DB[!ACT_DB$TIME %in% 'CORRUPT',]

Start <- as.Date('01-01-2015','%m-%d-%Y')
Yesterday <- Sys.Date()-1
Vector <- c(Start:Yesterday)
Dates <- as.data.frame(seq.Date(from=Start,to=Yesterday,length.out=length(Vector)))
names(Dates) <- 'Sequence'

DBDATES <- ACT_DB %>%
  group_by(ACT_DATE) %>%
  summarize(n = n())
DBDATES$ACT_DATE <- as.Date(DBDATES$ACT_DATE,'%m/%d/%Y')
DBDATES <- rename(DBDATES,Sequence=ACT_DATE)
Dates <- left_join(Dates,DBDATES,by='Sequence')
Dates <- Dates[is.na(Dates$n),]
rm(DBDATES);rm(Today);rm(Start);rm(Vector);rm(Yesterday)

saveRDS(CORRUPT,'//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/ActHistDatabase/CORRUPT.rds')
saveRDS(Dates,'//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/ActHistDatabase/NULLDATES.rds')
saveRDS(ACT_DB,'C:/ACTDB/ACT_DB.rds')
saveRDS(ACT_DB,'//knx1fs01/ED Reporting/ACTDB Backup 1/ACT_DB.rds')
saveRDS(ACT_DB,'//Knx3fs01/ED_BA_GROUP/Lowhorn/ACTDB Backup 2/ACT_DB.rds')




