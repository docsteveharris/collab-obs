# author: Steve Harris
# date: 2015-12-06
# subject: load and clean theatre data 

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2015-12-06
# - file created by copying code from link_obs.r
# 2016-01-18
# - extracts surgical procedure

rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh/'
setwd(paste0(PROJECT_PATH, 'src'))

library(assertthat)
library(Hmisc)
library(data.table)
library(gmodels)
library(stringr)
library(lubridate)

#  ==========================
#  = Load theatre databases =
#  ==========================
rdf.t1 <- read.csv(
        paste0(PROJECT_PATH, 'data/150701_obs-db-theatre13-15.csv'),
        strip.white=TRUE,
        stringsAsFactors=FALSE)
# Now work to extract 
str(rdf.t1)

rdt.t1 <- data.table(rdf.t1)
rdt.t1[, id.t1 := .I] # unique key based on excel row number

tdt.t1 <- rdt.t1[, .(
    id.t1,
    MRN=ID.HELPER,
    namefull=Patient.Name,
    # Anaesthetic_start_time, # commented out, used for inspection
    theatre.date=
        as.Date(
            as.POSIXct(Anaesthetic_start_time,
            format="%d/%m/%Y %H:%M", tz="GMT")),
    theatre.hour=
        hour(
            as.POSIXct(Anaesthetic_start_time,
            format="%d/%m/%Y %H:%M", tz="GMT")),
    priority =
        ifelse(grepl("elect.*", Operation.Priority, ignore.case=T, perl=T), "elective",
        ifelse(grepl("schedul*", Operation.Priority, ignore.case=T, perl=T), "scheduled",
        ifelse(grepl("urgent*", Operation.Priority, ignore.case=T, perl=T), "urgent",
        ifelse(grepl("immed*", Operation.Priority, ignore.case=T, perl=T), "emergent",NA)))),
    anaesthesia.time = difftime(
            as.POSIXct(Procedure_start_time, format="%d/%m/%Y %H:%M", tz="GMT"),
            as.POSIXct(Anaesthetic_start_time, format="%d/%m/%Y %H:%M", tz="GMT"),
            units="mins"),
    surgical.time = difftime(
            as.POSIXct(Procedure_end_time, format="%d/%m/%Y %H:%M", tz="GMT"),
            as.POSIXct(Procedure_start_time, format="%d/%m/%Y %H:%M", tz="GMT"),
            units="mins"),
    surgical.proc = NA
    )
    ]
# truncate anaesthesia delay at 2 hours
tdt.t1[, anaesthesia.time:=ifelse(anaesthesia.time>120 | anaesthesia.time < 0,NA,anaesthesia.time)]
# truncate surgical time at 4 hours
tdt.t1[, surgical.time:=ifelse(surgical.time>240 | surgical.time < 0,NA,surgical.time)]
# Convert all fields to UTF-8, lower case, and truncate
tdt.t1[, MRN  := iconv(MRN, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.t1[, MRN  :=substr(tolower(MRN),1,10)]
tdt.t1[, namefull  := tolower(iconv(namefull, 'WINDOWS-1252', 'UTF-8'))]
tdt.t1[, namelast  :=strsplit(namefull, ',')[[1]][1], by=id.t1]
tdt.t1[, namefirst :=strsplit(namefull, ',')[[1]][2], by=id.t1]
tdt.t1[, namefull := NULL]
tdt.t1[, id.t2 := NA]
describe(tdt.t1$anaesthesia.time)
describe(tdt.t1$surgical.time)
str(tdt.t1)


# Now start work on 2nd theatre database
rdf.t2 <- read.csv(
        paste0(PROJECT_PATH, 'data/150701_obs-db-theatre09-13.csv'),
        strip.white=TRUE,
        stringsAsFactors=FALSE)
str(rdf.t2)

rdt.t2 <- data.table(rdf.t2)
rdt.t2[, id.t2 := .I] # unique key based on excel row number

tdt.t2 <- rdt.t2[, .(
    id.t2,
    MRN=ID.helper,
    namefull=PatName,
    # cr_prdate, # used to inspect and check conversion
    theatre.date =
        as.Date(
            as.POSIXct(cr_prdate,
            format="%d/%m/%y", tz="GMT")),
    theatre.hour = as.numeric(substr(time_anesthesia_start,1,2)),
    # ncepod priority
    priority = 
        ifelse(grepl("elect.*", ncepod, ignore.case=T, perl=T), "elective",
        ifelse(grepl("schedul*", ncepod, ignore.case=T, perl=T), "scheduled",
        ifelse(grepl("urgent*", ncepod, ignore.case=T, perl=T), "urgent",
        ifelse(grepl("emerg*", ncepod, ignore.case=T, perl=T), "emergent",NA)))),
    anaesthesia.time = difftime(
        parse_date_time(time_actual_procedure_start, "HM"),
        parse_date_time(time_anesthesia_start, "HM"),
        units="mins") ,
    surgical.time = difftime(
        parse_date_time(time_actual_procedure_end, "HM"),
        parse_date_time(time_actual_procedure_start, "HM"),
        units="mins"),
    surgical.proc = PrimSurgProc1
    ) ]
# where crosses midnight then add 60*24 back
tdt.t2[, anaesthesia.time:=ifelse(anaesthesia.time<0,anaesthesia.time+60*24,anaesthesia.time)]
tdt.t2[, surgical.time:=ifelse(surgical.time<0,surgical.time+60*24,surgical.time)]
# truncate anaesthesia delay at 2 hours
tdt.t2[, anaesthesia.time:=ifelse(anaesthesia.time>120,NA,anaesthesia.time)]
# truncate surgical time at 2 hours
tdt.t2[, surgical.time:=ifelse(surgical.time>240,NA,surgical.time)]
# Convert all fields to UTF-8, lower case, and truncate
tdt.t2[, MRN  := iconv(MRN, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.t2[, MRN  :=substr(tolower(MRN),1,10)]

tdt.t2[, namefull  := tolower(iconv(namefull, 'WINDOWS-1252', 'UTF-8'))]
tdt.t2[, namelast  :=str_trim(strsplit(namefull, ',')[[1]][1]), by=id.t2]
tdt.t2[, namefirst :=str_trim(strsplit(namefull, ',')[[1]][2]), by=id.t2]
tdt.t2[, namefull := NULL]
tdt.t2[, id.t1 := NA]

describe(tdt.t2$anaesthesia.time)
describe(tdt.t2$surgical.time)
str(tdt.t2)

tdt.t <- rbind(tdt.t2, tdt.t1)
tdt.t[, id.t := .I]
str(tdt.t)
names(tdt.t)
setcolorder(tdt.t, 
    c("id.t", "id.t1", "id.t2", names(tdt.t)[c(-1,-11,-12)]))


setnames(tdt.t,'MRN','mrn')

# Inspect data
str(tdt.t)
t <- data.table(table(tdt.t$surgical.proc))
setorder(t, -N)
head(t,20)
# Label up procedures that are sections
tdt.t[, lscs.theatre := ifelse(is.na(surgical.proc), NA, grepl(".*caesar*", surgical.proc, ignore.case=T, perl=T))]
str(tdt.t)
require(mosaic)
tally(tdt.t$lscs.theatre)

require(lubridate)
table(year(tdt.t1$theatre.date))
table(year(tdt.t2$theatre.date))

save(tdt.t, file='../data/theatre.RData')
write.csv(tdt.t, "../data/wdt.theatre.csv" )
