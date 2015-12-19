# author: Steve Harris
# date: 2015-12-15
# subject: load and clean anaesthetic database

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2015-12-15
# - file created by copying code from link_obs.r

rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh/'
setwd(paste0(PROJECT_PATH, 'src'))

library(assertthat)
library(Hmisc)
library(data.table)
library(gmodels)
library(stringr)

#  =============================
#  = Load anaesthetic database =
#  =============================
rdf.a <- read.csv(
        paste0(PROJECT_PATH, 'data/150701_obs-db-anaes.csv'),
        strip.white=TRUE,
        stringsAsFactors=FALSE)
str(rdf.a)

# Everything loaded as character vectors
# Potential match fields
# - forename aka namelast
# - surname aka namefirst
# - MRN
# - date of anaesthetic or surgery

# Now standardise these (leaving date for now)
# --------------------------------------------
nrow(rdf.a)
rdt.a <- data.table(rdf.a)
rdt.a[, id.a := .I] # unique key based on excel row number
# CHANGED: 2015-11-25 - [ ] now duplicate where there is a secondary procedure
# then select theatre procedures for matching (i.e. drop mat req)
setcolorder(rdt.a,c(25,1:24))
names(rdt.a)

rdt.a[1:20,c(1:11,12), with=F]
rdt.a1 <- rdt.a[,c(1:11,12), with=F] # primary anaes
# flag where a secondary procedure occurs
rdt.a1[,procedure := 1]
rdt.a1[,secondary := ifelse(secondary.procedure=="" | secondary.procedure=="N/A",F,T)]
table(rdt.a1$secondary.procedure)
table(rdt.a1$secondary)
rdt.a1[,secondary.procedure:=NULL]
str(rdt.a1)
setnames(rdt.a1, 'primary.anaes.technique', 'anaesthetic')
setnames(rdt.a1, 'primary.ansthetist', 'anaesthetist1') # deliberate copy of misspell
setnames(rdt.a1, 'secondary.anaesthetist', 'anaesthetist2')
table(rdt.a1$procedure)
str(rdt.a1)

# extract second procs
names(rdt.a)
rdt.a[1:5,c(1:5,12:17), with=F]
rdt.a2 <- rdt.a[,c(1:5,12:17), with=F]
rdt.a2[,procedure := 2]
str(rdt.a2)
setnames(rdt.a2, 'indication.1', 'indication')
setnames(rdt.a2, 'secondary.procedure', 'anaesthetic')
setnames(rdt.a2, 'main.anasthetist', 'anaesthetist1') # deliberate copy of misspell
setnames(rdt.a2, 'secondary.anaesthetist.1', 'anaesthetist2')
setnames(rdt.a2, 'compication', 'complications')
setnames(rdt.a2, 'comments.1', 'comments')
# Now filter out missing
rdt.a2 <- rdt.a2[anaesthetic!="" & anaesthetic!="N/A"]
table(rdt.a2$procedure)
str(rdt.a2)

rdt.a.original <- rdt.a
rdt.a <- rbind(rdt.a1, rdt.a2, fill=TRUE)
names(rdt.a)
setcolorder(rdt.a,c(1,12,2:11,13))
table(rdt.a$procedure)
table(rdt.a$secondary)
str(rdt.a)

#  =========================================
#  = Now identify those needing theatre =
#  =========================================

rdt.a[,c(1:6,8),with=FALSE]
table(rdt.a$indication)
grep('mat',"Maternal request", perl=TRUE, ignore.case=FALSE)
grep('mat.*? +req.*?',"Maternal request", perl=TRUE, ignore.case=TRUE)
# Use regexp to pick out maternal request, obstetric request and epidural
rdt.a$labour.epidural <- ifelse(
    gregexpr('m[a| ]t.*? +req.*?',rdt.a$indication, perl=TRUE, ignore.case=TRUE) >0 |
    gregexpr('^epidural.*',rdt.a$indication, perl=TRUE, ignore.case=TRUE) >0 |
    gregexpr('obs.*? +req.*?',rdt.a$indication, perl=TRUE, ignore.case=TRUE) > 0 
    ,1,0 )
# with(rdt.a, CrossTable(indication, labour.epidural, 
#     prop.r=F, prop.t=T, prop.c=F, prop.chisq=F))
rdt.a[,indication.theatre := ifelse(labour.epidural==0,T,F)]
table(rdt.a$indication.theatre)
str(rdt.a)

# Filter line (don't filter here)
tdt.a <- rdt.a
# tdt.a <- rdt.a[indication.theatre==T]

# Convert all fields to UTF-8, lower case, and truncate
tdt.a[, MRN  := iconv(MRN, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.a[, MRN  :=substr(tolower(MRN),1,10)]
setnames(tdt.a,'surname','namelast')
tdt.a[, namelast  := iconv(namelast, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.a[, namelast  :=substr(tolower(namelast),1,10)]
setnames(tdt.a,'forename','namefirst')
tdt.a[, namefirst := iconv(namefirst, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.a[, namefirst :=substr(tolower(namefirst),1,10)]

# CHANGED: 2015-07-31 - [ ] last initial only
# Add an initials field (for blocking later)
# tdt.a[, initials := paste0(substr(namefirst,1,1), substr(namelast,1,1)), by=id.a]
tdt.a[, initials := substr(namelast,1,1), by=id.a]
# Convert date to CCYY-MM-DD
tdt.a[,.(id.a,date)][1:20]
tdt.a[, anaesthetic.date := 
    as.Date(as.POSIXct(date,"%d/%m/%y", tz="GMT"))]
setnames(tdt.a, "MRN", "mrn")
str(tdt.a)


setnames(tdt.a, "anaesthetic", "anaesthetic.text")
tdt.a[, anaesthetic :=
	ifelse(grepl(".*c\\.?s\\.?e.*", anaesthetic, ignore.case=T, perl=T), "CSE",
	ifelse(grepl(".*top*", anaesthetic, ignore.case=T, perl=T), "Top-up",
	ifelse(grepl(".*epid*", anaesthetic, ignore.case=T, perl=T), "Epidural",
	ifelse(grepl(".*spin*", anaesthetic, ignore.case=T, perl=T), "Spinal",
	ifelse(grepl(".*g\\.?a.*", anaesthetic, ignore.case=T, perl=T), "Spinal",
		"Other" ))))) ]
with(tdt.a, CrossTable(anaesthetic, anaesthetic.text))

str(rdt.a1)
str(rdt.a2)

# TODO: 2015-11-25 - [ ] lots of reversed names
tdt.a[,.(namelast,namefirst)][1:20]

save(tdt.a, file="../data/anaesthesia.RData")
write.csv(tdt.a, "../data/wdt.anaes.csv" )
