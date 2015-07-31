# author: Steve Harris
# date: 2015-07-30
# subject: Obstetrics and theatre data linkage

# Readme
# ======
# Working file but flag decisions that will need to be made by obs team
# - Removed all but 3 original? sheets from excel
# - then save all the original excel sheets as CSV files
#
# 1. Run an exact match using MRN
# 2. Run a match using names blocked first by initials

# Todo
# ====
# TODO: 2015-07-31 - [ ] try 'lcs' longest common substring method of
# string dist for managing inverted names
# TODO: 2015-07-31 - [ ] @Perv: what to do with duplicates


# Log
# ===
# 2015-07-30
# - file created



rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/p-academic/collab-obs-uclh/'
setwd(paste0(PROJECT_PATH, 'src'))

library(RecordLinkage)
library(stringdist)
library(data.table)
library(stringr)

# Load anaesthetic database
# -------------------------
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
rdt.a <- data.table(rdf.a)
rdt.a[, id.a := .I] # unique key based on excel row number

tdt.a <- rdt.a[, .(id.a, MRN=MRN, namelast=surname, namefirst=forename)]
# NOTE: 2015-07-31 - [ ] lots of reversed names
tdt.a[,.(namelast,namefirst)][1:20]

# Convert all fields to UTF-8, lower case, and truncate
tdt.a[, MRN  := iconv(MRN, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.a[, MRN  :=substr(tolower(MRN),1,10)]
tdt.a[, namelast  := iconv(namelast, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.a[, namelast  :=substr(tolower(namelast),1,10)]
tdt.a[, namefirst := iconv(namefirst, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.a[, namefirst :=substr(tolower(namefirst),1,10)]
# Add an initials field (for blocking later)
tdt.a[, initials := paste0(substr(namefirst,1,1), substr(namelast,1,1)), by=id.a]
str(tdt.a)

# Load theatre databases
# ----------------------
rdf.t1 <- read.csv(
        paste0(PROJECT_PATH, 'data/150701_obs-db-theatre13-15.csv'),
        strip.white=TRUE,
        stringsAsFactors=FALSE)
str(rdf.t1)

rdt.t1 <- data.table(rdf.t1)
rdt.t1[, id.t1 := .I] # unique key based on excel row number

tdt.t1 <- rdt.t1[, .( id.t1, MRN=ID.HELPER, namefull=Patient.Name) ]

# Convert all fields to UTF-8, lower case, and truncate
tdt.t1[, MRN  := iconv(MRN, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.t1[, MRN  :=substr(tolower(MRN),1,10)]

tdt.t1[, namefull  := tolower(iconv(namefull, 'WINDOWS-1252', 'UTF-8'))]
tdt.t1[, namelast  :=strsplit(namefull, ',')[[1]][1], by=id.t1]
tdt.t1[, namefirst :=strsplit(namefull, ',')[[1]][2], by=id.t1]
tdt.t1[, namefull := NULL]
tdt.t1[, id.t2 := NA]
str(tdt.t1)


rdf.t2 <- read.csv(
        paste0(PROJECT_PATH, 'data/150701_obs-db-theatre09-13.csv'),
        strip.white=TRUE,
        stringsAsFactors=FALSE)
str(rdf.t2)

rdt.t2 <- data.table(rdf.t2)
rdt.t2[, id.t2 := .I] # unique key based on excel row number

tdt.t2 <- rdt.t2[, .( id.t2, MRN=ID.helper, namefull=PatName) ]

# Convert all fields to UTF-8, lower case, and truncate
tdt.t2[, MRN  := iconv(MRN, 'WINDOWS-1252', 'UTF-8')] # convert windows char codes
tdt.t2[, MRN  :=substr(tolower(MRN),1,10)]

tdt.t2[, namefull  := tolower(iconv(namefull, 'WINDOWS-1252', 'UTF-8'))]
tdt.t2[, namelast  :=str_trim(strsplit(namefull, ',')[[1]][1]), by=id.t2]
tdt.t2[, namefirst :=str_trim(strsplit(namefull, ',')[[1]][2]), by=id.t2]
tdt.t2[, namefull := NULL]
tdt.t2[, id.t1 := NA]
str(tdt.t2)

tdt.t <- rbind(tdt.t2, tdt.t1)
tdt.t[, id.t := .I]
# Initials for blocking
tdt.t[, initials := paste0(substr(namefirst,1,1), substr(namelast,1,1)), by=id.t]

# Start with exact merge based on hospital number
# -----------------------------------------------
# Convert empty strings to NA to avoid matching on empty string
tdt.a[, MRN:=ifelse(str_trim(MRN)=='',NA,MRN)]
tdt.t[, MRN:=ifelse(str_trim(MRN)=='',NA,MRN)]
tdt.a[, namelast:=ifelse(str_trim(namelast)=='',NA,namelast)]
tdt.t[, namelast:=ifelse(str_trim(namelast)=='',NA,namelast)]
tdt.a[, namefirst:=ifelse(str_trim(namefirst)=='',NA,namefirst)]
tdt.t[, namefirst:=ifelse(str_trim(namefirst)=='',NA,namefirst)]

# Anaesthetic list is longer therefore this becomes master
nrow(tdt.a)
nrow(tdt.t)

# mdt <- merge(tdt.a[!is.na(MRN)],tdt.t[!is.na(MRN)],by='MRN',all.x=T)
# NOTE: 2015-07-30 - [ ] this throws an error b/c there are duplicates
# - for now assume excel order is chronological and work with unique
setkey(tdt.a, MRN)
tdt.a.mrn <- unique(tdt.a)
nrow(tdt.a.mrn)
setkey(tdt.t, MRN)
tdt.t.mrn <- unique(tdt.t)
nrow(tdt.t.mrn)

# Drop NAs when merging
mdt.mrn <- merge(
        tdt.a.mrn[!is.na(MRN)],
        tdt.t.mrn[!is.na(MRN)],
        by='MRN', all.x=TRUE)
mdt.mrn[,link:='mrn.exact']
mdt.mrn[!is.na(id.t)]


# Now remove these merges from the data to see what remains to be matched
tdt.a <- merge(tdt.a, mdt.mrn[,.(id.a, id.t,link)], by='id.a', all.x=TRUE)
tdt.t <- merge(tdt.t, mdt.mrn[,.(id.a, id.t,link)], by='id.t', all.x=TRUE)

tdt.a[is.na(id.t)]
tdt.t[is.na(id.a)]
# DEBUGGING: 2015-07-31 - [ ] problem with some names not having both initials
tdt.a[nchar(initials)<2]
tdt.t[nchar(initials)<2]

# Record linkage step
# -------------------
require(RecordLinkage)

tdt.a.names <- tdt.a[is.na(id.t)]
setkey(tdt.a.names, namelast, namefirst)
tdt.a.names <- unique(tdt.a.names[,.(id.a,MRN,namelast,namefirst,initials,id.t=NA)])
tail(tdt.a.names,20)

tdt.t.names <- tdt.t[is.na(id.a)]
setkey(tdt.t.names, namelast, namefirst)
tdt.t.names <- unique(tdt.t.names[,.(id.t,MRN,namelast,namefirst,initials,id.a=NA)])
str(tdt.t.names)

rpairs <- compare.linkage(tdt.a.names, tdt.t.names,
    exclude=c('MRN','id.a', 'id.t'), blockfld=c(5))
rpairs <- epiWeights(rpairs) # calculate weights
str(rpairs)
summary(rpairs)
summary(rpairs$Wdata)
head(getPairs(rpairs, max.weight=0.99, min.weight=0.4),20) # run after weights

# Now classify
rlink <- epiClassify(rpairs, 0.9, 0.6)
lpairs <- rlink$pairs
link.yes <- lpairs[rlink$prediction=='L',]
link.poss <- lpairs[rlink$prediction=='P',]

# Now subset
mdt.names.yes <- cbind(tdt.a.names[link.yes$id1,.(id.a)], tdt.t.names[link.yes$id2,.(id.t,link='name.yes')])
mdt.names.poss <- cbind(tdt.a.names[link.poss$id1,.(id.a)], tdt.t.names[link.poss$id2,.(id.t,link='name.poss')])
mdt.names <- rbind(mdt.names.yes, mdt.names.poss)
mdt.names

# Now remove duplicates
# TODO: 2015-07-31 - [ ] should do this by weight from match, just do in order for now
setkey(mdt.names, id.a)
mdt.names <- unique(mdt.names)
mdt.mrn <- mdt.mrn[!is.na(id.a) & !is.na(id.t), .(id.a, id.t, link)]

mdt <- rbind(mdt.mrn, mdt.names)
str(mdt)
str(tdt.a)
tdt.a[,initials:=NULL]
tdt.a[,id.t:=NULL]
tdt.a[,link:=NULL]
tdt.a <- merge( tdt.a, mdt, by='id.a', all.x=TRUE )
str(tdt.a)
str(tdt.t)
tdt.a <- merge(tdt.a,tdt.t,by='id.t',
    all.x=TRUE, suffixes=c('.a','.t'))

tdt.a[,link.t:=NULL]
setnames(tdt.a,'link.a','link')
tdt.a[,id.a.t:=NULL]
setnames(tdt.a,'id.a.a','id.a')

str(tdt.a)
head(tdt.a[link=='mrn.exact'])
head(tdt.a[link=='name.yes'])
head(tdt.a[link=='name.poss'],20)

# Now generate a summary match quality indicator
require(stringdist)
tdt.a[, pid.all.a := paste0(MRN.a,namelast.a,namefirst.a)]
tdt.a[, pid.all.t := paste0(MRN.t,namelast.t,namefirst.t)]
tdt.a[, pid.all.dist := stringdist(pid.all.a, pid.all.t, method='osa')]
summary(tdt.a$pid.all.dist)

# Inspect with distance metrics
head(tdt.a[link =='mrn.exact' & pid.all.dist!=0,
    .(id.a, id.t, MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)])
head(tdt.a[link =='name.yes' & pid.all.dist!=0,
    .(id.a, id.t,  MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)])
head(tdt.a[link =='name.poss' & pid.all.dist<5,
    .(id.a, id.t, MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)],20)

require(gmodels)
str(tdt.a)
with(tdt.a, CrossTable(link, missing.include=T))