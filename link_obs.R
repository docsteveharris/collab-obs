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
# 2015-11-24
# - updated for new path
# - rerun and rechecked


rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh/'
setwd(paste0(PROJECT_PATH, 'src'))

library(RecordLinkage)
library(stringdist)
library(data.table)
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

# CHANGED: 2015-07-31 - [ ] last initial only
# Add an initials field (for blocking later)
# tdt.a[, initials := paste0(substr(namefirst,1,1), substr(namelast,1,1)), by=id.a]
tdt.a[, initials := substr(namelast,1,1), by=id.a]
tdt.a[,][1:20]
str(tdt.a)

#  ==========================
#  = Load theatre databases =
#  ==========================
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
# CHANGED: 2015-07-31 - [ ] last initial only
# tdt.t[, initials := paste0(substr(namefirst,1,1), substr(namelast,1,1)), by=id.t]
tdt.t[, initials := substr(namelast,1,1), by=id.t]

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
# NOTE: 2015-07-30 - [ ] this throws an error b/c there are duplicates @discuss
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
        by='MRN')
mdt.mrn[,link:='mrn.exact']
mdt.mrn <- mdt.mrn[,.(id.a, id.t, link)]
str(mdt.mrn)

# Record linkage step
# -------------------
require(RecordLinkage)

# Now remove these merges from the data to see what remains to be matched
tdt.a.names <- merge(tdt.a, mdt.mrn[,.(id.a, id.t,link)], by='id.a', all.x=TRUE)
tdt.a.names <- tdt.a.names[is.na(id.t)]
nrow(tdt.a.names)

setkey(tdt.a.names, namelast, namefirst)
tdt.a.names <- unique(tdt.a.names[,.(id.a,MRN,namelast,namefirst,initials,id.t=NA)])
tdt.a.names[1:20]
str(tdt.a.names)

tdt.t.names <- merge(tdt.t, mdt.mrn[,.(id.a, id.t,link)], by='id.t', all.x=TRUE)
tdt.t.names <- tdt.t.names[is.na(id.a)]
setkey(tdt.t.names, namelast, namefirst)
tdt.t.names <- unique(tdt.t.names[,.(id.t,MRN,namelast,namefirst,initials,id.a=NA)])
tdt.t.names[1:20]
str(tdt.t.names)

rpairs <- compare.linkage(tdt.a.names, tdt.t.names,
    strcmp=TRUE, strcmpfun=jarowinkler,
    exclude=c('MRN','id.a', 'id.t'), blockfld=c(5))
rpairs <- epiWeights(rpairs) # calculate weights
str(rpairs)
summary(rpairs)
summary(rpairs$Wdata)
# NOTE: 2015-07-31 - [ ] these commands run really slowly, only for inspection
head(getPairs(rpairs, max.weight=0.99, min.weight=0.95),20) # run after weights
tail(getPairs(rpairs, max.weight=0.95, min.weight=0.9),20) # run after weights
# head(getPairs(rpairs, max.weight=0.79, min.weight=0.7),20) # run after weights
# head(getPairs(rpairs, max.weight=0.79, min.weight=0.7),20) # run after weights
# head(getPairs(rpairs, max.weight=0.69, min.weight=0.6),20) # run after weights

# Now classify
rlink <- epiClassify(rpairs, 0.95, 0.9)
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
mdt.names
# str(mdt.mrn)
# mdt.mrn <- mdt.mrn[!is.na(id.a) & !is.na(id.t), .(id.a, id.t, link)]

# Identify remaining unmatched records
tdt.a.rev <- merge(tdt.a, rbind(mdt.mrn, mdt.names), by='id.a', all.x=TRUE)
table(tdt.a.rev$link)
tdt.a.rev <- tdt.a.rev[is.na(id.t), .(id.a, 
    namelast, namefirst,
    n4 = paste(nl4=substr(namelast,1,4), nf4=substr(namefirst,1,4)))]
tdt.a.rev <- tdt.a.rev[!is.na(namelast) & !is.na(namefirst)]
tdt.a.rev

x <- rbind(mdt.mrn, mdt.names)
setkey(x, id.t)
x <- unique(x)
tdt.t.rev <- merge(tdt.t, x , by='id.t', all.x=TRUE)
table(tdt.t.rev$link)
tdt.t.rev <- tdt.t.rev[is.na(id.a), .(id.t,
    namelast, namefirst,
    n4=paste(nl4=substr(namelast,1,4), nf4=substr(namefirst,1,4)))]
tdt.t.rev <- tdt.t.rev[!is.na(namelast) & !is.na(namefirst)]
tdt.t.rev

# So now try stringdist matrix using qgram which seems to cope well with reversal
# stringdist('steve harris', 'steve harris', method='lcs')
# stringdist('steve harri', 'steve harris', method='qgram')
# stringdist('steve harri', 'rebekah harris', method='qgram')
# stringdist('rebekah harris','steve harri',  method='qgram')
# stringdist('steve, harris', 'harris, steve', method='osa')
# stringdist('steve, harris', 'harris, steve', method='lcs')
# stringdist('steve, harris', 'harris, steve', method='qgram')

sdm <- stringdistmatrix(tdt.a.rev$n4, tdt.t.rev$n4, method='qgram')
sdm.orig <- sdm
# Use ids as row / col names
rownames(sdm) <- tdt.a.rev$id.a
colnames(sdm) <- tdt.t.rev$id.t
# DEBUGGING: 2015-07-27 - [ ] problem with NA?
# And using colRanks does not help
# Will need to drop NA matches - @done(2015-07-31): see above
# x <- sdm[1:10,1:10]
# x[1,1] <- NA
# x
# max.col(-x, ties.method='first')

mid <- max.col(-sdm, ties.method='first')
mid <- matrix(c(1:nrow(sdm),mid),ncol=2)
bestdis <- sdm[mid]
r <- data.table(as.numeric(rownames(sdm)), as.numeric(colnames(sdm)[mid[,2]]), bestdis )
setnames(r, c('id.a', 'id.t', 'dist'))
head(r,30)
table(r$dist) # 730 'perfect' matches?
str(r)
mdt.rev <- r[dist<=0, .(id.a, id.t, link='reverse')]
head(mdt.rev)

# Now merge back into the original data
# -------------------------------------
mdt <- rbind(mdt.mrn, mdt.names, mdt.rev)
str(mdt)
str(tdt.a)
tdt.a[,initials:=NULL]
tdt.a.result <- merge( tdt.a, mdt, by='id.a', all.x=TRUE )
str(tdt.a.result)
tdt.a.result <- merge(tdt.a.result,tdt.t,by='id.t',
    all.x=TRUE, suffixes=c('.a','.t'))
str(tdt.a.result)

head(tdt.a.result[link=='mrn.exact'])
nrow(tdt.a.result[link=='mrn.exact'])

head(tdt.a.result[link=='name.yes'])
nrow(tdt.a.result[link=='name.yes'])

head(tdt.a.result[link=='name.poss'],20)
nrow(tdt.a.result[link=='name.poss'])

head(tdt.a.result[link=='reverse'],20)
nrow(tdt.a.result[link=='reverse'])

# Now generate a summary match quality indicator
tdt.a.result[, pid.all.a := paste0(MRN.a,namelast.a,namefirst.a)]
tdt.a.result[, pid.all.t := paste0(MRN.t,namelast.t,namefirst.t)]
tdt.a.result[, pid.all.dist := stringdist(pid.all.a, pid.all.t, method='osa')]
summary(tdt.a.result$pid.all.dist)

# Cut and classify by distance
require(Hmisc)
nrow(tdt.a.result)
tdt.a.result[, pid.all.dist.cat := cut2(pid.all.dist, c(0:3,5,10))]
CrossTable(tdt.a.result$pid.all.dist.cat)

# Inspect with distance metrics
head(tdt.a.result[link =='mrn.exact' & pid.all.dist!=0,
    .(id.a, id.t, MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)])
head(tdt.a.result[link =='name.yes' & pid.all.dist!=0,
    .(id.a, id.t,  MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)])
head(tdt.a.result[link =='name.poss' & pid.all.dist<5,
    .(id.a, id.t, MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)],20)
head(tdt.a.result[link =='reverse',
    .(id.a, id.t, MRN.a, MRN.t, namelast.a, namelast.t, namefirst.a, namefirst.t, pid.all.dist,link)],20)

require(gmodels)
str(tdt.a.result)
with(tdt.a.result, CrossTable(link, missing.include=T))