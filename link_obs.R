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
# TODO: 2015-11-24 - [ ] new strategy where you merge without dropping duplicates
# - rpt for mrn, lname, fname etc
# - create a master table
# - calculate the string dist including one which allows for lcs for inverted names
# - now order by string dist and drop duplicates
# - merge this back against original



# Log
# ===
# 2015-07-30
# - file created
# 2015-11-24
# - updated for new path
# - rerun and rechecked
# 2015-11-25
# - splitting primary and secondary anaesthetic techniques
# - linkage routines re-written
# - theatre used as master list


rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh/'
setwd(paste0(PROJECT_PATH, 'src'))

library(RecordLinkage)
library(stringdist)
library(data.table)
library(stringr)
library(gmodels)


#  ==========================
#  = Load theatre databases =
#  ==========================
load('../data/theatre.RData')

#  =====================================================
#  = Load anaesthesia and filter out non-theatre cases =
#  =====================================================
load('../data/anaesthesia.RData')
tdt.a <- tdt.a[indication.theatre==T]


#  ===========================================
#  = Two data tables below ready for merging =
#  ===========================================
str(tdt.t)
str(tdt.a)

# Start with merge based on MRN and year-month
# -----------------------------------------------
# Convert empty strings to NA to avoid matching on empty string
tdt.a[, MRN:=ifelse(str_trim(MRN)=='',NA,MRN)]
tdt.a[, namelast:=ifelse(str_trim(namelast)=='',NA,namelast)]
tdt.a[, namefirst:=ifelse(str_trim(namefirst)=='',NA,namefirst)]
tdt.a[, date.match:=anaesthetic.date]
# Work with last 6 chars of MRN to deal with missing leading zeros
# Work with year-month for women having separate procedures
tdt.a[, key_1 := paste(
    substr(MRN,nchar(MRN)-5,nchar(MRN)),
    substr(date.match,1,7))]
tdt.a[,.(MRN,date.match,key_1,namefirst,namelast)]

tdt.t[, MRN:=ifelse(str_trim(MRN)=='',NA,MRN)]
tdt.t[, namelast:=ifelse(str_trim(namelast)=='',NA,namelast)]
tdt.t[, namefirst:=ifelse(str_trim(namefirst)=='',NA,namefirst)]
tdt.t[, date.match:=theatre.date]
tdt.t[, key_1 := paste(
    substr(MRN,nchar(MRN)-5,nchar(MRN)),
    substr(date.match,1,7))]
tdt.t[,.(MRN,date.match,key_1,namefirst,namelast)]

# Anaesthetic list is longer therefore this becomes master
str(tdt.a)
str(tdt.t)

#  ============================================
#  = END OF DATA PREPARATION - START OF MERGE =
#  ============================================

#  ==========================
#  = START MERGE SEQUENTIAL =
#  ==========================
# mdt <- merge(tdt.a[!is.na(MRN)],tdt.t[!is.na(MRN)],by='MRN',all.x=T)
# NOTE: 2015-07-30 - [ ] this throws an error b/c there are duplicates @discuss
# - for now assume excel order is chronological and work with unique

# Working notes
# - merge will need to be on hospital number and date
# - hence split date into d/m/y fields and then with MRN make this the primary key
#   for first merge
# - b/c time is missing then match on date but after sorting into time
#   order and by anaesthetic database order

nrow(tdt.a)
setkey(tdt.a, key_1)
tdt.a.key_1 <- unique(tdt.a) # drop around 2000 rows
nrow(tdt.a.key_1)
setkey(tdt.t, key_1)
nrow(tdt.t)
tdt.t.key_1 <- unique(tdt.t) # drop around 2000 rows
nrow(tdt.t.key_1)

# Theatre list is longer so this becomes master
nrow(tdt.a.key_1)
nrow(tdt.t.key_1)

# Drop NAs when merging
mdt.key_1 <- merge(
        tdt.t.key_1[!is.na(key_1)],
        tdt.a.key_1[!is.na(key_1)],
        by='key_1')
mdt.key_1[,link:='key_1.exact']
# Inspect quickly the merge
mdt.key_1[,.(id.a,id.t,MRN.x,MRN.y,date.match.x,date.match.y,
    namefirst.x,namefirst.y,
    namelast.x,namelast.y
    )]
mdt.key_1 <- mdt.key_1[,.(id.a, id.t, link)]
str(mdt.key_1)

#  =========================
#  = Key 2 - date and name =
#  =========================
# - use date and first three letters of first and last name for exact match

# Now remove these merges from the data to see what remains to be matched
tdt.a.names <- merge(tdt.a, mdt.key_1[,.(id.a, id.t,link)], by='id.a', all.x=TRUE)
tdt.a.names <- tdt.a.names[is.na(id.t)]
tdt.a.names[, key_2 := paste(
    substr(namefirst,1,3),
    substr(namelast,1,3),
    substr(date.match,1,7))]
tdt.a.names[,.(id.a,MRN,date.match,key_2,namefirst,namelast)]
tdt.a.names <- tdt.a.names[,.(id.a,MRN,date.match,key_2,namefirst,namelast)]
# NOTE: 2015-11-25 - [ ] down to 2730 unmatched
nrow(tdt.a.names)

tdt.t.names <- merge(tdt.t, mdt.key_1[,.(id.a, id.t,link)], by='id.t', all.x=TRUE)
tdt.t.names <- tdt.t.names[is.na(id.a)]
tdt.t.names[, key_2 := paste(
    substr(namefirst,1,3),
    substr(namelast,1,3),
    substr(date.match,1,7))]
tdt.t.names <- tdt.t.names[,.(id.t,MRN,date.match,key_2,namefirst,namelast)]
# NOTE: 2015-11-25 - [ ] 4540 names unmatched
nrow(tdt.t.names)

# Merge step
mdt.key_2 <- merge(
        tdt.t.names[!is.na(key_2)],
        tdt.a.names[!is.na(key_2)],
        by='key_2')
mdt.key_2[,link:='key_2.exact']
nrow(mdt.key_2) # 698 extra merges
str(mdt.key_2)

# Inspect quickly the merge
mdt.key_2[,.(id.a,id.t,MRN.x,MRN.y,date.match.x,date.match.y,
    namefirst.x,namefirst.y,
    namelast.x,namelast.y
    )]
mdt.key_2 <- mdt.key_2[,.(id.a, id.t, link)]
str(mdt.key_2)

#  =======================================
#  = Record Linkage for remaining using 
#  =======================================
require(RecordLinkage)

# Drop those merged above
tdt.a.fuzzy <- merge(tdt.a.names, mdt.key_2[,.(id.a, id.t,link)], by='id.a', all.x=TRUE)
tdt.a.fuzzy <- tdt.a.fuzzy[is.na(id.t)]
tdt.a.fuzzy[, key_3 := paste(namefirst,namelast,date.match,MRN)]
tdt.a.fuzzy <- tdt.a.fuzzy[,.(id.a,MRN,date.match,key_3,namefirst,namelast)]
str(tdt.a.fuzzy)

tdt.t.fuzzy <- merge(tdt.t.names, mdt.key_2[,.(id.a, id.t,link)], by='id.t', all.x=TRUE)
tdt.t.fuzzy <- tdt.t.fuzzy[is.na(id.a)]
tdt.t.fuzzy[, key_3 := paste(namefirst,namelast,date.match,MRN)]
tdt.t.fuzzy <- tdt.t.fuzzy[,.(id.t,MRN,date.match,key_3,namefirst,namelast)]
str(tdt.t.fuzzy)

str(tdt.t.fuzzy)
str(tdt.a.fuzzy)

# Slowest function call
rpairs <- compare.linkage(tdt.a.fuzzy, tdt.t.fuzzy,
    strcmp=TRUE, strcmpfun=jarowinkler,
    exclude=c(1,2,3,5,6)
    # exclude=c('MRN','id.a', 'id.t', 'namefirst', 'namelast', 'date.match')
    # , blockfld=c(3) # block on date
    )

rpairs <- epiWeights(rpairs) # calculate weights
str(rpairs)
summary(rpairs)
summary(rpairs$Wdata)
# NOTE: 2015-07-31 - [ ] these commands run really slowly, only for inspection
# head(getPairs(rpairs, max.weight=0.99, min.weight=0.95),20) # run after weights # tail(getPairs(rpairs, max.weight=0.95, min.weight=0.9),20) # run after weights
# head(getPairs(rpairs, max.weight=0.89, min.weight=0.8),20) # run after weights
# head(getPairs(rpairs, max.weight=0.79, min.weight=0.7),20) # run after weights
# head(getPairs(rpairs, max.weight=0.69, min.weight=0.6),20) # run after weights

# Now classify
rlink <- epiClassify(rpairs, 0.95, 0.85)
lpairs <- rlink$pairs
link.yes <- lpairs[rlink$prediction=='L',]
link.poss <- lpairs[rlink$prediction=='P',]

# Now subset
mdt.fuzzy.yes <- cbind(
    tdt.a.fuzzy[link.yes$id1,.(id.a,key_3.a=key_3)],
    tdt.t.fuzzy[link.yes$id2,.(id.t,key_3.t=key_3,link='fuzzy.yes')])
mdt.fuzzy.poss <- cbind(
    tdt.a.fuzzy[link.poss$id1,.(id.a,key_3.a=key_3)], 
    tdt.t.fuzzy[link.poss$id2,.(id.t,key_3.t=key_3,link='fuzzy.poss')])
mdt.fuzzy <- rbind(mdt.fuzzy.yes, mdt.fuzzy.poss)

# Now remove duplicates but first sort by string dist
mdt.fuzzy[, key_3.dist := stringdist(key_3.a, key_3.t, method='jw')]
setorder(mdt.fuzzy,id.a,key_3.dist)
mdt.fuzzy
setkey(mdt.fuzzy, id.a)
mdt.fuzzy <- unique(mdt.fuzzy)
mdt.fuzzy
setorder(mdt.fuzzy,key_3.dist)
mdt.fuzzy
mdt.key_3 <- mdt.fuzzy

#  ===============================================
#  = Put together the three record linkage steps =
#  ===============================================
str(mdt.key_1[,.(id.a,id.t,link)])
str(mdt.key_2[,.(id.a,id.t,link)])
str(mdt.key_3[,.(id.a,id.t,link)])

mdt.key <- rbind(
    mdt.key_1[,.(id.a,id.t,link)],
    mdt.key_2[,.(id.a,id.t,link)],
    mdt.key_3[,.(id.a,id.t,link)])

mdt <- merge(mdt.key, tdt.a, by='id.a', all.x=T)
mdt <- merge(mdt, tdt.t, by='id.t', all.y=T)
str(mdt)

# Now generate a summary match quality indicator
str(mdt)
mdt[, pid.a := paste(date.match.x,MRN.x,namelast.x,namefirst.x)]
mdt[, pid.t := paste(date.match.y,MRN.y,namelast.y,namefirst.y)]
mdt[, pid.dist := stringdist(pid.a, pid.t, method='osa')]
summary(mdt$pid.dist)
nrow(mdt)
mdt[, pid.dist.cat := cut2(pid.dist, c(0:3,5,10))]

CrossTable(mdt[!is.na(id.t) & labour.epidural==FALSE]$pid.dist.cat)
mdt[pid.dist %in% c(4,5), .(id.a,id.t,pid.a,pid.t)]

# Now generate a date proximity indicator
str(mdt)
mdt[, date.dist := abs(date.match.x - date.match.y)]
mdt[, date.dist.ok := ifelse(
    date.dist < 180         # any date within 6 months - allows for typos
    | as.numeric(date.dist)%%365 < 2    # allows year errors 
    , 1, 0 )]
describe(mdt$date.dist)
describe(mdt$date.dist.ok)

# Now define acceptable links
mdt[, link :=   ifelse(pid.dist <= 3 & date.dist.ok, 1,
                ifelse(pid.dist <=10 & date.dist.ok, 0, -1 ) ) ]

# So look at the linkage
CrossTable(mdt$link)
CrossTable(mdt[!is.na(id.t) & labour.epidural==FALSE]$link)

# Prepare final output using the theatre record as the master
str(mdt)
mdt.t <- mdt[!is.na(id.t), .(
    id.t,
    id.a,
    link,
    pid.dist,
    pid.t,
    pid.a,
    procedure,      # primary or secondary
    anaesthetic,
    indication,
    anaesthetist1,
    anaesthetist2
    ) ]
mdt.t

# Most anaesthetic fields are included - need to go back to the original
# theatre data for the rest and merge on id.t1 and id.t2
str(tdt.t)
mdt.t <- merge(mdt.t, tdt.t[,.(id.t,id.t1,id.t2)], all.x=T)
mdt.t

str(tdt.t1)
str(tdt.t2)

#  ==================
#  = Write the file =
#  ==================
# - [ ] NOTE(2015-12-06): you need rdt.t1 and rdt.t2 here 
save(mdt.t, tdt.t, tdt.a, file="../data/theatre_linked.RData")
rite.csv(mdt.t, file="../data/theatre_linked.csv", row.names=FALSE)

