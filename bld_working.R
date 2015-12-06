# author: Steve Harris
# date: 2015-12-03
# subject: Combine the various data sets into a single table

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2015-12-03
# - file created

rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh'
setwd(paste(PROJECT_PATH, 'src', sep='/'))

library(assertthat)
library(Hmisc)
library(data.table)
library(gmodels)

load('../data/census.RData')
tdt.census <- wdt
str(wdt)
# working data will be a unique key of births

# pkey         =	primary key derived from census sheets
# dtob         =	date and time of birth
# mrn          =	mother's MRN
wdt <- wdt[,.(pkey,dtob,mrn=tolower(mrn))]
nrow(wdt)
length(unique(wdt$pkey))==nrow(wdt) # check primary key unique
sum(duplicated(wdt$dtob)) # 74 rows with identical dtob (close to being a good key)
setkey(wdt,dtob,mrn) # really for the side effect of sorting
# Add id.birth
wdt[, id.birth := .I] # convenience ID sorted by dtob

# Add a birth counter per mother (so you know how many times she appears in the data)
setkey(wdt,mrn) # really for the side effect of sorting
wdt <- wdt[wdt[,.(id.mother=.GRP),by=mrn]]
wdt.sofarsogood <- wdt

setkey(wdt,id.mother)
# Suprisingly slow command
tdt <- wdt[, .SD[,.(id.birth, birth.seq = .I)], by=id.mother, .SDcols=c('id.birth')]
setkey(tdt,id.birth)
setkey(wdt,id.birth)
wdt <- wdt[tdt]
setkey(wdt,id.mother)
describe(wdt$birth.seq)
wdt[, dob:=as.Date(dtob)]
str(wdt)


# Future merges to maternal data will be based on MRN and dob
# So take an example data set that contains MRN's and dates and write a function for merging
# Incoming data will contain
# - MRN
# - date (you may want to define if you pick closest overall, closest previous, closest past)
# - a unique key of its own

stop()




load('../data/theatre_linked.RData')
setnames(tdt.t,'MRN','mrn')
setnames(tdt.a,'MRN','mrn')
ls()

# Merge theatre data onto census data
# -----------------------------------
# - [ ] NOTE(2015-12-04): goes from 19.8k census obs and finds matches
#   in 13k of 15k theatre cases; does this mean that the other theatre
#   cases are not related to live births??
str(tdt.census)
length(unique(tdt.census$pkey))
k.census <- tdt.census[,.(pkey, mrn, dtob)]
setkey(k.census,mrn)
setorder(k.census,dtob,mrn)
str(k.census)
k.t <- tdt.t[,.(id.t, mrn, date.match)]
setkey(k.t,mrn)
str(k.t)
m1 <- merge(k.census, k.t, by=c('mrn'), all.x=TRUE, suffixes=c('census','t'))
str(m1)
length(unique(m1$pkey)) 
# - [ ] NOTE(2015-12-04): duplicate rows: either b/c match further
#   admissions surgical trips (incorrect) or b/c match repeated surgical
#   trips
# ** Arbitrary decision ** to drop if data.diff > 1m b/c seems very
#    unlikely to be related to same episode of illness so the merge has
#    duplicate rows where the same MRN has matched more than once now
#    compare dates and sort by absolute difference then drop duplicates
#    thereby keeping the closest
m1[, date.diff := abs(as.Date(dtob)-date.match)]
setkey(m1,pkey,date.diff)
m1 <- m1[,.(dups=.N),by=pkey][dups>1][m1]
setkey(m1,pkey,date.diff,dtob)
m1
describe(as.numeric(m1$date.diff))
m1 <- m1[date.diff < 30 | is.na(date.diff)]
m1 <- m1[!is.na(id.t),.(pkey,id.t)]
# - [ ] NOTE(2015-12-04): m1 is simply a key to use to link census and theatre data
str(m1)

# Merge census data directly onto anaesthetic data
# ------------------------------------------------
# - [ ] NOTE(2015-12-04): do this directly (uses MRN only) for non
#   theatre related anaesthetics
str(tdt.census)
str(k.census) # see above but simplifed key for census data
str(tdt.a)
k.a <- tdt.a[,.(id.a, mrn, date.match)]
setkey(k.a,mrn)
str(k.a)
m2 <- merge(k.census, k.a, by=c('mrn'), all.x=TRUE, suffixes=c('census','a'))
str(m2)
length(unique(m2$pkey)) 
m2[, date.diff := abs(as.Date(dtob)-date.match)]
setkey(m2,pkey,date.diff)
m2 <- m2[,.(dups=.N),by=pkey][dups>1][m2]
setkey(m2,pkey,date.diff,dtob)
m2
describe(as.numeric(m2$date.diff))
m2 <- m2[date.diff < 30 | is.na(date.diff)]
m2 <- m2[!is.na(id.a),.(pkey,id.a)]
# - [ ] NOTE(2015-12-04): m2 is simply a key to use to link census and theatre data
str(m2)


# Now add primary keys for anaesthesia and theatres to census
tdt <- merge(tdt.census, m1, by='pkey', all.x=TRUE)
tdt <- merge(tdt, m2, by='pkey', all.x=TRUE)
setnames(tdt, 'id.a', 'id.a.direct') # direct link of anaesthetic case
ls()
str(mdt.t)
# anaesthetic links via theatre merge
tdt <- merge(tdt, mdt.t[,.(id.t, id.a.viaTheatre=id.t)], by='id.t', all.x=TRUE)
str(tdt)

wdt <- tdt

# Save file
save(wdt, file='../data/census_plus.RData')



