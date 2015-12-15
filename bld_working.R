# author: Steve Harris
# date: 2015-12-03
# subject: Combine the various data sets into a single table

# Readme
# ======
# Starting with census - therefore one row per birth
# - join to theatre
# 		- limit to place of birth theatre (hence maternal only theatre indications)
  

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
library(RecordLinkage)
library(stringdist)

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

wdt.sofarsogood <- wdt
# wdt <- wdt.sofarsogood
wdt

# Future merges to maternal data will be based on MRN and dob
# So take an example data set that contains MRN's and dates and write a function for merging
# Incoming data will contain
# - MRN
# - date (you may want to define if you pick closest overall, closest previous, closest past)
# - a unique key of its own
# Merge steps
# 1. 	Look for perfect MRN and date based merges
# 		?what to do if there are duplicate MRN and dates in the incoming data?
# 		?should drop and keep only the most suitale so this is not possible
# 		- e.g. prioritise certain theatre procedures, or blood timing
# 2.	Now try fuzzy merges
# 3.	rbind the different merges and use string dist on the MRN and
#       distance on the date to choose the best


# Link census to theatre data
# ---------------------------
# Direct link census and theatre data via dob and theatre date AND MRN
str(wdt)
str(tdt.census)
describe(tdt.census$birth_place)
# Only link where place of birth is labour ward theatres
# so now merge birth place onto wdt with a bit of cleaning
setkey(wdt, pkey)
setkey(tdt.census, pkey)
wdt <- tdt.census[,.(pkey,
	birth_place=
	    ifelse(grepl(".*theatr.*", birth_place, ignore.case=T, perl=T), "theatre", 
	    ifelse(grepl(".*labour ward.*", birth_place, ignore.case=T, perl=T), "labour ward", "other"))
	)][wdt]
describe(wdt$birth_place)
# Now merge
load('../data/theatre.RData')
tdt.t[, birth_place := "theatre"] # add this column to force the merge to use it

# Merge 1 - using MRN and theatre and then dropping if date is too far apart
tdt.mrn <- tdt.t
tdt.mrn[, mrn.t := MRN]
setnames(tdt.mrn, "MRN", "mrn")
tdt.mrn <- tdt.mrn[,.(id.t,mrn.t,mrn,birth_place,theatre.date)]
setkey(tdt.mrn, mrn, birth_place )
str(tdt.mrn)

wdt.mrn <- wdt
str(wdt.mrn)
wdt.mrn[, mrn.w := mrn]
(wdt.mrn <- wdt.mrn[,.(pkey,mrn,mrn.w,birth_place,dob)])
setkey(wdt.mrn, mrn, birth_place)

mdt.mrn <- tdt.mrn[wdt.mrn]
mdt.mrn[, diffdate.merge := abs(difftime(dob, theatre.date, units="days"))]
mdt.mrn[,.(pkey,id.t, mrn.w, mrn.t, dob, theatre.date, diffdate.merge)]
mdt.mrn <- mdt.mrn[diffdate.merge<=1]
mdt.mrn[, mkey := "mrn"] # label the key
str(mdt.mrn)
# - [ ] NOTE(2015-12-11): @wow 12k of 19k go through theatres!

# - [ ] NOTE(2015-12-11): @resume(2015-12-11)
# Merge 2 - use MRN (first part, last part then fuzzy, then string distance)
# 1. first remove existing matches? common key is id.t
str(tdt.t)
str(mdt.mrn)
setkey(tdt.t, id.t)
setkey(mdt.mrn, id.t)
tdt.t.fuzzy <- mdt.mrn[,.(mkey,id.t)][tdt.t][is.na(mkey)]
# so we 3400+ theatre cases that we didn't match to our census data
setnames(tdt.t.fuzzy, "mrn", "mrn.t")
names(tdt.t.fuzzy) # drop unessential columns
(tdt.t.fuzzy <- tdt.t.fuzzy[,c(1,2,5,6),with=FALSE])

# Now do the same for the census data - common key is pkey
str(wdt)
str(mdt.mrn)
setkey(wdt, pkey)
setkey(mdt.mrn, pkey)
wdt.fuzzy <- mdt.mrn[,.(mkey,pkey)][wdt][is.na(mkey) & birth_place=="theatre"]
names(wdt.fuzzy)
setnames(wdt.fuzzy, "mrn", "mrn.w")
(wdt.fuzzy <- wdt.fuzzy[,c(1,2,5,10),with=FALSE]) # necessary columns only

# 2	Create a 'block' - take first (or last 2 chars) 
# 	- skipping this since we have only 400 to fuzzy join	
rpairs <- compare.linkage(
	wdt.fuzzy,
	tdt.t.fuzzy,
    strcmp=TRUE, strcmpfun=jarowinkler,
    exclude=c(1,2,4)
    )

rpairs <- epiWeights(rpairs) # calculate weights
str(rpairs)
summary(rpairs)
summary(rpairs$Wdata)

head(getPairs(rpairs, max.weight=0.99, min.weight=0.95),20) # run after weights

# Now classify
rlink <- epiClassify(rpairs, 0.95, 0.85)
lpairs <- rlink$pairs
link.yes <- lpairs[rlink$prediction=='L',]
link.poss <- lpairs[rlink$prediction=='P',]

# Now subset
mdt.fuzzy.yes <- cbind(
    wdt.fuzzy[link.yes$id1,.(pkey,mrn.w,dob)],
    tdt.t.fuzzy[link.yes$id2,.(id.t,mrn.t,theatre.date,link='fuzzy.yes')])
mdt.fuzzy.yes
mdt.fuzzy.poss <- cbind(
    wdt.fuzzy[link.poss$id1,.(pkey,mrn.w,dob)],
    tdt.t.fuzzy[link.poss$id2,.(id.t,mrn.t,theatre.date,link='fuzzy.poss')])
mdt.fuzzy.poss
mdt.fuzzy <- rbind(mdt.fuzzy.yes, mdt.fuzzy.poss)
mdt.fuzzy

# Now set limits on data difference
mdt.fuzzy[, diffdate.merge := abs(difftime(dob, theatre.date, units="days"))]
mdt.fuzzy[,.(pkey, id.t, mrn.w, mrn.t,  dob, theatre.date, diffdate.merge)]
mdt.fuzzy <- mdt.fuzzy[diffdate.merge<=1]
mdt.fuzzy[, mkey := "fuzzy"] # label the key
mdt.fuzzy

# Now remove duplicates but first sort by string dist
mdt.fuzzy[, mrn.dist := stringdist(mrn.w, mrn.t, method='jw')]

setorder(mdt.fuzzy,pkey,mrn.dist)
mdt.fuzzy
setkey(mdt.fuzzy, pkey)
mdt.fuzzy <- unique(mdt.fuzzy)
mdt.fuzzy
setorder(mdt.fuzzy,mrn.dist)
setkey(mdt.fuzzy, mrn.dist)
(mdt.fuzzy <- mdt.fuzzy[mrn.dist < 0.1])

# End of fuzzy merge - not particularly rewarding

# Now bind mdt.mrn and mdt.fuzzy and merge back onto census
(mdt <- rbind(
	mdt.mrn[,.(pkey,id.t,mkey,mrn.w,mrn.t)], 
	mdt.fuzzy[,.(pkey,id.t,mkey,mrn.w,mrn.t)]
	))
mdt[, mrn.dist := stringdist(mrn.w, mrn.t, method='jw')]
setorder(mdt,pkey,mrn.dist)
setkey(mdt, pkey)
mdt<- unique(mdt)
nrow(mdt)
mdt

# Now merge working data and theatre data
wdt
wdt.final <- wdt
setkey(wdt.final, pkey)
wdt.final <- mdt[,.(pkey,id.t)][wdt.final]
wdt.final$i.id.mother <- NULL
assert_that(nrow(wdt.final)==nrow(unique(wdt.final)))
wdt.final[, match.theatre := 
	ifelse(is.na(id.t) & birth_place == "theatre", 0,
	ifelse(!is.na(id.t) & birth_place == "theatre", 1, NA)
	)]
# 97% merge OK
describe(wdt.final$match.theatre) 
wdt.final

# Define the fields you want to bring in from tdt
names(tdt.t)
tdt.t.fields <- c("id.t",
	"theatre.hour", "priority", "anaesthesia.time", "surgical.time")
setkey(wdt.final, id.t)
setkey(tdt.t, id.t)
wdt.final <- tdt.t[,tdt.t.fields,with=FALSE][wdt.final]
str(wdt.final)



# 3. Now do fuzzy merge on hospital number
#    ?may want to include theatre data now?



# - [ ] NOTE(2015-12-09): old code
stop()
load('../data/theatre_linked.RData')
setnames(tdt.t,'MRN','mrn')
setnames(tdt.a,'MRN','mrn')
str(tdt.t)
ls()


# - [ ] NOTE(2015-12-08): debugging?
stop()
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
m1[, date.diff := abs(as.Date(dtob) - date.match)]
setkey(m1,pkey,date.diff)
m1[,.(dups=.N),by=pkey][dups>1][m1]
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



