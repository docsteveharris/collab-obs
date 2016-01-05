
save(wdt.final, file="../data/working_final.RData")

#  =======================================================
#  = Now merge anaesthetic data onto census data via MRN =
#  =======================================================
# - focus on maternal requests only (and first attempts)
load('../data/anaesthesia.RData')
str(tdt.a)
describe(tdt.a$labour.epidural)
tdt.labepi <- tdt.a[labour.epidural==1 & secondary == FALSE]
str(tdt.labepi)


# Merge 1 - using MRN and maternal request and then dropping if date is
# too far apart
tdt.mrn <- tdt.labepi
tdt.mrn[, mrn.a := mrn]
tdt.mrn <- tdt.mrn[,.(id.a,mrn.a,mrn,labepi.date=anaesthetic.date)]
setkey(tdt.mrn, mrn)
str(tdt.mrn)

wdt.mrn <- wdt
str(wdt.mrn)
wdt.mrn[, mrn.w := mrn]
(wdt.mrn <- wdt.mrn[,.(pkey,mrn,mrn.w,dob)])
setkey(wdt.mrn, mrn)

mdt.mrn <- tdt.mrn[wdt.mrn]
# Don't merge using absolute difference
mdt.mrn[, diffdate.merge :=
	as.numeric(difftime(labepi.date, dob, units="days"))]
mdt.mrn[!is.na(diffdate.merge),
	.(pkey,id.a, mrn.w, mrn.a, dob, labepi.date, diffdate.merge)][1:20]
describe(mdt.mrn$diffdate.merge)
# - [ ] NOTE(2015-12-15): allow labour epidurals for up to 3 days
mdt.mrn <- mdt.mrn[diffdate.merge <= 0 & diffdate.merge >= -3]
mdt.mrn[, mkey := "mrn"] # label the key
str(mdt.mrn)

message(paste(
	"Matched", nrow(mdt.mrn),
	"of", nrow(tdt.labepi), "labour epidurals"
	 ))


# Merge 2 - use MRN (first part, last part then fuzzy, then string distance)
# 1. first remove existing matches? common key is id.a
str(tdt.labepi)
str(mdt.mrn)
setkey(tdt.labepi, id.a)
setkey(mdt.mrn, id.a)
tdt.labepi.fuzzy <- mdt.mrn[,.(mkey,id.a)][tdt.labepi][is.na(mkey)]
str(tdt.labepi.fuzzy)
setnames(tdt.labepi.fuzzy, "mrn", "mrn.a")
names(tdt.labepi.fuzzy) # drop unessential columns
(tdt.labepi.fuzzy <- tdt.labepi.fuzzy[,c(1,2,7,18),with=FALSE])

# Now do the same for the census data - common key is pkey
str(wdt)
str(mdt.mrn)
setkey(wdt, pkey)
setkey(mdt.mrn, pkey)
wdt.fuzzy <- mdt.mrn[,.(mkey,pkey)][wdt][is.na(mkey) & birth_place=="theatre"]
names(wdt.fuzzy)
setnames(wdt.fuzzy, "mrn", "mrn.w")
(wdt.fuzzy <- wdt.fuzzy[,c(1,2,5,10),with=FALSE]) # necessary columns only

# 2	Create a 'block' - convert to numerical then round the date
# - [ ] NOTE(2015-12-15): initially blocked on month but too few matches
library(lubridate)
wdt.fuzzy[, date.round := as.numeric(round_date(dob, "year"))]
# wdt.fuzzy[, date.round := as.numeric(round_date(dob, "month"))]
tdt.labepi.fuzzy[, date.round := as.numeric(round_date(anaesthetic.date, "month"))]
str(wdt.fuzzy)
str(tdt.labepi.fuzzy)

# drop blocking because much smaller data
rpairs <- compare.linkage(
	wdt.fuzzy,
	tdt.labepi.fuzzy,
	blockfld = 5,
    strcmp=TRUE, strcmpfun=jarowinkler,
    exclude=c(1,2,4)
    # exclude=c(1,2,4,5)
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
    tdt.labepi.fuzzy[link.yes$id2,.(id.a,mrn.a,anaesthetic.date,link='fuzzy.yes')])
mdt.fuzzy.yes
mdt.fuzzy.poss <- cbind(
    wdt.fuzzy[link.poss$id1,.(pkey,mrn.w,dob)],
    tdt.labepi.fuzzy[link.poss$id2,.(id.a,mrn.a,anaesthetic.date,link='fuzzy.poss')])
mdt.fuzzy.poss
mdt.fuzzy <- rbind(mdt.fuzzy.yes, mdt.fuzzy.poss)
mdt.fuzzy

# Now set limits on data difference
mdt.fuzzy[, diffdate.merge := as.numeric(difftime(anaesthetic.date, dob, units="days"))]
mdt.fuzzy[,.(pkey, id.a, mrn.w, mrn.a,  dob, anaesthetic.date, diffdate.merge)]
mdt.fuzzy <- mdt.fuzzy[diffdate.merge <= 0 & diffdate.merge >= -3]
mdt.fuzzy[, mkey := "fuzzy"] # label the key
mdt.fuzzy

# Now remove duplicates but first sort by string dist
mdt.fuzzy[, mrn.dist := stringdist(mrn.w, mrn.a, method='jw')]

setorder(mdt.fuzzy,pkey,mrn.dist)
mdt.fuzzy
setkey(mdt.fuzzy, pkey)
mdt.fuzzy <- unique(mdt.fuzzy)
mdt.fuzzy
setorder(mdt.fuzzy,mrn.dist)
setkey(mdt.fuzzy, mrn.dist)
mdt.fuzzy

# Repeat remove of duplicates (this time of id.a dups)
setorder(mdt.fuzzy,id.a,mrn.dist)
mdt.fuzzy
setkey(mdt.fuzzy, id.a)
mdt.fuzzy <- unique(mdt.fuzzy)
mdt.fuzzy

# Now drop unlikely merges
setorder(mdt.fuzzy,mrn.dist)
lapply(c(0:10), function(x) head(mdt.fuzzy[mrn.dist < x/10 & mrn.dist >= (x-1)/10]))
tail(mdt.fuzzy[mrn.dist <0.15])

# - [ ] NOTE(2015-12-15): manually tuning merges allowed through
(mdt.fuzzy <- mdt.fuzzy[mrn.dist < 0.1])
nrow(mdt.fuzzy)

# Now bind mdt.mrn and mdt.fuzzy and merge back onto census
(mdt <- rbind(
	mdt.mrn[,.(pkey,id.a,mkey,mrn.w,mrn.a)], 
	mdt.fuzzy[,.(pkey,id.a,mkey,mrn.w,mrn.a)]
	))
mdt[, mrn.dist := stringdist(mrn.w, mrn.a, method='jw')]
setorder(mdt,pkey,mrn.dist)
setkey(mdt, pkey)
mdt<- unique(mdt)
nrow(mdt)

message(paste(
	"Fuzzy merges:", nrow(mdt.fuzzy), 
	"\nMatched (after additional fuzzy merge)", nrow(mdt.mrn),
	"of", nrow(tdt.labepi), "labour epidurals"
	 ))

# Now merge working data and theatre data
mdt
setkey(wdt.final, pkey)
wdt.final <- mdt[,.(pkey,id.a.labepi=id.a,mrn.dist.labepi=mrn.dist)][wdt.final]
# wdt.final$i.id.mother <- NULL
assert_that(nrow(wdt.final)==nrow(unique(wdt.final)))
wdt.final[, match.labepi := ifelse(is.na(id.a.labepi),0,1)]
# 97% merge OK
describe(wdt.final$match.labepi) 
wdt.final
with(wdt.final, CrossTable(round_date(dob, "year"), birth_place))
# - [ ] NOTE(2015-12-15): v poor matching in 2010 ? problems with data
with(wdt.final, CrossTable(round_date(dob, "year"), match.labepi))

# Define the fields you want to bring in from tdt
names(tdt.labepi)

tdt.t.fields <- c("id.t",
	"theatre.hour", "priority", "anaesthesia.time", "surgical.time")
setkey(wdt.final, id.t)
setkey(tdt.t, id.t)
wdt.final <- tdt.t[,tdt.t.fields,with=FALSE][wdt.final]

# Let's do some tidying of these data so they are close to ready for export
str(wdt.final)
setkey(wdt.final, pkey)
assert_that(nrow(wdt.final)==nrow(unique(wdt.final)))
wdt <- wdt.final[,.(
	id.birth=as.numeric(id.birth),
	id.mother,
	pkey,
	dtob,
	dob,
	birth.seq,
	birth_place,
	match.theatre,
	id.theatre = id.t,
	priority,
	anaesthesia.time,
	surgical.time,
	match.labepi,
	id.a.labepi
	)]
setkey(wdt.final, id.birth)
assert_that(nrow(wdt)==nrow(unique(wdt)))
head(wdt)

# Merge the remaining census fields
names(wdt)
str(tdt.census)
names(tdt.census)
# - [ ] NOTE(2015-12-16): do this var by var - seems clumsy but can check as you do it 

# mat_age
wdt <- merge(wdt, tdt.census[,.(pkey, age.mother=mat_age)],
	by="pkey", all.x=TRUE)
describe(wdt$age.mother)

# labour_type
table(tdt.census$labour_type)
wdt <- merge(wdt, tdt.census[,.(pkey,
	labour.type = 
	ifelse(grepl(".*spont.*", labour_type, ignore.case=T, perl=T), "spontaneous",
	ifelse(grepl(".*section*", labour_type, ignore.case=T, perl=T), "csection",
	ifelse(grepl(".*induction*", labour_type, ignore.case=T, perl=T), "induction",
		"other" ))))
		],
		by="pkey", all.x=TRUE)
describe(wdt$labour.type)

# lscs
# lscs.cat
wdt <- merge(wdt, tdt.census[,.(pkey, lscs, lscs.cat)],
	by="pkey", all.x=TRUE)
describe(wdt$lscs)
describe(wdt$lscs.cat)

# birth_place - already imported
setnames(wdt, "birth_place", "birth.place")

# parity - primip imported
# primip
wdt <- merge(wdt, tdt.census[,.(pkey, primip)],
	by="pkey", all.x=TRUE)
describe(wdt$primip)

# bmi
wdt <- merge(wdt, tdt.census[,.(pkey, bmi)],
	by="pkey", all.x=TRUE)
describe(wdt$bmi)

# Duration of stages of labour
# Custom function to import times
hm.clean <- function(s) {
	require(lubridate)
	# Converts string to a timestamp to parse (takes advantage of lubridate)
	# but therefore fails with "32h 30m" b/c times cannot be > 24h
	# problematic for 64 of 19k
	t <- parse_date_time(s, orders=c("H!M!", "H!M!S!", "M!", "H!"), truncated=1)-ymd("0000-01-01")
	return(as.numeric(round(t/3600,2)))
}
wdt <- merge(wdt,
		tdt.census[,.(pkey,
		 stage1.hrs=hm.clean(first_duration),
		 stage2.hrs=hm.clean(second_duration),
		 stage3.hrs=hm.clean(third_duration)
		 )],
	by="pkey", all.x=TRUE)
describe(wdt$stage1.hrs)
describe(wdt$stage2.hrs)
describe(wdt$stage3.hrs)

# APGAR scores
wdt <- merge(wdt,
		tdt.census[,.(pkey,
		apgar01=apgar_1,
		apgar05=apgar_5,
		apgar10=apgar_10
		 )],
	by="pkey", all.x=TRUE)
describe(wdt$apgar01)
describe(wdt$apgar05)
describe(wdt$apgar10)

# Cord gases and arterial pH and NNU admission
wdt <- merge(wdt,
		tdt.census[,.(pkey,
		cord.gases=ifelse(cord_gases=="Yes",1,0),
		cord.ph=arterial_ph,
		nnu.admx=ifelse(nnu_admission,1,0)
		 )],
	by="pkey", all.x=TRUE)
describe(wdt$cord.gases)
describe(wdt$cord.ph)
describe(wdt$nnu.admx)

# Delivery route
describe(tdt.census$delivery_route)
wdt <- merge(wdt, tdt.census[,.(pkey,
	delivery.route = 
	ifelse(grepl(".*caesar.*", delivery_route, ignore.case=T, perl=T), "csection",
	ifelse(grepl(".*vaginal*", delivery_route, ignore.case=T, perl=T), "vaginal",
		"other" )))
		],
		by="pkey", all.x=TRUE)
describe(wdt$delivery.route)

wdt[, dob.hr := round_date(dtob,unit="hour")]
wdt[, dob.hr := hour(dtob)]
names(tdt.census)
str(wdt)




#  ================
#  = Final export =
#  ================
ls()
str(tdt.a)

wdt.export <- wdt
wdt.export$pkey <- NULL
wdt.export$mrn <- NULL
wdt.export$mrn.w <- NULL
wdt.export$i.id.mother <- NULL
setorder(wdt.export,id.birth)
str(wdt.export)

save(wdt,wdt.export, file="../data/working.RData")
write.csv(wdt.export, "../data/wdt.export.csv" )

