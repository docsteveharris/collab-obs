# author: Steve Harris
# date: 2015-11-29
# subject: Build final data table

# Readme
# ======
# - working from field list provided by PS 2015-11-29
# - base data comes from /Users/steve/aor/academic/collab-obs-uclh/data/_raw/25.11.15 Obs database.xlsx



# Todo
# ====
# - see todo.md


# Log
# ===
# 2015-11-29
# - file created
# - tried to import via XLconnect but 'out of memory' error
# - manually convert to tab delimited text


rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh'
EXCEL_FILE <- '151129_census.xlsx'
DATA_FILE <- paste(PROJECT_PATH, 'data', EXCEL_FILE, sep='/')
TAB_DATA <- '151129_census'
setwd(paste(PROJECT_PATH, 'src', sep='/'))

library(assertthat)
library(Hmisc)
library(data.table)
library(gmodels)

#  =============================
#  = Import tab delimited data =
#  =============================

# - [ ] NOTE(2015-11-29): just testing how to assing to a list
# rdf <- list()
# rdf[['2010']] <- rdf2010 
# str(rdf)
# class(rdf$'2010')

rdt <- list()
key <- c()
for(year in 2009:2014) {
	file.name <- paste0(PROJECT_PATH, '/data/', TAB_DATA, as.character(year), '.txt')
	print(file.name)
	d <- read.delim(file.name, 
	        strip.white=TRUE,
	        stringsAsFactors=FALSE)
	print(names(d[1]))
	assert_that(names(d)[1]=='MRN')
	# Make a unique key for future merging and joining
	d <- data.table(d)
	d[, row.num := .I] # excel sheet row number
	d <- d[!is.na(MRN) & MRN != ""]
	d[, 'pkey' := paste(year,row.num,MRN, sep='-'), with=TRUE] # unique key
	setkey(d,pkey)
	# set column names to lower case
	for (n in names(d)) {
		setnames(d,n,tolower(n))
	}
	rdt[[as.character(year)]] <- d
	key <- c(key, d$pkey)
}
vars <- names(d) # capture variables
vars.original <- vars

# Check key is unique
assert_that(length(key)==length(unique(key)))
# Check data is correct length
length(key)
assert_that(length(key)==19810)

# Create your master data table
wdt <- data.table(key)
setnames(wdt,'key','pkey')
setkey(wdt,pkey)
str(wdt)
wdt.original <- wdt

# Take a variable from one sheet and merge it against the key
# Loop through the 5 raw tables
# Extract the variable and the key and rbind (stack)
# then merge


# Now add in some error management
#  =====================
#  = Error correction  =
#  =====================
# rdt[[1]][,c(1:5),with=F]
# rdt[[2]][,c(1:5),with=F]

# - [ ] NOTE(2015-12-01): fix separate columns for dob date and timebirth
rdt[[2]][,dob_timebirth := paste(dob, timebirth)]
rdt[[3]][,dob_timebirth := paste(dob, timebirth)]

# - [ ] NOTE(2015-12-01): bw instead of birth_weight in 2009-11
setnames(rdt[[1]], 'bw', 'birth.weight')
setnames(rdt[[2]], 'bw', 'birth.weight')
setnames(rdt[[3]], 'bw', 'birth.weight')

# drop pkey from vars
vars <- vars[vars != "pkey"]
# - [ ] NOTE(2015-12-01): variables missing from majority therefore dropped
vars <- vars[vars != "bw"]
vars <- vars[vars != "episiotomy_reason"]
vars <- vars[vars != "staff_delivery"]
vars <- vars[vars != "ethnic_parents"]

# - [ ] NOTE(2015-12-01): 
# - ethnicity missing from 2009 but kept overall
# vars <- vars[vars != "ethnicity"]

wdt <- wdt.original
for (var in vars[1:length(vars)]) {
	d <- data.table()
	for (i in 1:6) {
		r <- try({
			x <- rdt[[i]][,c('pkey',var), with=FALSE]
			d <- rbind(d,x)
		}, # silent=TRUE
		)
		if (class(r)=="try-error") {
			print(paste("ERROR:", var, "not found in", 2008+i, "census?"))
			print(names(rdt[[i]]))
		}
		# assert_that(var %in% names(rdt[[i]])) # error check
	}
	setkey(d,pkey)
	tryCatch(
		wdt <- merge(wdt,d,all.x=T),
		error=function(c) {
			# c$message <- paste0(c$message, " related to names(d)\n", names(d))
			stop(c$message)
		}
		)
	# print(describe(wdt[,var,with=FALSE]))
}

str(wdt)
# describe(wdt)

# Convert birth.order to numeric
describe(wdt$birth.order)
table(wdt$parity)
describe(wdt$ebl)
wdt[, `:=` (
	birth.order = as.numeric(birth.order),
	ebl         = as.numeric(ebl),
	primip      = ifelse(substr(parity,0,1)==0,TRUE,FALSE),
	dtob        = as.POSIXct(strptime(dob_timebirth, format="%d/%m/%Y %H:%M", tz="GMT"))
	)]
str(wdt)

# - [ ] NOTE(2015-12-02): correct for 2 digit years
library(lubridate)
wdt[year(dtob)==11, dtob := update(dtob, year=2011)]
table(year(wdt$dtob))

# Convert dob_timebirth to date
setorder(wdt,dtob,birth.order)

str(wdt)

# LSCS
table(wdt$delivery_route)
wdt[, lscs:=ifelse(grepl(".*caesar.*", delivery_route,
	ignore.case=TRUE, perl=TRUE),1,0)]
describe(wdt$lscs)

# LSCS cat
describe(wdt$cesarean_category)
wdt[, lscs.cat := 
	ifelse(cesarean_category %in% c('Category 1', 'I Crash (Emergency)'), 1,
	ifelse(cesarean_category %in% c('Category 2', 'II Urgent'), 2,
	ifelse(cesarean_category %in% c('Category 3', 'III Scheduled', 'III Scheduled.'), 3,
	ifelse(cesarean_category %in% c('Category 4', 'IV Planned (Elective)'), 4,
		NA ))))]
describe(wdt$lscs.cat)


# Save file
save(wdt, file='../data/census.RData')
# rm(list=ls(all=TRUE))
# load(file='../data/working.RData')


