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

# - [ ] NOTE(2015-12-01): variables missing from majority therefore dropped
vars <- vars[vars != "bw"]
vars <- vars[vars != "episiotomy_reason"]
vars <- vars[vars != "staff_delivery"]
vars <- vars[vars != "ethnic_parents"]

# - [ ] NOTE(2015-12-01): 
# - ethnicity missing from 2009 but kept overall

wdt <- wdt.original
for (var in vars[2:length(vars)]) {
	d <- data.table()
	for (i in 1:6) {
		r <- try({
			x <- rdt[[i]][,c('pkey',var), with=FALSE]
			d <- rbind(d,x)
		}, silent=TRUE)
		if (class(r)=="try-error") {
			print(paste("ERROR:", var, "not found in", 2008+i, "census?"))
			print(names(rdt[[i]]))
		}
		# assert_that(var %in% names(rdt[[i]])) # error check
	}
	setkey(d,pkey)
	wdt <- merge(wdt,d,all.x=T)
	# print(describe(wdt[,var,with=FALSE]))
}

describe(wdt)