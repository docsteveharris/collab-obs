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
setwd(paste0(PROJECT_PATH, 'src'))

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
	d[, row.num := .I]
	d[, 'pkey' := paste(year,row.num,MRN, sep='-'), with=TRUE]
	rdt[[as.character(year)]] <- d
	key <- c(key, d$pkey)
	
}
# Check key is unique
assert_that(length(key)==length(unique(key)))
# Check data is correct length
assert_that(length(key)==19855)



# Loop through the list
rdt.all <- data.table()
for(i in 1:2) {
	print(paste0(as.character(2008+i), ': Importing and cleaning ...'))
	# print(nrow(rdt[[i]]))
	print(head(rdt[[i]][,.(MRN)]))
	rdt.i <- rdt[[i]]
	rdt.all <- rbind(rdt.)
}
# rdt[[1]][,1]

rdt.all <- rbind(
	rdt[[1]][,.(census=2009,MRN)],
	rdt[[2]][,.(census=2010,MRN)]
	# rdt[[3]][,.(census=2011,MRN)],
	# rdt[[4]][,.(census=2012,MRN)],
	# rdt[[5]][,.(census=2013,MRN)],
	# rdt[[6]][,.(census=2014,MRN)]
	)


#  ====================
#  = Tidy and convert =
#  ====================




