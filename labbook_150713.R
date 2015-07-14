# author: Steve Harris
# date: 2015-07-13
# subject: Working draft of data management for Perv and obs data

# Readme
# ======
# Aim is to merge excel data


# Todo
# ====


# Log
# ===
# 2015-07-13
# - file created



rm(list=ls())

setwd('/Users/steve/aor/p-academic/collab-obs-uclh/src')
library(XLConnect)
library(stringdist)

# Load anaesthetic database
# -------------------------

xlbook <- loadWorkbook('/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-anaes.xlsx')
rdf.anaes <- readWorksheet(xlbook, sheet = 'ANAESTHETIC DATABASE')
dim(rdf.anaes)
str(rdf.anaes)


# Load theatre databases
# ----------------------

xlbook <- loadWorkbook('/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-theatre13-15.xlsx')
rdf.theatre1 <- readWorksheet(xlbook, sheet = 'Theatre Case times 2013-15 orig')
str(rdf.theatre1)

# FIXME: 2015-07-13 - [ ] out of memory issues
# xlbook <- loadWorkbook('/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-theatre09-13.xlsx')
# rdf.theatre2 <- readWorksheet(xlbook, sheet = 'Theatre case times 2009-13 orig')
# str(rdf.theatre2)
rm(xlbook)
ls()

# Now try a test join
# test.a <- head(rdf.anaes[1:5])
# test.a
# test.t <- head(rdf.theatre1[1:5])
# test.t
test.a <- rdf.anaes[1:5]
nrow(test.a)
test.t <- rdf.theatre1[1:5]

j <- merge(test.a, test.t, by.x='MRN', by.y='Hospital.Number')
head(j)