rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh/'
setwd(paste0(PROJECT_PATH, 'src'))

library(data.table)
library(gmodels)

wdt <- data.table(read.csv(file="../data/theatre_linked.csv"))
wdt
str(wdt)
table(wdt$link)
CrossTable(wdt[!is.na(id.t) & labour.epidural==FALSE]$link)
