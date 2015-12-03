library(ggplot2)
devtools::install_github(("ropensci/plotly")) # latest development version

Sys.setenv("plotly_username"="drstevok")
Sys.setenv("plotly_api_key"="9zzlo2rb2q")

rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh'
setwd(paste(PROJECT_PATH, 'src', sep='/'))
load('../data/working.RData')
str(wdt)

tdt <- wdt[,.(dtob)]
tdt[,.(dtob,week=week(dtob),year=year(dtob))]
tdt <- tdt[,.N,by=round_date(dtob,"week")]
setnames(tdt,'round_date','week')

# drop last incomplete week 284
p <- plot_ly(data=tdt[c(-284,-1)], x=week, y=N)
plotly_POST(p, filename='labbook_151202')
