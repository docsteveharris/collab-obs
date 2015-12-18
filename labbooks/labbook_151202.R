library(plotly)
devtools::install_github(("ropensci/plotly")) # latest development version

Sys.setenv("plotly_username"="drstevok")
Sys.setenv("plotly_api_key"="9zzlo2rb2q")

rm(list=ls())
PROJECT_PATH <- '/Users/steve/aor/academic/collab-obs-uclh'
setwd(paste(PROJECT_PATH, 'src', sep='/'))
load('../data/census.RData')
str(wdt)

tdt <- wdt[,.(dtob,lscs.cat)]
tdt[,.(dtob,week=week(dtob),year=year(dtob),lscs.cat)]
tdt <- tdt[,.N,by=.(round_date(dtob,"week"),lscs.cat)]
setnames(tdt,'round_date','week')
# set lscs.cat to -1 if SV
tdt[, lscs.cat := ifelse(is.na(lscs.cat),-1,lscs.cat)]
tdt[, y := factor(lscs.cat,
                  levels=c(-1,4,3,2,1),
                  labels=c('Vaginal', 'Cat IV', 'Cat III', 'Cat II', 'Cat I')
                  )]
library(Hmisc)
describe(tdt$y)
tdt

# drop last incomplete week 284
p <- plot_ly(data=tdt[week!='2009-8-02' & week !='2015-01-4'],
             group=y,
             x=week, y=N)
p
plotly_POST(p, filename='labbook_151202')


# - [ ] NOTE(2015-12-15): using wdt.final from bld_working.R

# PLAYING
# Final tidy by string and date diff
str(wdt.final)
# Let's plot proportion of cases per week that go through theatre
pdt <- wdt.final[,.(dtob,birth_place,surgical.time)]
pdt[,.(week=week(dtob),birth_place)]
library(lubridate)
pdt <- pdt[,.N,by=.(week=round_date(dtob,"week"),birth_place)]
table(pdt$birth_place)
pdt <- merge(
	pdt[,.(births=sum(N)),by=week],
	pdt[birth_place=="theatre",.(week,births.theatre=N)],
	by="week")
pdt[,y:=round(100*births.theatre/births)]
pdt


library(plotly)
p <- plot_ly(data=pdt,
             x=week, y=y)
p
Sys.setenv("plotly_username"="drstevok")
Sys.setenv("plotly_api_key"="9zzlo2rb2q")
plotly_POST(p, filename='labbook_151212')


