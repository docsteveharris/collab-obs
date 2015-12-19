# Created 2015-12-19
# Uses wdt.export.csv and wdt.anaes.csv

library(Hmisc)
library(ggplot2)
library(lubridate)
library(data.table)
library(gmodels)
library(plotly)

# Set-up plotly
Sys.setenv("plotly_username"="drstevok")
Sys.setenv("plotly_api_key"="9zzlo2rb2q")

rm(list=ls(all=TRUE))
load("../data/anaesthesia.RData")
str(tdt.a)
tdt.a[,anaesthetic.location := ifelse(indication.theatre,"Theatre","Labour ward")]

# Plot anaesthetic interventions over time
# - by location
d <- tdt.a[,.(N=.N),by=.(anaesthetic.location,
	date=round_date(anaesthetic.date, unit="month"))]
g <- ggplot(data=d, aes(y=N,x=date))
g + geom_smooth() +
	facet_grid(.~anaesthetic.location) +
	labs(x="Year", title="Anaesthetics by location")
ggsave("../figs/anaesthesia.by.location.png")

# - by type
d <- tdt.a[anaesthetic!="Other",.(N=.N),by=.(anaesthetic,
	date=round_date(anaesthetic.date, unit="month"))]
g <- ggplot(data=d, aes(y=N,x=date))
g + geom_smooth() +
	facet_grid(.~anaesthetic) +
	labs(x="Year", title="Anaesthetics by type")
ggsave("../figs/anaesthesia.by.type.png", width=8, height=4, scale=2)


# Now plot theatre workload stratified by time of day
load("../data/theatre.RData")
str(tdt.t)
tdt.t[, shift:=
	ifelse(theatre.hour %in% c(8:16), "Day",
	ifelse(theatre.hour %in% c(17:19), "Evening", 
		"Night" ))]
describe(tdt.t$shift)
d <- tdt.t[,.(N=.N),by=.(shift,
	date=round_date(theatre.date, unit="month"))]
g <- ggplot(data=d, aes(y=N,x=date))
g + geom_smooth() +
	facet_grid(.~shift) +
	labs(x="Year", y="Cases (per month)", title="Theatre work by shift")
ggsave("../figs/theatre.by.shift.png", width=8, height=6, scale=2)

# Now plot theatre workload stratified by time of day
# - additionally facet by day of the week
d <- tdt.t[,.(N=.N),by=.(dow=wday(theatre.date), shift,
	date=round_date(theatre.date, unit="month"))]
str(d)
g <- ggplot(data=d, aes(y=N,x=date))
g + geom_smooth() +
	facet_grid(shift~dow) +
	labs(x="Year", y="Cases (per month)", title="Theatre work by shift")
ggsave("../figs/theatre.by.dow.shift.png", width=8, height=6, scale=2)

stop()
#  ==============
#  = Quick play =
#  ==============

# Plot c-section rate by time of day over time

# Plot mean age of primips over time
str(wdt)
require(ggplot2)
require(lubridate)

g <- ggplot(data=wdt[primip==1],
	aes(y=age.mother, x=round_date(dob, "month")))
g + geom_smooth() +
	labs(x="Year", y="Age", title="Primips (Mean age)") +
	coord_cartesian(ylim=c(25,35))

quantile(wdt$bmi, c(0.4), na.rm=TRUE)
fivenum(wdt$bmi)
g <- ggplot(data=wdt,
	aes(y=quantile(bmi,c(0.9),na.rm=TRUE), x=round_date(dob, "month")))
g + geom_smooth() +
	labs(x="Year", y="BMI", title="BMI") +
	coord_cartesian(ylim=c(20,40))
