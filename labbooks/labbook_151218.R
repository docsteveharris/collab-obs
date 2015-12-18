# Uses wdt.export

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
