# STA 242 HW01
# 20130109 - hughcrockford

# things to try out:
## melt/cast
## plyr - aaply - parralle
## ggplot2 + - lattice?
## sqldf? data.table?

#########################
# Questions
#########################

# delay by carrier
# delay by quarter - season?

library(lattice)
library(ggplot2)
library(reshape2)


# load(url('http://eeyore.ucdavis.edu/stat242/data/BayAreaDelays.rda'))
save("BayAreaDelays.rda")


load("BayAreaDelays.rda")
trim = BayAreaDelays[,c(2,6,9,10,14,23,30,31:32,41:43,48:52,55,57:61)]

##################33
# delay by carrier
#####################

levels(trim$CARRIER)
summary(trim$DEP_DELAY)
boxplot(trim$DEP_DELAY ~ trim$CARRIER)	# prob most useful

#delay vary by time of year, some carriers better than others?
qplot(delayed$dat, color=delayed$CARRIER, geom = "density")

#######################
# delay by tim of year
#######################3

#delay vary by time of year,?
del = transform( trim , isLate = trim$ARR_DELAY < 0 )
delayed = subset(del, del$isLate)
delayed$dat = strptime(delayed$FL_DATE,format = "%Y-%m-%d")
qplot(delayed$dat,geom="density")

# pattens in delay reason, more weather delay in winter??
reasons = transform( trim, isWEATHER = trim$WEATHER_DELAY > 0, isCARRIER = trim$CARRIER_DELAY > 0 , isNAS = trim$NAS_DELAY > 0 , isSECURITY = trim$SECURITY_DELAY >0, isLATE_AIRCRAFT = trim$LATE_AIRCRAFT_DELAY > 0 )
dat = reasons[,c(2,3,24:28)]
m.datT = melt(dat,id = 1:2)
m.datTT = m.dat[m.dat$value == TRUE,]	# for some reason didnt work??
m.datTT = subset(m.datT,m.datT$value)
m.datTT$dat = strptime(m.datTT$FL_DATE,format = "%Y-%m-%d")
qplot(m.datTT$dat, geom = "density")
qplot(m.datTT$dat, color=m.datTT$variable, geom = "density")
min(m.datT$FL_DATE)
densityplot(m.datT$FL_DATE, groups=m.datT$CARRIER)




# delay explained by recent flights - to what number?
late = as.numeric(del$isLate)
plot(late[200:500],type = "l")
