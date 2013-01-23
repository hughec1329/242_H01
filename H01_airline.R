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
trim = BayAreaDelays[,c(2,6,9,10,15,24,30,31:32,41:43,48:52,55,57:61)]

#####################
# use of airports
#####################

# which airports are the worst for on time performance?

table(trim$ORIGIN)
table(trim$DEST)
trim = transform(trim, inbound =  trim$DEST %in% c("SFO","OAK","SMF"), outbound = trim$ORIGIN %in% c("SFO","OAK","SMF"))	#  inbound vs outbound.
latelimit = 30                         # 
trim$isLate = (trim$inbound == TRUE & trim$ARR_DELAY > latelimit) | (trim$outbound == TRUE & trim$DEP_DELAY > latelimit )
llate = tapply(trim$isLate, trim$DEST , sum,na.rm = TRUE) / tapply(trim$isLate, trim$DEST , length)  


jpeg("lateCar.jpg")
lateCar = tapply(trim$isLate, trim$CARRIER , sum,na.rm = TRUE) / tapply(trim$isLate, trim$CARRIER , length)  
barplot(lateCar[order(lateCar)],main = "% late flights (+- >30 min ) by carrier")
dev.off()

jpeg("pcl_airport.jpg")
barplot(late[order(late)],main = "percent flights that are late (>30 min) by airport")
dev.off()

# by map - color scale to red of lateness??
library(maps)
library(maptools)
library(RColorBrewer)
ports = readShapeSpatial("./airports/airports.shp")
map("state")
air = ports@data$LOCID
colScale = colorRampPalette(brewer.pal(3,"Reds"))
lt = (late / max(late))*10
points(ports[air %in% rownames(late),],pch = 20,col = colScale[rank(late)],cex = lt)

# playt with color ramp

col = colorRamp(colors = c("white","red"))
points(ports[air %in% rownames(late),],pch = 20,col = col(late),cex = 2)

jpeg("latemap.jpg")
map("state",main = "Late flights by airport, size and color = % late")
col = colorRampPalette(c("white","red"))
points(ports[air %in% rownames(late),],pch = 20,col = col(10)[cut(late,breaks = 10)],cex = lt)
dev.off()

# size by rank lateness

###############
# how late are most of the late planes"
###################

late = function(late) {
	isLate = (trim$inbound == TRUE & trim$ARR_DELAY > late) | (trim$outbound == TRUE & trim$DEP_DELAY > late )
	lp = sum(isLate,na.rm = TRUE)/length(isLate)
	return(lp)
}

llist = seq(0,600,30)
lateness = sapply(llist,late)
jpeg("lateness.jpg")
plot(llist,lateness,type = "b", main = "percent of planes late by various cutoffs", xlab = "late time cutoff", ylab = "percent late")
dev.off()


late3 = function(varb,late) {
	trim$isLate = (trim$inbound == TRUE & trim$ARR_DELAY > late) | (trim$outbound == TRUE & trim$DEP_DELAY > late )
	late = tapply(trim$isLate, trim[[varb]] , sum,na.rm = TRUE) / tapply(trim$isLate, trim[[varb]] , length)  
	return(late)
}

l = sapply(c("CARRIER","DEST","ORIGIN"),function(x) mapply(late2,x,seq(60,360,60)))
ld =lapply(l,data.frame)
# lapply(ld,function(i) names(i) = seq(60,360,60))	# didnt work?
n = seq(60,360,60)
names(ld$CARRIER) = n
names(ld$DEST) = n
names(ld$ORIGIN) = n

# need a better way of plotting this, but catagorical makes hard- 3d barplot?

jpeg("ltcarrier.jpg")
y=ld$CARRIER
y = t(y)
o = melt(y)
# sapply(1:14,function(i) lines(1:6,y[,i]))	# couldnt get to work, lattice? ggplot?
cutoff=n
plot(cutoff, y[,1],type = "b",main = "lateness by carrier with different late cutoffs", ylab = "percent late")
lines(cutoff,y[,2])
lines(cutoff,y[,3])
lines(cutoff,y[,4])
lines(cutoff,y[,5])
lines(cutoff,y[,6])
lines(cutoff,y[,7])
lines(cutoff,y[,8])
dev.off()


jpeg("ltcarrier.jpg")	#much easier w ggplot!
y=ld$CARRIER
y = t(y)
o = melt(y)
rank(y[1,])	# rank of HA at cutoff = 60.
rank(y[6,])	# rank of HA at cutoff = 360.
y[6,7]/mean(y[6,])
carriers[carriers$Code == "HA",]	# ID HA as hawiaan air.
qplot(X1,value , color = X2,data = o,type = "b",geom="path",main = "percent planes delayed by carrier for various cutoffs of late", xlab = "late cutoff", ylab = "Percent late")		
dev.off()


jpeg("ltdest.jpg")
y=ld$DEST
i=t(y)
r=i[,c("SFO","SMF","OAK")]
o = melt(r)
qplot(X1,value,data=o, color = X2,geom = "line",main = "lateness by destination airport (for SFO, SMF, OAK) by different late cutoffs")
dev.off()

jpeg("ltorig.jpg")
y=ld$ORIGIN
i=t(y)
r=i[,c("SFO","SMF","OAK")]
o = melt(r)
qplot(X1,value,data=o, color = X2,geom = "line",main = "lateness by origin airport (for SFO, SMF, OAK) by different late cutoffs")
dev.off()

##################33
# delay by carrier
#####################

levels(trim$CARRIER)
summary(trim$DEP_DELAY)
boxplot(trim$DEP_DELAY ~ trim$CARRIER,main = "boxplot showing level of dely by airline")	# prob most useful

#delay vary by time of year, some carriers better than others?
qplot(delayed$dat, color=delayed$CARRIER, geom = "density")

#######################
# delay by time of year
#######################3

#delay vary by time of year,?
del = transform( trim , isLate = trim$ARR_DELAY < 0 )
delayed = subset(del, del$isLate)
delayed$dat = strptime(delayed$FL_DATE,format = "%Y-%m-%d")
jpeg("delaydate.jpg")
qplot(delayed$dat,geom="density",main = "Percent of flights delayed, by time of year")
dev.off()

# pattens in delay reason, more weather delay in winter??
reasons = transform( trim, isWEATHER = trim$WEATHER_DELAY > 0, isCARRIER = trim$CARRIER_DELAY > 0 , isNAS = trim$NAS_DELAY > 0 , isSECURITY = trim$SECURITY_DELAY >0, isLATE_AIRCRAFT = trim$LATE_AIRCRAFT_DELAY > 0 )
dat = reasons[,c(2,3,24:28)]
m.datT = melt(dat,id = 1:2)
m.datTT = m.dat[m.dat$value == TRUE,]	# for some reason didnt work??
m.datTT = subset(m.datT,m.datT$value)
m.datTT$dat = strptime(m.datTT$FL_DATE,format = "%Y-%m-%d")
qplot(m.datTT$dat, geom = "density")

jpeg("weather.jpg")
qplot(m.datTT$dat, color=m.datTT$variable, geom = "density",main = "Reason for delay, by time")
dev.off()



# delay explained by recent flights - to what number?
late = as.numeric(del$isLate)
plot(late[200:500],type = "l")

# whichc airline takes loongest to taxi over same route - pref treatment by air traffic control?
taxi = BayAreaDelays[,c(6,9,15,24,31,37:40,42)]
names(taxi)

# arriving flights

trim = transform(trim, inbound =  trim$DEST %in% c("SFO","OAK","SMF"), outbound = trim$ORIGIN %in% c("SFO","OAK","SMF"))	#  inbound vs outbound.


# add in weather data - Rcurl fill form to source data?
## do like thi - cumulative days of shit weather, wind above threshold, test diff thresholds.
