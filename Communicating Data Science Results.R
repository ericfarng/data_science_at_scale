sanfran <- read.csv("sanfrancisco_incidents_summer_2014.csv", as.is=TRUE)

head(sanfran)
sort(table(sanfran$Category))

sanfran$Time_num <- sapply(strsplit(sanfran$Time,":"),
   function(x) {
   	x <- as.numeric(x)
   	x[1]+x[2]/60
   }
)

sanfran$Time_cat <- trunc(sanfran$Time_num / 4)
table(sanfran$Time_cat)

t1 <- table(sanfran[sanfran$Category=='LARCENY/THEFT',]$Time_cat)/sum(sanfran$Category=='LARCENY/THEFT')
t2 <- table(sanfran[sanfran$Category=='OTHER OFFENSES',]$Time_cat)/sum(sanfran$Category=='OTHER OFFENSES')
t3 <- table(sanfran[sanfran$Category=='NON-CRIMINAL',]$Time_cat)/sum(sanfran$Category=='NON-CRIMINAL')
t4 <- table(sanfran[sanfran$Category=='ASSAULT',]$Time_cat)/sum(sanfran$Category=='ASSAULT')
t5 <- table(sanfran[sanfran$Category=='VEHICLE THEFT',]$Time_cat)/sum(sanfran$Category=='VEHICLE THEFT')
t6 <- table(sanfran[sanfran$Category=='WARRANTS',]$Time_cat)/sum(sanfran$Category=='WARRANTS')
t7 <- table(sanfran[sanfran$Category=='DRUG/NARCOTIC',]$Time_cat)/sum(sanfran$Category=='DRUG/NARCOTIC')
t8 <- table(sanfran[sanfran$Category=='SUSPICIOUS OCC',]$Time_cat)/sum(sanfran$Category=='SUSPICIOUS OCC')
t9 <- table(sanfran[sanfran$Category=='MISSING PERSON',]$Time_cat)/sum(sanfran$Category=='MISSING PERSON')

max <- 24 / 4 - 1
plot(0:max, t1, type="l", col=2, axes=FALSE,
	 ylab="Percent of Category", ylim=c(0,0.35),
	 xlab="Hour of the Day", main="Percent of Crime by Time of Day")
axis(2, at=(0:7)/100*5, tick=TRUE)
axis(1, at=0:max, tick=TRUE, labels=c("12-4AM", "4-8AM", "8-12PM", "12-4PM", "4-8PM", "8PM-12AM"))
abline(h=(0:7)/100*5, lty=3, col=1)
lines(0:max, t2, type="l", col=3, ylab="")
lines(0:max, t3, type="l", col=4, ylab="")
lines(0:max, t4, type="l", col=5, ylab="")
lines(0:max, t5, type="l", col=6, ylab="")
lines(0:max, t6, type="l", col=7, ylab="")
lines(0:max, t7, type="l", col=2, ylab="", lty=2)
lines(0:max, t8, type="l", col=3, ylab="", lty=2)
lines(0:max, t9, type="l", col=4, ylab="", lty=2)

legend("topleft", 
	   c("Larceny/Theft", "Other", "Non-criminal", "Assault", "Vehicle Theft", 
	     "Warrants", "Drug/Narcotic", "Suspicious Occ", "Missing Person"),
	   fill=c(2, 3, 4, 5, 6, 7, 2, 3, 4),
	   lty=c(1, 1, 1, 1, 1, 1, 2, 2, 2)
)
	   
	   