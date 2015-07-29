#Step biotic factors 
library(nlme)
library(reshape)
library(reshape2)
library(plyr)
library(MARSS)

rm(asmi)

setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))

asmi<-read.csv("2014_PlotSummary_asmi.csv", as.is=T,header=TRUE)	
names(asmi)

asmi1 <- asmi
asmiTest <- subset(merge(asmi1, asmi1, by = "Plot", sort = FALSE), Year.x == Year.y + 1) #makes year.y the year before
names(asmiTest)
asmiTest[300:340,1:6]
head(asmiTest)
TEST<-asmiTest[order(asmiTest$Plot,asmiTest$Year.x),]	#X current year, Y previous year
	
par(mfrow=c(2,2))
hist(log(TEST$Total.Plants.x+1))
hist(log(TEST$Total.Fruit.x+1)) #without +1 excludes 0s and looks good
hist(TEST$X..Browsed.x)
hist(asin(sqrt(TEST$X..Browsed.y))*(2/pi))
hist(log(TEST$X..Flowered.1.y+1))
hist(log(TEST$X..Flowered.y+1))
hist(log(TEST$X..Dormant.x+1))
hist(log((TEST$Total.Fruit.x+1)^(1/3)))
hist(log(TEST$Total.Fruit.x+1))
hist(asin(sqrt(TEST$X..Flowered.y))*(2/pi))

TEST$X..Flowered.1.y[is.na(TEST$X..Flowered.1.y)] <- 0

TEST<-cbind(TEST, asinBr.x = c(asin(sqrt(TEST$X..Browsed.x))*(2/pi)),
		asinBr.y = c(asin(sqrt(TEST$X..Browsed.y))*(2/pi)), 
		logTotalFl.x = c(log(TEST$X..Flowered.1.x+1)),
		logTotalFl.y = c(log(TEST$X..Flowered.1.y+1)),
		logTotalAl.x = log(TEST$Total.Alive.x+.5),	#Total.Alive are AGG
		logTotalAl.y = log(TEST$Total.Alive.y+.5),
		logDorm.x = log(TEST$X..Dormant.x+1),
		logDorm.y = log(TEST$X..Dormant.y+1),
		logFr.x = log(TEST$Total.Fruit.x+1),
		logFr.y = log(TEST$Total.Fruit.y+1),
		logPercentFl.x = c(asin(sqrt(TEST$X..Flowered.x))*(2/pi)),
		logPercentFl.y = c(asin(sqrt(TEST$X..Flowered.y))*(2/pi)))
names(TEST)

_______________________________________
#Correlation table instead of linear models where Y keeps swapping out

TEST$logTotalFl.x[is.na(TEST$logTotalFl.x)] <- 0
TEST$logPercentFl.y

cor(TEST[,c(101:109)])

names(TEST[,c(2,50,4,5,6,7,8,9,10,102,103,98,100,105,106,109,108)])	#Total.Alive = AGG, Total.Plants = dormant and AGG
head(TEST[,c(2,50,4,5,102,103,98,100,105,106,109,108)])	#.x is year t, .y is year t-1
cor(TEST[,c(102,103,98,100,105,106,109,108)])

library(psych)
corr.test(TEST[,c(102,103,98,100,105,106,109,108)])
corr.p(cor(TEST[,c(102,103,98,100,105,106,109,108)]), n = 8)


# 4 factors
cor(TEST[,c(102,98,100,105,106)])


TEST$X..Flowered.1.x
TEST[is.na(TEST)] <- 0

head(TEST)
cor(TEST[,c(102,103,98,100,105,106,109,108)])






_________________________________________________________________
# total AGG


lmAGG<-lm(logTotalAl.x~ #log(Total.Plants.y+1)
	+ asinBr.x
#	+ logTotalFl.x
	+ logDorm.x
	+ logFr.x
	+ logPercentFl.y
	, 
	data=TEST)
lmAllstep<-step(lmAGG, log(TEST$Total.Alive.x+.5)~1)
summary(lmAllstep)

plot(TEST$logDorm.y, TEST$logTotalAl.x)
abline(lm(TEST$logTotalAl.x~TEST$logDorm.y))

plot(TEST$logTotalFl.y, TEST$logTotalAl.x)
abline(lm(TEST$logTotalAl.x~TEST$logTotalFl.y))

ggplot(TEST, aes(logTotalAl.x,logTotalFl.y))+
	geom_point() +
	geom_smooth(method = 'lm')

ggplot(TEST, aes( logTotalAl.x, logFr.y)) +
	geom_point() +
	geom_smooth(method = 'lm')

plot(TEST$logFr.y, TEST$logTotalAl.x)
abline(lm(TEST$logTotalAl.x~TEST$logFr.y))


lmAGG <- lm(logTotalAl.x ~ 		# .y is the previous year; .x is the current year
		(asinBr.x + asinBr.y)^2,
		data = TEST)
summary(lmAGG)

lmAGG <- lm(logTotalAl.x ~ asinBr.x ,
		data = TEST)
summary(lmAGG)
plot(TEST$asinBr.x, TEST$logTotalAl.x,
	xlab = "Percent Browsed (arcsin transformed)",
	ylab = "Total AGG (log transformed)")
lmAGG2 <- lm(logTotalAl.x ~ asinBr.x^2 ,
		data = TEST)
summary(lmAGG2)
plot(TEST$asinBr.x, TEST$logTotalAl.x,
	xlab = "Percent Browsed (arcsin transformed)",
	ylab = "Total AGG (log transformed)")		#more browsing more plants?


midL <- unique(ave(unique(TEST$asinBr.x)))-unique(sd(unique(TEST$asinBr.x)))	
# 0.250595
midH <- unique(ave(unique(TEST$asinBr.x)))+unique(sd(unique(TEST$asinBr.x)))
# [1] 0.6659951

lmAGGl <- lm(logTotalAl.x ~ 		# .y is the previous year; .x is the current year
		asinBr.x,
		data = subset(TEST, TEST$asinBr.x < midL))
summary(lmAGGl)

lmAGGm <- lm(logTotalAl.x ~ 		# .y is the previous year; .x is the current year
		asinBr.x,
		data = subset(TEST, 
			TEST$asinBr.x >= midL & TEST$asinBr.x <= midH))
summary(lmAGGm)

lmAGGh <- lm(logTotalAl.x ~ 		# .y is the previous year; .x is the current year
		asinBr.x,
		data = subset(TEST, 
			TEST$asinBr.x > midH))
summary(lmAGGh)

## From the plot, it looks like it changes at %50 herbivory
lmAGG.5 <- lm(logTotalAl.x ~ 		# .y is the previous year; .x is the current year
		asinBr.x,
		data = subset(TEST, 
			TEST$asinBr.x > 0.5 & TEST$asinBr.x > 0 & TEST$asinBr.x < 1))
summary(lmAGG.5)

lmAGG.0 <- lm(logTotalAl.x ~ 		# .y is the previous year; .x is the current year
		asinBr.x,
		data = subset(TEST, 
			TEST$asinBr.x <= 0.5 & TEST$asinBr.x > 0 & TEST$asinBr.x < 1))
summary(lmAGG.0)


______________________________________________________
#Percent  Reproductive


lmTR<-lm(logPercentFl.x~ 
	+ asinBr.x
#	+ logTotalAl.y
	+ logDorm.x
	+ logFr.x
	+ logTotalAl.x
	, 
	data=TEST)
lmAllstep<-step(lmTR, logPercentFl~1)
summary(lmAllstep)

plot(TEST$logTotalAl.x, TEST$logPercentFl.x)
abline(lm(TEST$logPercentFl.x~TEST$logTotalAl.x))
summary(lm(TEST$logPercentFl.x~TEST$logTotalAl.x))

plot(TEST$asinBr.x, TEST$logPercentFl.x)
abline(lm(TEST$logPercentFl.x~TEST$asinBr.x))
summary(lm(TEST$logPercentFl.x~TEST$asinBr.x))

______________________________________________________
#Total Fruit


lmTF<-lm(logFr.x~ 
	+ asinBr.x
	+ logTotalAl.x
	+ logDorm.x
	+ logPercentFl.y
	, 
	data=TEST)
lmAllstep<-step(lmTF, TEST$logFr~1)
summary(lmAllstep)

plot(TEST$logDorm.y, TEST$logFr.x)
abline(lm(TEST$logFr.x~TEST$logDorm.y))

plot(TEST$logPercentFl.y, TEST$logFr.x)
abline(lm(TEST$logFr.x~TEST$logPercentFl.y))


______________________________________________________
#Total Dormant


lmTD<-lm(logDorm.x~ 
	+ asinBr.x
	+ logTotalAl.x
	+ logFr.x
	+ logPercentFl.y
	, 
	data=TEST)
lmAllstep<-step(lmTD, TEST$logFr~1)
summary(lmAllstep)

plot(TEST$logTotalAl.y, TEST$logDorm.x)
abline(lm(TEST$logDorm.x~TEST$logTotalAl.y))


plot(TEST$logFr.y, TEST$logDorm.x)
abline(lm(TEST$logDorm.x~TEST$logFr.y))


plot(TEST$logPercentFl.y, TEST$logDorm.x)
abline(lm(TEST$logDorm.x~TEST$logPercentFl.y))


plot(TEST$asinBr.x, TEST$logDorm.x)
abline(lm(TEST$logDorm.x~TEST$asinBr.x))
	summary(lm(TEST$logDorm.x~TEST$asinBr.x))






###########
# Long lived?
##	Download from Raw Data years alive with 1 through 20 years dormant


setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/Asmi_Excel/Yearly Summaries/2014_asmi/LimitYearsDormant_RawData_2014"))

dormancy <- data.frame()
for(i in 1:18){
	rm(d.i)
	d.i <- read.csv(paste(i,"yr_2014.csv", sep = ""), as.is=TRUE,
	na.strings="NA", header=TRUE)	
	names(d.i) <- c("YrsDormant","tagid","TagNo","Plot","Site",
		"StartYear","DeadYear","YearsAlive","Fence")
	d.i <- data.frame(d.i, YrsAllowedDorm = paste(i))
	print(tail(d.i))
	dormancy <- rbind(dormancy,d.i)
}


head(dormancy)
unique(dormancy$DeadYear)
dormancy <- dormancy[complete.cases(dormancy[,2:4]),]
str(dormancy)
dormancy <- subset(dormancy, DeadYear != 2014 & Site != 1 & Site != 2)
dormancy[43500:43520,]
str(dormancy)
unique(dormancy$YrsAllowedDorm)
unique(dormancy$Fence)
unique(dormancy$YearsAlive)

	## There are cases where the plants are alive in the current year so don't
	##	have an 'EndYear' and thus NA for 'EndYear' and for 'Yearsalive'

dormancy$YrsAllDorm <- as.numeric(levels(dormancy$YrsAllowedDorm))[dormancy$YrsAllowedDorm]
str(dormancy)

library(plyr)
library(plotrix)
library(ggplot2)

##	Average life length when dormancy is limited
lifelength <- ddply(dormancy, .(YrsAllDorm,Plot,YrsDormant), summarise, 
	Individs = length(tagid),
	MaxLife = max(YearsAlive),
	AverageLifeLength = mean(YearsAlive, na.rm = T),
	SELifeLength = std.error(YearsAlive, na.rm = T))

lifelength <- ddply(dormancy, .(YrsAllDorm), summarise, 
	Individs = length(tagid),
	MaxLife = max(as.numeric(YearsAlive)),
	AverageLifeLength = mean(as.numeric(YearsAlive), na.rm = T),
	SELifeLength = std.error(as.numeric(YearsAlive), na.rm = T))

plot(lifelength$YrsAllDorm, lifelength$AverageLifeLength, 
	type = "l",
	xlab = "Maximum years dormant",
	ylab = "Life length",
	ylim = c(2.5,4.5))
	)
abline(h = max(lifelength$AverageLifeLength, na.rm = T),
	lty = 2, col = "grey50")


library(gplots)
plotmeans(as.numeric(dormancy$YearsAlive) ~ 
		dormancy$YrsAllDorm, 
	ylim = c(2.5,4.5),
	ylab = "Life length",
	xlab = "Maximum years dormant",
	n.label = F)

# one standard error from the mean, confidence limits based on the t-distribution
ggplot(dormancy, aes(YrsAllDorm, as.numeric(YearsAlive))) + #, colour = as.factor(Site)
	stat_summary(fun.y = mean, geom = 'line') +
	stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1) +
	ylab("Life length") +
	xlab("Maximum years dormant")

ggplot(dormancy, aes(YrsAllDorm, as.numeric(YearsAlive), colour = as.factor(Site))) + 
	stat_summary(fun.y = mean, geom = 'line') +
	stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1) +
	labs(colour = "Site") +
	ylab("Life length") +
	xlab("Maximum years dormant")

## Maximum life length when dormancy is limitied
lifelengthMax <- ddply(dormancy, .(YrsAllDorm ), summarise, 
	MaxLifeLength = max(as.numeric(YearsAlive), na.rm = T))




# how long was dormancy?

unlim <- subset(dormancy, dormancy$YearsDormant == 18)

str(unlim)

table(

















