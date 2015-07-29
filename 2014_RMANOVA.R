RM ANOVA

## Example of how the repeated measures ANOVA should be set up
#aov1 = aov(response ~ stimulus*condition + Error(subject/(stimulus*condition)), 
#data=scrd) 

#Q:\Research\All_Projects_by_Species\Astragalus_microcymbus\R_Analysis\R_tables
library(ggplot2)
library(plotrix)
library(plyr)
library(lme4)


setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))
PS<-read.csv("2014_PlotSummary_asmi.csv", header = T, as.is = T)
	

head(PS)
names(PS)

rm(totfruit)

hdd <- read.csv("HeatingDegreeDays_cimerron.csv", header = T, as.is = T)
names(hdd)

hdd.PS <- merge(PS, hdd, by.x = 'Year', by.y = "YEAR.S")
head(hdd.PS)
names(hdd.PS)
names(hdd.PS[,50:56])
hdd.PS$July <- rowSums(hdd.PS[,50:56], na.rm = T)

cor(hdd.PS[complete.cases(hdd.PS),c(4:10,50:62)])

summary(lm(Total.Alive ~ JUL, data = hdd.PS))	#July alone signficant, r^2 = 0.04
summary(lm(Total.Alive ~ July, data = hdd.PS))	#cummulative to July is worse


----------------------------------------------------------------------
# to visualize for the PCA results
# At what levels to cut off years and assign high or low for PCA


totfruit <- ddply(PS,.(Year,Site),summarise, val = mean(Total.Fruit))
colMeans(totfruit)[3]	# 184.94
yearmeans <- ddply(totfruit, .(Year), summarise,
	avgtot = mean(val))
subset(yearmeans, avgtot > colMeans(totfruit)[3])
ggplot(PS, aes(x = factor(Year), y = Total.Fruit, colour = as.factor(Site))) + 
    geom_boxplot() + 
    geom_point(data = totfruit , aes(y = val)) +
 #   geom_line(data = totfruit , aes(y = val, group = Site)) + 
    theme_bw() +
	xlab("YEAR") +
	ylab("Total Fruit") +
	ylim(0,5500)

ggplot(subset(hdd.PS, Year < 2013), aes(Year, July)) +
	geom_point() +
	geom_smooth(method = 'lm')

totfrhdd <- ddply(subset(hdd.PS, Year < 2013),.(July,Site),summarise,val=mean(Total.Fruit))
ggplot(subset(hdd.PS, Year < 2013), aes(x = factor(July), y = Total.Fruit, colour = as.factor(Site))) + 
    geom_boxplot() + 
    geom_point(data = totfrhdd , aes(y = val)) +
    theme_bw() +
	xlab("YEAR") +
	ylab("Total Fruit") +
	ylim(0,5500)
ggplot(subset(hdd.PS, Year <2013), aes(July, Total.Fruit, colour = as.factor(Site))) +
	geom_point() +
	geom_smooth() 

totrepro <- ddply(PS,.(Year,Site),summarise, val = mean(X..Flowered))
colMeans(totrepro)[3]	# .17
yearmeans <- ddply(totrepro, .(Year), summarise,
	avgtot = mean(val))
subset(yearmeans, avgtot > colMeans(totrepro)[3])

ggplot(PS, aes(x = factor(Year), y = X..Flowered, colour = as.factor(Site))) + 
    geom_boxplot() + 
    geom_point(data = totrepro , aes(y = val)) +
 #   geom_line(data = totrepro , aes(y = val, group = Site)) + 
    theme_bw() +
	xlab("YEAR") +
	ylab("Percent Reproductive Individuals") 

which(ddply(totrepro, .(Year), summarise,
	AvgRep = mean(val))[,2] > .4) 	# 16
which(ddply(totrepro, .(Year), summarise,
	AvgRep = mean(val))[,2] > .3) 	# 14, 15
which(ddply(totrepro, .(Year), summarise,
	AvgRep = mean(val))[,2] > .2) 	# 1,3,9,17




ggplot(subset(hdd.PS, Year < 2013), aes(x = July, y = X..Flowered, colour = as.factor(Site))) + 
    geom_point() +
	geom_smooth() +
    theme_bw() +
	xlab("YEAR") +
	ylab("Percent Reproductive Individuals") 


totBr <- ddply(PS,.(Year,Site),summarise, val = mean(X..Browsed))
colMeans(totBr)[3]	# .25
yearmeans <- ddply(totBr, .(Year), summarise,
	avgtot = mean(val))
subset(yearmeans, avgtot > colMeans(totBr)[3])


ggplot(PS, aes(x = factor(Year), y = X..Browsed, colour = as.factor(Site))) + 
    geom_boxplot() + 
    geom_point(data = totBr , aes(y = val)) +
    theme_bw() +
	xlab("YEAR") +
	ylab("Percent Browsed")
ggplot(subset(hdd.PS, Year < 2013), aes(July, X..Browsed, colour = as.factor(Site)))+
    geom_point() +
	geom_smooth() +
    theme_bw() +
	ylab("Percent Browsed")



totD <- ddply(subset(PS, Year > 1995 & Year < 2014),.(Year,Site),summarise, val = mean(X..Dormant))
colMeans(totD)[3]	# 6.23
yearmeans <- ddply(totD, .(Year), summarise,
	avgtot = mean(val))
subset(yearmeans, avgtot > colMeans(totD)[3])


ggplot(PS, aes(x = factor(Year), y = X..Dormant, colour = as.factor(Site))) + 
    geom_boxplot() + 
    geom_point(data = totD , aes(y = val)) +
    geom_line(data = totD , aes(y = val, group = Site)) + 
    theme_bw()

totD <- ddply(PS,.(Year,Site),summarise, val = mean((X..Dormant/Total.Plants)))
ggplot(PS, aes(x = factor(Year), y = X..Dormant, colour = as.factor(Site))) + 
    geom_boxplot() + 
    geom_point(data = totD , aes(y = val)) +
    geom_line(data = totD , aes(y = val, group = Site)) + 
    theme_bw()



# Figure 5
-------------------------------------------------------
#Figure 5c	Figure 8c
PS.sub <- subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013) 

brkdn.plot("Total.Fruit","Fence","Year",data=PS.sub, pch=1:4, 
	col = c("black","slategray4","gray50","gray25"), stagger=0.002, #type="p",
	main="",xlab="Year", ylab="Total Fruit")
abline(v = 2005.5)
abline(v = 2006.5, lty = 2)
legend(2011, 6000, c("Open", "Fenced"), 
	col = c("black","slategray4"), pch=1:2)

# Figure XX browsed
brkdn.plot("X..Browsed","Fence","Year",data=PS.sub, pch=1:2, 
	col = c("black","slategray4"), stagger=0.002, #type="p",
	main="",xlab="Year", ylab="% Browsed")
abline(v = 2005.5)
abline(v = 2006.5, lty = 2)
legend(2011, .8, c("Open", "Fenced"), 
	col = c("black","slategray4"), pch=1:2)

# Figure 5d reproductive	Figure 8d
brkdn.plot("X..Flowered","Fence","Year",data=PS.sub, pch=1:2, 
	col = c("black","slategray4"), stagger=0.002, #type="p",
	main="",xlab="Year", ylab="% Reproductive")
abline(v = 2005.5)
abline(v = 2006.5, lty = 2)
legend(2011, .9, c("Open", "Fenced"), 
	col = c("black","slategray4"), pch=1:2)


# Figure 5b	Figure 8b
brkdn.plot("Total.Plants","Fence","Year",
	data= subset(PS.sub, Year > 1995 & Year < max(Year)), 
	pch=1:4, 
	col = c("black","slategray4","gray50","gray25"), stagger=0.01, 
	main="",xlab="Year", ylab="Total AGG and Dormant")
abline(v = 2005.5)
abline(v = 2006.5, lty = 2)
legend(2009, 45, c("Open", "Fenced"), 
	col = c("black","slategray4"), pch=1:2)

# Figure 5b	Figure 8a
brkdn.plot("Total.Alive","Fence","Year",data=PS.sub, pch=1:4, 
	col = c("black","slategray4"), stagger=0.01, 
	main="",xlab="Year", ylab="# AGG")
abline(v = 2005.5)
abline(v = 2006.5, lty = 2)
legend(max(PS$Year)-3, 67, c("Open", "Fenced"), 
	col = c("black","slategray4"), pch=1:2)


# AGG and dormant
ggplot(subset(PS.sub, Site %in% c("5","15","19","26")), aes(Year, Total.Alive, colour = Fence)) +
	stat_smooth() +
	facet_grid(~Site) +
	theme_bw() +
	stat_smooth(method = "lm", colour = "black")

# AGG
ggplot(subset(PS.sub, Site %in% c("5","15","19","26")), aes(Year, Total.Plants, colour = Fence)) +
	stat_smooth() +
	facet_grid(~Site) +
	theme_bw()+
	stat_smooth(method = "lm", colour = "black")




## T-test for differences after fencing between fenced and open
t.test(sqrt(PS$Total.Fruit[PS$Year > 2005]) ~ as.factor(PS$Fence[PS$Year > 2005]))
plot(sqrt(PS$Total.Fruit[PS$Year > 2005]) ~ as.factor(PS$Fence[PS$Year > 2005]))

t.test(sqrt(PS$X..Flowered[PS$Year > 2005]) ~ as.factor(PS$Fence[PS$Year > 2005]))
plot(sqrt(PS$X..Flowered[PS$Year > 2005]) ~ as.factor(PS$Fence[PS$Year > 2005]))




#Student's T-test of difference in mean

plot(PS$Total.Alive[PS$Year==2011]~as.factor(PS$Fence[PS$Year==2011]))
with(PS, t.test(Total.Alive[Year==2011 & Fence=="y"], 
	Total.Alive[Year==2011 & Fence=="n"]))

with(PS, t.test(Total.Alive[Year==2006 & Fence=="y"], 
	Total.Alive[Year==2006 & Fence=="n"]))

plot(PS$Total.Alive[PS$Year==2007]~as.factor(PS$Fence[PS$Year==2007]))
with(PS, t.test(Total.Alive[Year==2007 & Fence=="y"], 
	Total.Alive[Year==2007 & Fence=="n"]))

hist(asin(sqrt(PS$X..Browsed))*(2/pi))

############### Different in 2014??  ###################
#########################################################
#########################################################
#########################################################
PS$asinBr <- asin(sqrt(PS$X..Browsed))*2/pi
PS$asinFl <- asin(sqrt(PS$X..Flowered))*2/pi
PS$X..Flowered.1[is.na(PS$X..Flowered.1)] <- 0
PS$logFl <- log(PS$X..Flowered.1 +1)
PS$logFr <- log(PS$Total.Fruit +1)

unique(PS$Site)
PS1 <- subset(PS, Site != 1 & Site != 2)

# difference in browsing between fence and no fence
par(mfrow=c(3,7))
for(i in 1995:2014){
#	plot(PS1$asinBrowsed[PS1$Year==i]~as.factor(PS1$Fence[PS1$Year==i]), main=i)
	print(i)
	print(with(PS1, t.test(asinBrowsed[Year==i & Fence=="y"], 
		asinBrowsed[Year==i & Fence=="n"])))}

par(mfrow=c(3,4))
for(i in 1995:2006){
	plot(PS$asinBr[PS$Year==i]~as.factor(PS$Fence[PS$Year==i]), main=i)
	print(i)
	print(with(PS1, t.test(asinBr[Year==i & Fence=="y"], 
		asinBr[Year==i & Fence=="n"])))}

par(mfrow=c(3,4))
for(i in 2004:2014){
	plot(PS$asinBr[PS$Year==i]~as.factor(PS$Fence[PS$Year==i]), main=i)
	print(i)
	print(with(PS1, t.test(asinBr[Year==i & Fence=="y"], 
		asinBr[Year==i & Fence=="n"])))}


for(i in 1995:2014){
#	plot(PS1$X..Browsed[PS1$Year==i]~as.factor(PS1$Fence[PS1$Year==i]), main=i)
	print(i)
	print(with(PS1, t.test(X..Browsed[Year==i & Fence=="y"], 
		X..Browsed[Year==i & Fence=="n"])))}


t.test(asinBr[Year > 2005] ~ Fence[Year > 2005], data = PS1)
plot(PS1$asinBr[PS1$Year > 2005]~ as.factor(PS1$Fence[PS1$Year > 2005]))

t.test(asinBr[Year < 2006] ~ Fence[Year < 2006], data = PS1)
plot(PS1$asinBr[PS1$Year < 2006]~ as.factor(PS1$Fence[PS1$Year < 2006]))


par(mfrow=c(3,7))
for(i in 1995:2014){
	plot(PS1$X..Flowered[PS1$Year==i]~as.factor(PS1$Fence[PS1$Year==i]), main=i)
	print(i)
	print(with(PS1, t.test(X..Flowered[Year==i & Fence=="y"], 
		X..Flowered[Year==i & Fence=="n"])))}

plot(PS$X..Browsed[PS$Year==2007]~as.factor(PS$Fence[PS$Year==2007]))
with(PS, t.test(X..Browsed[Year==2007 & Fence=="y"], 
	X..Browsed[Year==2007 & Fence=="n"]))

par(mfrow=c(1,1))
hist(sqrt(PS1$Total.Fruit))
PS2<-cbind(PS,sqrtFr = sqrt(PS1$Total.Fruit))

par(mfrow=c(3,7))
for(i in 1995:2014){
	plot(PS2$sqrtFr[PS1$Year==i]~as.factor(PS2$Fence[PS1$Year==i]), main=i)
	print(i)
	print(with(PS2, t.test(sqrtFr[Year==i & Fence=="y"], 
		sqrtFr[Year==i & Fence=="n"])))}


TEST <- PS

library(nlme)

TEST1<-subset(TEST, TEST$Year.x<2010)
test3<-lme(Total.Plants.x~ Year.x, random=~Year.x|Site.x/Plot, data=TEST1) #if there were a single fixed effect it would be ~ 1
test3
summary(test3)##starting in 2009 there is no longer a significant decline

test1<-lme(Total.Plants.x~ Year.x, random=~Year.x|Site.x/Plot, data=TEST) #if there were a single fixed effect it would be ~ 1
test1
summary(test1)
plot(test1) ##how to deal with wedge shape http://web.grinnell.edu/individuals/kuipers/stat2labs/Handouts/C3%20April2010a.pdf

test1<-lme(Total.Plants.x~as.factor(Year.x)+as.factor(Site.x) - 1, data=TEST)
test1




#________________________________________________
# Herbivory
# Total.Alive AGG
# Total.Plants AGG and Dormant

PS$asinBr <- asin(sqrt(PS$X..Browsed))*2/pi
PS$asinFl <- asin(sqrt(PS$X..Flowered))*2/pi
PS$X..Flowered.1[is.na(PS$X..Flowered.1)] <- 0
PS$logFl <- log(PS$X..Flowered.1 +1)
PS$logFr <- log(PS$Total.Fruit +1)


PS.merge <- subset(merge(PS, PS, by = c('Site','Plot')), Year.x == Year.y -1)
head(PS.merge)

# Total.Alive.y is AGG in the next year from .x
summary(step(lm(Total.Alive.y ~ (asinBr.x + asinBr.y +
		asinFl.x + asinFl.y)^2 , data = PS.merge)))


summary(lm(Total.Alive.y ~ asinBr.y, data = PS.merge))
ggplot(PS.merge, aes(asinBr.y, Total.Alive.y))+
	geom_point() +
	stat_smooth(method = 'lm')
summary(lm(Total.Alive.y ~ asinBr.x, data = PS.merge))






















