## use: 
#	Q:\Research\All_Projects_by_Species\Astragalus_microcymbus\
#	Asmi_Manuscript\PlotSummaries_2011_mintemp_1

library(sciplot)
library(gplots)
library(ggplot2)
library(plotrix)
library(plyr)
library(Hmisc)
library(lme4)


setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))
PS<-read.csv("2014_PlotSummary_asmi.csv", header = T, as.is = T)
	
names(PS)
PS[100:120,]

table(PS$Plot, PS$Site)
table(PS$Plot[PS$Total.Plants > 0], PS$Year[PS$Total.Plants > 0])
	# Plot 238 and 300 were added in 1996
	# Plot 598 was added in 2004
	# 1,16,22,32,42,52,58,89,90,96,98 in Cebolla Mid (1), and Cebolla North (2) added in 2014


PStest <- subset(PS, PS$Plot != 238 & PS$Plot != 300 & PS$Plot != 598 & PS$Site != 1 & PS$Site != 2 | 
		PS$Plot == 238 & PS$Year > 1995 |
		PS$Plot == 300 & PS$Year > 1995 |
		PS$Plot == 598 & PS$Year > 2003 |
		PS$Site == 1 & PS$Year > 2013 |
		PS$Site == 2 & PS$Year > 2013)
table(PStest$Plot[PStest$Total.Plants > 0], PStest$Year[PStest$Total.Plants > 0])

unique(PS$Site)
unique(PS$Year)
head(PS)
head(PStest)

# Table 3, Fruit per plot, standard errors in parentheses

write.table(ddply(PStest, .(Site,Year), summarise,
	AvgFr = mean(Total.Fruit),
	SEFr = std.error(Total.Fruit)), "clipboard", sep="\t", row.names = T, col.names = T)

# How many years there was no Fruit
ddply(ddply(PStest, .(Site,Year), summarise,
	AvgFr = mean(Total.Fruit),
	SEFr = std.error(Total.Fruit)), .(Site), summarise,
		NoReproYrs = length(AvgFr[AvgFr == 0])/length(1995:2014),
		NumYrs = length(AvgFr[AvgFr == 0]))

# Table 1a, Average AGG individual

write.table(ddply(PStest, .(Year,Site), summarise,
	AvgTotAlive = mean(Total.Alive),
	SETotAlive = std.error(Total.Alive)), "clipboard", sep="\t", row.names = T, col.names = T)

AGG <- ddply(PStest, .(Year,Site), summarise,
	AvgTotAlive = mean(Total.Alive),
	SETotAlive = std.error(Total.Alive))

# "On average, the population size at all sites contracted over half the years studied ...."
colMeans(ddply(ddply(AGG, .(Site), summarise,
	AnnDiffs = diff(AvgTotAlive)), .(Site), summarise,
		PercentContracted = length(AnnDiffs[AnnDiffs < 0])/20))	# 20 is the number of years

totalive <- ddply(PStest,.(Year,Site), summarise,
	TotAlive = sum(Total.Alive))
colMeans(ddply(totalive, .(Site), summarise,
	Perof1996 = TotAlive[Year == 2014]/TotAlive[Year == 1996]))
colMeans(ddply(totalive, .(Site), summarise,
	Perof1996 = TotAlive[Year == 2014]/TotAlive[Year == 1995]))

# Table 1a, Total Individuals 


AGG.dorm <- ddply(PStest, .(Year,Site), summarise,
	AvgTotAlive = mean(Total.Plants),
	SETotAlive = std.error(Total.Alive))

reshape(AGG.dorm, idvar = "Year", timevar = "Site", direction = "wide")

write.table(ddply(PStest, .(Year,Site), summarise,
	AvgTotAlive = mean(Total.Plants),
	SETotAlive = std.error(Total.Alive)), "clipboard", sep="\t", row.names = T, col.names = T) 

write.table(reshape(AGG.dorm, idvar = "Year", timevar = "Site", direction = "wide"),
	 "clipboard", sep="\t", row.names = T, col.names = T) 

# Table 1b, Percent reproductive individuals

Per.repro <- ddply(PStest, .(Year,Site), summarise,
	AvgPerRepro = mean(X..Flowered),
	SEPerRepro = std.error(X..Flowered))

write.table(reshape(Per.repro, idvar = "Year", timevar = "Site", direction = "wide"), 
	"clipboard", sep="\t", row.names = T, col.names = T) 

# Table 1b, Average total reproductive

Tot.repro <- ddply(PStest, .(Year,Site), summarise,
	AvgTotRepro = mean(X..Flowered.1, na.rm = T),
	SETotRepro = std.error(X..Flowered.1, na.rm = T))

write.table(reshape(Tot.repro, idvar = "Year", timevar = "Site", direction = "wide"), 
	"clipboard", sep="\t", row.names = T, col.names = T) 

# How many years there were no reproductive individuals
ddply(Tot.repro, .(Site), summarise,
		NoReproYrs = length(AvgTotRepro[is.nan(AvgTotRepro)])/length(1995:2014),
		NumYrs = length(AvgTotRepro[is.nan(AvgTotRepro)]))

mean(c(4,5,3,3))




lineplot.CI(PS$Year, PS$Total.Fruit, err.width = 0.05, xlab="Year", 
	ylab="Total Fruit")
par(new=TRUE)
lineplot.CI(PS$Year, PS$X..Dormant, col = "gray",
	err.width = 0.05, xlab="", ylab="", yaxt = "n")

ggplot(subset(PS, Total.Alive > 0), aes(Year, Total.Plants)) +
	geom_point() +
#	stat_smooth(method = 'lm') +
#	geom_smooth(method = 'lm', formula = y ~ poly(x, 2), colour = 'red') +
	geom_smooth(method = 'lm', formula = y ~ poly(x, 3), colour = 'purple') +
	facet_grid(~ Site) +
	ylab("Individuals per plot")

ggplot(PS) +
	geom_smooth(method = 'lm', colour = 'red', aes(Year, AnSnow)) +
	ylab("Annual Snowfall")

ggplot(PS) +
	geom_smooth(method = 'lm', colour = 'orange', aes(Year, FebMaxTemp)) +
	ylab("Temperature")


plotmeans(PS$X..Dormant~PS$Year, 
	connect=list(2:18),
	col = "gray", xlab="", ylab="",
	barcol="gray",n.label=FALSE )

par(cex=1)

-------------------------------------------------------------

## AGG and Dormant
# Beaver Creek only
plotmeans(Total.Plants ~ Year, data = subset(PS, Site != 1 & Site != 2),
	connect = list(2:19),
	xlab = "Year",
	barcol = "black",
	col = "black",
	ylab = "AGG and Dormant individuals per plot")

#Cebolla Creek
plotmeans(Total.Plants ~ Year, 
	data = subset(PS, Year > 2013 & Site == 1 | Year > 2013 & Site == 2),
	xlab = "Year",
	barcol = "black",
	col = "black",
	ylab = "AGG and Dormant individuals per plot")


library(nlme)

# Repeated measures ANOVA
anova(lme(log(Total.Plants+1) ~ Year*Site, random = ~ 1 | Plot, data = PS))

ggplot(data = PS) +
	geom_histogram(aes(x = Total.Plants), alpha = 0.5) +
	facet_wrap( ~ Site)

ggplot(data = PS) +
	geom_histogram(aes(x = log(Total.Plants+1)), alpha = 0.5) +
	facet_wrap( ~ Site)

for(i in unique(PS$Site)){
	print(i)
	PS1 <- subset(PS, Site == i & Year > 1995 & Year < 2014)
	print(summary(lm(log(Total.Plants+1) ~ Year, data = PS1)))
}

summary(lm(log(Total.Plants+1) ~ Year, 
	data = subset(PS, Year > 1995 & Year < 2013)))


for(i in unique(PS$Site)){
	print(i)
	PS1 <- subset(PS, Site == i & Year > 1995 & Year < 2013)
	print(summary(lm(Total.Plants ~ Year, data = PS1)))
}

summary(lm(Total.Plants ~ Year, 
	data = subset(PS, Year > 1995 & Year < 2014)))



# Not transformed data - AGG and Dormant
ggplot(subset(PS, Year > 1995 & Year < 2014 & Site != 1 & Site != 2),
		 aes(x = Year, y = Total.Plants)) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("AGG and Dormant") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1) +
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(subset(PS, Year > 1995 & Year < 2014& Site != 1 & Site != 2),
		 aes(x = Year, y = log(Total.Plants+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("AGG and Dormant") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(subset(PS, Year > 1995 & Year < 2014), aes(x = Year, y = log(Total.Plants+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("AGG and Dormant") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(subset(PS, Year > 1995 & Year < 2014), aes(x = Year, y = log(Total.Plants+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("AGG and Dormant") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

	
--------------------------
## AGG only
plotmeans(PS$Total.Alive ~ PS$Year,
	connect = list(1:20),
	xlab = "Year",
	barcol = "black",
	col = "black",
	ylab = "AGG per plot")

# Beaver Creek only
plotmeans(Total.Alive ~ Year,
	data = subset(PS, Site != 1 & Site != 2),
	connect = list(1:20),
	xlab = "Year",
	barcol = "black",
	col = "black",
	ylab = "AGG per plot")

# Repeated measures ANOVA
anova(lme(log(Total.Alive+1) ~ Year*Site, random = ~ 1 | Plot, data = PS))

ggplot(data = PS) +
	geom_histogram(aes(x = Total.Alive), alpha = 0.5) +
	facet_wrap( ~ Site)

ggplot(data = PS) +
	geom_histogram(aes(x = log(Total.Alive+1)), alpha = 0.5) +
	facet_wrap( ~ Site)


# Not transformed data
ggplot(PS, aes(x = Year, y = Total.Alive)) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("AGG") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(subset(PS, Site != 1 & Site != 2),
		 aes(x = Year, y = log(Total.Alive+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("AGG") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

# Beaver Creek only
ggplot(subset(PS, Site != 1 & Site != 2),
		 aes(x = Year, y = log(Total.Alive+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
#	facet_grid(facets = ~ Site) +
	ylab("AGG") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))


ggplot(subset(PS, Site != 1 & Site != 2),
		 aes(x = Year, y = Total.Alive)) +
	opts(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("AGG and Dormant") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))


for(i in unique(PS$Site)){
	print(i)
	PS1 <- subset(PS, Site == i & Year > 1995 & Year < 2014)
	print(summary(lm(log(Total.Alive+1) ~ Year, data = PS1)))
}

summary(lm(log(Total.Alive+1) ~ Year, 
	data = subset(PS, Year > 1995 & Year < 2013)))



--------------------------
## Dormant only
plotmeans((Total.Plants - Total.Alive) ~ Year,
	data = subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013),
	connect = list(2:19),
	xlab = "Year",
	barcol = "black",
	col = "black",
	ylab = "Dormant per plot")

# Repeated measures ANOVA
anova(lme(log((Total.Plants-Total.Alive)+1) ~ Year*Site, random = ~ 1 | Plot, 
	data = subset(PS, Year > 1995 & Year < 2013)))


ggplot(data = subset(PS, Year > 1995 & Year < 2013)) +
	geom_histogram(aes(x = Total.Plants - Total.Alive), alpha = 0.5) +
	facet_wrap( ~ Site)

ggplot(data = subset(PS, Year > 1995 & Year < 2013)) +
	geom_histogram(aes(x = log((Total.Plants - Total.Alive)+1)), alpha = 0.5) +
	facet_wrap( ~ Site)


# Not transformed data
ggplot(subset(PS, Year > 1995 & Year < 2014), aes(x = Year, y = (Total.Plants - Total.Alive))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("Dormant") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(subset(PS, Year > 1995 & Year < 2014), aes(x = Year, y = log((Total.Plants - Total.Alive)+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	facet_grid(facets = ~ Site) +
	ylab("Dormant") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

	

ggplot(subset(PS, Year > 1995 & Year < 2013), aes(x = Year, y = (Total.Plants - Total.Alive))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("Dormant") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(PS, aes(x = Year, y = Total.Alive)) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("AGG") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(subset(PS, Year > 1995 & Year < 2013), aes(x = Year, y = Total.Plants)) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("AGG and Dormant") +
#	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))



for(i in unique(PS$Site)){
	print(i)
	PS1 <- subset(PS, Site == i & Year > 1995 & Year < 2013)
	print(summary(lm(log((Total.Plants - Total.Alive)+1) ~ Year, data = PS1)))
}

summary(lm(log((Total.Plants - Total.Alive)+1) ~ Year, 
	data = subset(PS, Year > 1995 & Year < 2013)))


# All sites
ggplot(subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), aes(x = Year, y = log(Total.Plants+1))) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("AGG and Dormant") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))


ggplot(subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), aes(x = Year, y = Total.Plants)) +
	theme(panel.grid.major = element_blank(),
		panel.background = element_blank()) +	
	ylab("AGG and Dormant") +
	stat_smooth(method = lm) +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))




-------------------------------------------------------------
# climate and count

ggplot(subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), aes(x = (NovRain*DecRain*OctRain)/3, y = Total.Plants)) +
	stat_smooth(method = lm) +
	geom_point() +
	xlab("Average Winter Precipitation") +
	ylab("Above ground and dormant individuals")

ggplot(subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), aes(x = (NovRain*DecRain*OctRain)/3, y = Total.Alive)) +
#	stat_smooth(method = lm) +
#	geom_point() +
	xlab("Average Winter Precipitation") +
	ylab("Above ground growth") +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1, size = 1.5)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))


ggplot(PS, aes(x = ((NovRain*DecRain*OctRain)/3)*((AugMinTemp*SeptMinTemp*OctMinTemp)/3),
			 y = Total.Alive)) +
#	stat_smooth(method = lm) +
#	geom_point() +
	xlab("Average Fall Temperature and Winter Precipitation") +
	ylab("Above ground growth") +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1, size = 1.5)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))

ggplot(PS, aes(x = ((AugMinTemp*SeptMinTemp*OctMinTemp)/3),
			 y = Total.Alive)) +
#	stat_smooth(method = lm) +
#	geom_point() +
	xlab("Average Fall Temperature") +
	ylab("Above ground growth") +
	stat_summary(fun.y = mean, geom = "line") +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult = 1, size = 1.5)+
	geom_errorbar(stat = "hline", yintercept = "mean", col = "grey",
		width = 1, aes(ymax = ..y.., ymin = ..y..))




-------------------------------------------------------------
#Fruit and Annual Growth

plotmeans(Total.Fruit~Year, xlab="Year", 
	data = subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013),
	ylab="Total Fruit",barcol="black",n.label=FALSE )
par(new=TRUE,mar = c(5, 4, 4, 4))
plotmeans(X..Dormant~Year, 
	data = subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013),
	connect=list(2:18), 
	pch=4,
	col = "slategray4", xaxt = "s", yaxt = "n", xlab="", ylab="",
	barcol="slategray4",n.label=FALSE)
axis(4, at=c(0,5,10,15))
axis(4, line = 1.5, at = 10, labels = "Total Dormant", tick = FALSE, 
	col = "slategray4")
par(new=TRUE)
plot(c(1,10),c(1,10), type="n", xaxt='n', yaxt='n', ann=FALSE)
legend(9, 10, c("Fruit","Dormant"), col=c("black","slategray4"),
	pch=c(1,4), cex = 0.7)



----------------------------------------------------
#Annual growth

plotmeans(Total.Fruit~Year, xlab="Year", 
	data = subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013),
	ylab="Total Fruit",barcol="black",n.label=FALSE )
par(new=TRUE,mar = c(5, 4, 4, 4))
plotmeans(PS$Total.Alive~PS$Year, 
	data = subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), 
	pch=4,
	col = "slategray4", xaxt = "s", yaxt = "n", xlab="", ylab="",
	barcol="slategray4",n.label=FALSE)
axis(4, at=c(0,10,20,30,40,50,60))
axis(4, line = 1.5, at = 20.5, labels = "Total Annual Growth", tick = FALSE, 
	col = "slategray4")
par(new=TRUE)
plot(c(1,10),c(1,10), type="n", xaxt='n', yaxt='n', ann=FALSE)
legend(8.35, 10, c("Fruit","Annual Growth"), col=c("black","slategray4"),
	pch=c(1,4), cex = 0.7)


ggplot(subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), 
	aes(x = Year, y = Total.Fruit, colour = as.factor(Site))) +
	stat_summary(fun.y = mean, geom = 'line', aes(group = Site)) +
	stat_summary(fun.data = mean_cl_boot, conf.int = .95, B = 5000, 
		geom = 'errorbar', width = 0.2) +
	ylab('Total Fruit')

ggplot(subset(PS, Plot != 238 & Plot != 300 & Plot != 598 & 
			Site != 1 & Site != 2 | 
		Plot == 238 & Year > 1995 |
		Plot == 300 & Year > 1995 |
		Plot == 598 & Year > 2003 |
		Site == 1 & Year > 2013 |
		Site == 2 & Year > 2013), 
	aes(x = Year, y = Total.Alive, colour = as.factor(Site))) +
	stat_summary(fun.y = mean, geom = 'line', aes(group = Site)) +
	stat_summary(fun.data = mean_cl_boot, conf.int = .95, B = 5000, 
		geom = 'errorbar', width = 0.2) +
	ylab('Total AGG')



----------------------------------------------------
#Annual growth and dormant

plotmeans(PS$Total.Fruit~PS$Year, xlab="Year", 
	ylab="Total Fruit",barcol="black",n.label=FALSE )
par(new=TRUE,mar = c(5, 4, 4, 4))
plotmeans(PS$Total.Alive~PS$Year, 
	pch=4,
	col = "slategray4", xaxt = "s", yaxt = "n", xlab="", ylab="",
	barcol="slategray4",n.label=FALSE)
axis(4, at=c(0,10,20,30,40,50,60))
axis(4, line = 1.5, at = 32.5, labels = "Total Annual Growth", tick = FALSE, 
	col = "slategray4")
par(new=TRUE)
plot(c(1,10),c(1,10), type="n", xaxt='n', yaxt='n', ann=FALSE)
legend(8.35, 10, c("Fruit","Annual Growth"), col=c("black","slategray4"),
	pch=c(1,4), cex = 0.7)



------------------------------------------------------------------------------------
#	Table summary data
library(plyr)

## This has zeros in the plots that weren't there yet, changes averages 
write.table(ddply(PS_all, .(Year,Site), summarise, val = mean(Total.Alive)), 
	"clipboard", sep="\t",
	row.names = F, col.names = T)	#total alive and dormant

PS.narm <- PS
PS.narm$X..Flowered.1[PS$X..Flowered == 0] <- 0

subset(ddply(PS, .(Site, Year), summarise, val = sum(Total.Alive)),
	Year == 2013)

# Table 1(a), AGG and SE
write.table(	ddply(PS, .(Site, Year), summarise, val = mean(Total.Alive))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	#total alive and dormant
	#total AGG
 write.table(	ddply(PS, .(Site, Year), summarise, 
	val = (sd(Total.Alive, na.rm = TRUE)/sqrt(length(Total.Alive))))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	#total alive and dormant
	#se total AGG

# Table 1(a), AGG and Dormant and SE
write.table(		
ddply(PS, .(Site, Year), summarise, val = mean(Total.Plants))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	#total alive and dormant
 write.table(	ddply(PS, .(Site, Year), summarise, 
	val = (sd(Total.Alive, na.rm = TRUE)/sqrt(length(Total.Plants))))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	#total alive and dormant
	#se 

# Table 1(b), Percent flowered and SE	X..Flowered is the %
write.table(ddply(PS.narm, .(Site, Year), summarise, val = mean(X..Flowered))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	
	#percent flowered
 write.table(ddply(PS.narm, .(Site, Year), summarise, 
	val = (sd(X..Flowered, na.rm = TRUE)/sqrt(length(X..Flowered))))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	
	#se 

	t.test(PS$X..Flowered[PS$Year > 2005] ~ PS$Fence[PS$Year > 2005])


# Table 1(b), total flowered and SE		X..Flowered.1 is the total flowered
write.table(ddply(PS.narm, .(Site, Year), summarise, val = mean(X..Flowered.1))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	
	#total indivduals that flowered
 write.table(ddply(PS.narm, .(Site, Year), summarise, 
	val = (sd(X..Flowered.1, na.rm = TRUE)/sqrt(length(X..Flowered.1))))
, "clipboard", sep="\t",
	row.names = F, col.names = T)	#total alive and dormant
	#se 

# Table 3, Total fruit and SE
write.table(ddply(PS, .(Site, Year), 
		summarise, 
		val = mean(Total.Fruit)),
	"clipboard", sep="\t",
	row.names = F, col.names = T)		#total fruit
 write.table(ddply(PS, .(Site, Year), 
			summarise, 
			SE = (sd(Total.Fruit, na.rm = TRUE)/sqrt(length(Total.Fruit)))), 
	"clipboard", sep="\t",
	row.names = F, col.names = T)	
	#se 

	t.test(PS$Total.Fruit[PS$Year > 2005] ~ PS$Fence[PS$Year > 2005])

subset(PS, PS$Site == 26 & PS$Year == 2013)



ddply(PS, .(Year, Fence), summarise, val = mean(X..Browsed))	#percent herbivory
 ddply(PS, .(Year, Fence), summarise, 
	val = (sd(X..Browsed, na.rm = TRUE)/sqrt(length(X..Browsed))))	#se 

	t.test(PS$X..Browsed[PS$Year == 1996] ~ PS$Fence[PS$Year == 1996])

	t.test(PS$X..Browsed[PS$Year > 2005] ~ PS$Fence[PS$Year > 2005])


# difference in herbivory levels in consecutive years in fenced (y) 
#	and open plots (n) (same grouping before fencing as well)
for(i in 1996:2012){
	print(i)
	print(t.test(PS$X..Browsed[PS$Year == i] ~ PS$Fence[PS$Year == i]))
			}






## Updated 10/30/13
# "Q:\Research\All_Projects_by_Species\Astragalus_microcymbus\
#	R_Analysis\R_tables\AsMi_PDSI_2013.csv"

asmipdsi <- read.csv(file.choose(""))
head(asmipdsi)
names(asmipdsi)

ap <- cbind(subset(asmipdsi, asmipdsi$Year.PDSI > 1994), 
	subset(asmipdsi, asmipdsi$Year.PDSI < 2013)) 
colnames(ap)
colnames(ap) <- c("Year.x", "Jan.x",  "Feb.x","Mar.x","Apr.x","May.x",
			"Jun.x","Jul.x","Aug.x","Sep.x","Oct.x","Nov",
			"Dec.x","Year.y", "Jan","Feb","Mar","Apr",
			"May","Jun","Jul","Aug","Sep","Oct",
			"Nov","Dec")
ap <- cbind(ap, 	PDSImean = rowMeans(ap[,c(2:8,22:26)])) 

NumperYrPS <- aggregate(PS$Total.Plants[PS$Year > 1994], 
	list(Site = PS$Site[PS$Year > 1994], 
		Year = PS$Year[PS$Year > 1994]),
	sum)	

# Format the data so each site is a column and number of individauls per year (row)
numPS <- reshape(NumperYrPS, idvar = "Year", timevar = "Site", direction = "wide")

#	load("CountPVASimpleFunction.R")

CountPVAsimple(numPS$Year, numPS$x.5) 

#	load("PredictNext.R")
getwd()

PredictNext(ap[,c(1,27)],numPS[,c(1,2)],"05", "Astragalus microcymbus")
PredictNext(ap[,c(1,27)],numPS[,c(1,3)],"15", "Astragalus microcymbus")
PredictNext(ap[,c(1,27)],numPS[,c(1,4)],"19", "Astragalus microcymbus")
PredictNext(ap[,c(1,27)],numPS[,c(1,5)],"26", "Astragalus microcymbus")

head(NumperYrPS)
head(ap)

# Fall precipiation and winter minimum temperatures

# Fall will be July-Sept, Winter Oct-Dec

setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/Dormancy_manuscript/R_code_Dormancy_AsMi/AsOs_data"))
asmidorm <- read.csv("AsMidormancy.csv", header = T)

wrccasmi <- read.csv("WRCC_AsMi.csv")

# previous years amended with .years ago
asmi.y <- data.frame(subset(asmidorm, asmidorm$Year < 2013),
		subset(wrccasmi, wrccasmi$Year > 1995 & wrccasmi$Year < 2013),
		subset(wrccasmi, wrccasmi$Year > 1994 & wrccasmi$Year < 2012),
		subset(wrccasmi, wrccasmi$Year > 1993 & wrccasmi$Year < 2011),
		subset(wrccasmi, wrccasmi$Year > 1992 & wrccasmi$Year < 2010),
		subset(wrccasmi, wrccasmi$Year > 1991 & wrccasmi$Year < 2009))

head(asmi.y)
asmi.y$Year
head(NumperYrPS)
NumperYrPS$Year

totalclim <- merge(asmi.y, NumperYrPS, by = "Year")
 library(plotrix)
 library(ggplot2)

head(totalclim)
tcl <- ddply(totalclim, .(Year), summarise,
	se = sd(x)/sqrt(length(x)),
	mx = mean(x),
	JAS_precip.1 = mean(JAS_precip.1),
	OND_temp.1 = mean(OND_temp.1))
head(tcl)


ggplot(tcl, aes(Year)) +
	geom_line(aes(y = mx)) +
	geom_errorbar(aes(ymin = mx-se, ymax=mx+se), width = 0.21) +
	geom_line(aes(y = (JAS_precip.1*10), colour = "Fall precipitation")) +
	geom_line(aes(y = (OND_temp.1), colour = "Winter temperatures")) 







