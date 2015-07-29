#Create matrix tables A

#library(PVAClone)

rm(asmi)
rm(ASMIF4)
rm(ASMIF42)
rm(asmiA)

########### temp testing
rm(seedlingsfert)
rm(fencedseedlingsfert)
rm(notfencedseedlingsfert)

rm(pro.matrix)
rm(fenced.pro.matrix)
rm(notfenced.pro.matrix)
rm(Site.matrix)
rm(fenced.Site.matrix)
rm(notfenced.Site.matrix)

rm(promatrix)
rm(fenced.promatrix)
rm(notfenced.promatrix)

library(plotrix)
library(plyr)
library(popbio)
library(ggplot2)
library(maps)

setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))
 asmi <-read.csv("2014_RawData_AsMi.csv", header = T, as.is = T, na.strings = 'na')

names(asmi)
unique(asmi$plotlocation)

asmiplots <- read.csv("AsMiPlots.csv")
asmiplots$Tag__
Veg.Repro <- ddply(PS, .(Plot), summarise,
	Repro = sum(X..Flowered.1, na.rm = T),
	Veg = sum(Total.Alive, na.rm = T)-sum(X..Flowered.1, na.rm = T))
foo.m <- merge(asmiplots, Veg.Repro, by.x = 'Tag__', by.y = 'Plot') 

map('state', region = 'colorado')
map.axes()
points(asmiplots$Lat, asmiplots$Long)
for(x in 1:nrow(foo.m)) {
	floating.pie(foo.m$Lat[x], foo.m$Long[x], c(foo.m$Repro[x], foo.m$Veg[x]),
		radius = .2, col = c('red','blue'))
	} 

ggplot(asmiplots, aes(map_id = Tag__)) +
	geom_map(aes(fill = Tag__), map = map_data("county")) +
	expand_limits(x = asmiplots$Long, y = asmiplots$Lat) +
	coord_map()

asmi[is.na(asmi$AsMi_site_id),]	#10225
asmi <- asmi[complete.cases(asmi[,1:3]),]


unique(asmi$AsMi_site_id)
unique(asmi$AsMi_plot_id)	
unique(asmi$flower); unique(asmi$fruit); unique(asmi$browsing); unique(asmi$status)
unique(asmi$AsMi_plot_id); unique(asmi$direction); 

asmi$direction[asmi$direction == 'w'] <- 'W'
asmi$direction[asmi$direction == 'E '] <- 'E'
asmi$browsing[asmi$browsing == 'mammal'] <- 'Mammal'
unique(asmi$fence)		
table(asmi$AsMi_site_id, asmi$AsMi_plot_id)

length(asmi$fence[asmi$AsMi_site_id == 1 | asmi$AsMi_site_id == 2])  # 217 in 2014
asmi <- subset(asmi, AsMi_site_id != 1 & AsMi_site_id != 2 &
		AsMi_plot_id != 598)	# Should keep Beaver and Cebolla seperate, 
						# remove plot 598


ASMIF4 <- asmi[order(asmi$AsMi_site_id, asmi$AsMi_tag_id, asmi$year),] 
str(asmi)
str(ASMIF4)

table(ASMIF4$status)		# one blank untill get rid of last row of asmi
table(ASMIF4$year, ASMIF4$AsMi_site_id)


#### Set the order of the classes
stages <- c("seedling", "vegetative", "reproductive", "dormant","dead") 
	#Orders the status by the stage levels
  ASMIF4$status <- ordered(ASMIF4$status, levels = stages)	

## check the data for errors
table(ASMIF4$status)
table(ASMIF4$year,ASMIF4$status)
table(ASMIF4$year, ASMIF4$AsMi_site_id)
table(ASMIF4$length)
ASMIF4$status[is.na(ASMIF4$length)]
	subset(ASMIF4, status == 'vegetative' & is.na(length))
ASMIF4$status[is.na(ASMIF4$length) & ASMIF4$status == 'vegetative'] 

str(ASMIF4)

# One length that was recorded crazy
ASMIF4$length[ASMIF4$length == 921] <- NA	
		
# Change insect browsing, mammal browsing, 
#	both to True or False any browsing 
ASMIF4$browsed[ASMIF4$browsing=="None"]<-FALSE		
ASMIF4$browsed[ASMIF4$browsing!="None"]<-TRUE
table(ASMIF4$browsed)
table(ASMIF4$browsing)


################################################
# AsMi_tag_id is unique per plant over all sites
# .y is the following year
# remove dead fate from stages because can't start dead!
#################### Merge year to year ########
ASMIF42 <- subset(merge(ASMIF4, ASMIF4, by = "AsMi_tag_id", sort = FALSE), year.x == year.y - 1) 	
names(ASMIF42)
head(ASMIF42)
asmimerge <- ASMIF42	# to use for correlations year to year


head(ASMIF42)
subset(ASMIF42, AsMi_site_id.x == 15 & fence.x == "y")
subset(ASMIF42, AsMi_site_id.x == 15 & fence.x == "n")

################################################
###	Narrow down to 
#	[1] "AsMi_site_id.x" "AsMi_tag_id"    "year.x"        
#	[4] "length.x"       "fruit.x"        "browsed.x"      
#	[7] "fence.x"        "status.x"       "status.y"    
################################################

     names(ASMIF42[, c(55, 1, 3, 4, 6, 58, 56, 8, 65)])
ASMIF42 <- ASMIF42[, c(55, 1, 3, 4, 6, 58, 56, 8, 65)]
colnames(ASMIF42) <- c("site", "tag", "year", "length", "fruits", 
	"browse", "fenced", "stage", "fate")
## re-number
rownames(ASMIF42) <- 1:nrow(ASMIF42)
ASMIF42$stage <- ordered(ASMIF42$stage, levels = stages[-5])
# check transitions
#fate down and stage across the top; These are the raw numbers combined for all years 
  table(Fate = ASMIF42$fate, Stage = ASMIF42$stage)  

table(Year = ASMIF42$year, Stage = ASMIF42$stage)
table(Year = ASMIF42$year, Fate = ASMIF42$fate)
table(ASMIF42$stage)
table(ASMIF42$fate)


### bootstrap distributions of population growth rates (lambda), stage vectors, 
# 	and projection matrix elements by 
# 	randomly sampling with replacement from a 
# 	stage-fate data frame of observed transitions
#
boot.transitions(subset(ASMIF42, ASMIF42$site != 1 & ASMIF42$site != 2),
	 10, fertility = "fruits")

par(mfrow =c(2,2))
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 5 & ASMIF42$fenced == 'n'), 
	1000, fertility = "fruits")$lambda)
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 15), 
	1000, fertility = "fruits")$lambda)
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 19 & ASMIF42$fenced == 'n'), 
	1000, fertility = "fruits")$lambda)
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 26 & ASMIF42$fenced == 'n'), 
	1000, fertility = "fruits")$lambda)

boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 5), 1000, fertility = "fruits")$lambda)
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 15), 1000, fertility = "fruits")$lambda)
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 19), 1000, fertility = "fruits")$lambda)
boxplot(boot.transitions(subset(ASMIF42, ASMIF42$site == 26), 1000, fertility = "fruits")$lambda)

??


##################################
### Length of dormancy periods ###
##################################

stagesnum <- as.numeric(ASMIF42$stage)
# 1: seedling
# 2: vegetative
# 3: reproductive
# 4: dormant
# 5: dead

rle(stagesnum)$lengths[rle(stagesnum)$values == 4]	# lengths of repeats when the value is 4 which is dormant
dorms <- rle(stagesnum)$lengths[rle(stagesnum)$values == 4]

par(mar = c(4,4,0,0))
hist(dorms, main = "",					# 17 is the max consecutive dormant years
	xlab = "Consecutive years dormant",
	ylab = "Frequency",
	col = "grey",
	breaks = 0:20,
	ylim = c(0,325))

ggplot(data.frame(Dormant = dorms), aes(Dormant)) +
	geom_histogram() +
	theme_bw() +
	ylab('Frequency') +
	xlab('Consecutive years dormant')


median(dorms)	# 2
length(dorms[dorms == 1])	# 2014: 310; 2013: 309	# 293 with plot 598 removed
length(dorms)			# 2014: 718; 2013: 672	# 671 with plot 598 removed
length(dorms[dorms == 18])	# 2014: 2


length(dorms[dorms == 1])/
length(dorms)			# 43.18% of the time dormant for one season	# 42.40 with plot 598 removed

max(dorms)
length(dorms[dorms == max(dorms)])

#_____________________________________________________
#_____________________________________________________
### For Lambdas by site 
#	To use the function, enter "CountPVA(x,y)
#	x The column with the years of the study 	'table$year'
#	y The column with the site names		'table$site'
#	z The column with fate				'table$fate'
#	a The column with stage				'table$stage'
#	df The whole data frame that has (1) site, (2) tag, (3) year, (4) length/measurment, 
#			must be "fruits"		(5) fruits, (6) browsed/measurment
#							(7) stage, (8) fate
# rm(StagePVA)
### To test
#	x<-ASMIF42$year
#	y<-ASMIF42$site
#	z<-ASMIF42$fate
#	a<-ASMIF42$stage
#	df <- ASMIF42


setwd(path.expand("Q:/Research/Stats & Software/R CODE/Functions"))
load("StagePVAFunction.R")


StagePVA(ASMIF42$year, ASMIF42$site, ASMIF42$fate, ASMIF42$stage, ASMIF42)


fenced.pro.matrix
notfenced.pro.matrix
fenced.Site.matrix
fenced.Site.matrix$'26'[10]	#Site 26, 2004 to 2005
fenced.Site.matrix$'15'
notfenced.Site.matrix$'15'

splitA(mean(fenced.pro.matrix))
stable.stage(mean(fenced.pro.matrix))
fundamental.matrix(mean(fenced.pro.matrix))

# the log stochastic growth rate
stoch.growth.rate(fenced.pro.matrix)
stoch.growth.rate(notfenced.pro.matrix)

# stochastic growth by projection using matrix selection 
library(plyr)
n0 <- subset(ddply(ASMIF42, .(site, year, stage), summarize,
	n0 = length(stage)), year == 1996)

# all excluding first and last years, non fenced only
par(mar = c(5,5,2,5))
par(mfrow = c(2,2))
plot(density(apply(stoch.projection(notfenced.Site.matrix$'5'[2:19], c(0,25,0,26), tmax = 5), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 5",
	xlim =c(0,100))
mtext("Final population size at t = 5", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(notfenced.Site.matrix$'15'[2:19], c(0,25,0,26), tmax = 5), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 15",
	xlim =c(0,100))
mtext("Final population size at t = 5", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(notfenced.Site.matrix$'19'[2:19], c(0,25,0,26), tmax = 5), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 19",
	xlim =c(0,100))
mtext("Final population size at t = 5", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(notfenced.Site.matrix$'26'[2:19], c(0,25,0,26), tmax = 5), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 26",
	xlim =c(0,100))
mtext("Final population size at t = 5", side = 1, line = 2, cex = 0.8)

# all excluding first and last years, non fenced only
par(mar = c(5,5,2,5))
par(mfrow = c(2,2))
plot(density(apply(stoch.projection(notfenced.Site.matrix$'5'[2:19], c(0,25,0,26), tmax = 100), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 5",
	xlim =c(0,100))
mtext("Final population size at t = 100", side = 1, line = 2, cex = 0.8)
plot(density(apply(stoch.projection(notfenced.Site.matrix$'15'[2:19], c(0,25,0,26), tmax = 100), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 15",
	xlim =c(0,100))
mtext("Final population size at t = 100", side = 1, line = 2, cex = 0.8)
plot(density(apply(stoch.projection(notfenced.Site.matrix$'19'[2:19], c(0,25,0,26), tmax = 100), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 19",
	xlim =c(0,100))
mtext("Final population size at t = 100", side = 1, line = 2, cex = 0.8)
plot(density(apply(stoch.projection(notfenced.Site.matrix$'26'[2:19], c(0,25,0,26), tmax = 100), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 26",
	xlim =c(0,100))
mtext("Final population size at t = 100", side = 1, line = 2, cex = 0.8)




## first 10 years, 1996 through the 2005 to 2006 transition
par(mar = c(5,5,2,5))
par(mfrow = c(2,2))

plot(density(apply(stoch.projection(Site.matrix$'5'[2:11], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 5",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(Site.matrix$'15'[2:11], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 15",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(Site.matrix$'19'[2:11], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 19",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(Site.matrix$'26'[2:11], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 26",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)



## last 10 years, 2002 through the 2011 to 2012 transition
par(mar = c(5,5,2,5))
par(mfrow = c(2,2))
plot(density(apply(stoch.projection(notfenced.Site.matrix$'5'[8:19], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 5",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)
plot(density(apply(stoch.projection(notfenced.Site.matrix$'15'[8:19], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 15",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)
plot(density(apply(stoch.projection(notfenced.Site.matrix$'19'[8:19], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 19",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)
plot(density(apply(stoch.projection(notfenced.Site.matrix$'26'[8:19], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 26",
	xlim =c(0,100))
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)



## post fencing years, 2006 through the 2012 to 2013 transition
par(mar = c(5,5,2,5))
par(mfrow = c(2,2))

plot(density(apply(stoch.projection(fenced.Site.matrix$'5'[12:18], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 5")
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(fenced.Site.matrix$'15'[12:18], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 15")
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(fenced.Site.matrix$'19'[12:18], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 19")
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)

plot(density(apply(stoch.projection(fenced.Site.matrix$'26'[12:18], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), 
	main = "Site 26")
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)




###################################################################
###################################################################
#hist(apply(stoch.projection(Site.matrix$'5'[2:17], c(0,25,0,26), tmax = 4), 1, sum),
#	breaks = 50,
#	col = rgb(0,0,1,0.2), xlab = "Final population size at t=4", 
#	ylab = "Frequncy (5,000 iterations)", main = "Site 5")
#par(new=T)
plot(density(apply(stoch.projection(Site.matrix$'5'[2:17], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,0,1), #xlab = "", yaxt='n', xaxt='n', 
#	ylab = "", 
	main = "Site 5")
mtext("Final population size at t = 4", side = 1, line = 2, cex = 0.8)


hist(apply(stoch.projection(Site.matrix$'15'[2:17], n0$n0[n0$site == 15], tmax = 4), 1, sum),
	breaks = 50,
	col = rgb(0,0,1,0.2), xlab = "Final population size at t=4", main = "Site 15")
par(new=T)
plot(density(apply(stoch.projection(Site.matrix$'15'[2:17], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,1,1), xlab = "", yaxt='n', xaxt='n',
	ylab = "", main = "")
axis(4, las=1)
mtext("density", side = 4, line = 2.3, cex = 0.8)

hist(apply(stoch.projection(Site.matrix$'19'[2:17], n0$n0[n0$site == 19], tmax = 4), 1, sum),
	breaks = 50,
	col = rgb(0,0,1,0.2), xlab = "Final population size at t=4", main = "Site 19")
par(new=T)
plot(density(apply(stoch.projection(Site.matrix$'19'[2:17], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,1,1), xlab = "", yaxt='n', xaxt='n',
	ylab = "", main = "")
axis(4, las=1)
mtext("density", side = 4, line = 2.3, cex = 0.8)

hist(apply(stoch.projection(Site.matrix$'26'[2:17], n0$n0[n0$site == 26], tmax = 4), 1, sum),
	breaks = 50,
	col = rgb(0,0,1,0.2), xlab = "Final population size at t=4", main = "Site 26")
par(new=T)
plot(density(apply(stoch.projection(Site.matrix$'26'[2:17], c(0,25,0,26), tmax = 4), 1, sum)),
	col = rgb(1,0,1,1), xlab = "", yaxt='n', xaxt='n',
	ylab = "", main = "")
axis(4, las=1)
mtext("density", side = 4, line = 2.3, cex = 0.8)
###################################################################
###################################################################




#___________________________________________________________
hist(apply(stoch.projection(notfenced.Site.matrix$'5'[2:18], c(0,25,0,26), tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 5 (open plots)",
	xlim= c(0,500), breaks = 10)

hist(apply(stoch.projection(notfenced.Site.matrix$'15'[2:18], n0$n0[n0$site == 15], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 15",
	xlim= c(0,500), breaks = 1000)

hist(apply(stoch.projection(notfenced.Site.matrix$'19'[2:18], n0$n0[n0$site == 19], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 19 (open plots)",
	xlim= c(0,500), breaks = 100)

hist(apply(stoch.projection(notfenced.Site.matrix$'26'[2:18], n0$n0[n0$site == 26], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 26 (open plots)",
	xlim= c(0,500), breaks = 2000)
#___________________________________________________________




# 10 years from now?
#___________________________________________________________
hist(apply(stoch.projection(notfenced.Site.matrix$'5'[2:18], c(0,25,0,26), tmax = 10), 1, sum),
	col = "skyblue", xlab = "Final population size at t=10", main = "Site 5 (open plots)",
	xlim= c(0,500), breaks = 5)

hist(apply(stoch.projection(notfenced.Site.matrix$'15'[2:18], n0$n0[n0$site == 15], tmax = 10), 1, sum),
	col = "skyblue", xlab = "Final population size at t=10", main = "Site 15",
	xlim= c(0,500), breaks = 5000)

hist(apply(stoch.projection(notfenced.Site.matrix$'19'[2:18], n0$n0[n0$site == 19], tmax = 10), 1, sum),
	col = "skyblue", xlab = "Final population size at t=10", main = "Site 19 (open plots)",
	xlim= c(0,500), breaks = 50)

hist(apply(stoch.projection(notfenced.Site.matrix$'26'[2:18], n0$n0[n0$site == 26], tmax = 10), 1, sum),
	col = "skyblue", xlab = "Final population size at t=10", main = "Site 26 (open plots)",
	xlim= c(0,500), breaks = 5000)
#___________________________________________________________



par(mfrow = c(2,2))
# 1996 through 2005_____________________________________________________
hist(apply(stoch.projection(Site.matrix$'5'[2:10], c(0,25,0,26), tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 5 (all plots)",
	xlim = c(0,60))

hist(apply(stoch.projection(Site.matrix$'15'[2:10], n0$n0[n0$site == 15], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 15",
	xlim = c(0,400))

hist(apply(stoch.projection(Site.matrix$'19'[2:10], n0$n0[n0$site == 19], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 19 (all plots)",
	xlim = c(0,950))
par(new=T, mar=c(5,5,5,5))
plot(density(apply(stoch.projection(Site.matrix$'19'[2:10], n0$n0[n0$site == 19], tmax = 4), 1, sum)),
	yaxt='n', xaxt='n', xlab = '', ylab='', main='')
axis(4, las=1)


hist(apply(stoch.projection(Site.matrix$'26'[2:10], n0$n0[n0$site == 26], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 26 (all plots)",
	xlim = c(0,700))
par(new=T, mar=c(5,5,5,5))
plot(density(apply(stoch.projection(Site.matrix$'19'[2:10], n0$n0[n0$site == 26], tmax = 4), 1, sum)),
	yaxt='n', xaxt='n', xlab = '', ylab='', main='')
axis(4, las=1)
#___________________________________________________________



par(mfrow = c(2,2),mar=c(5,5,5,5))
# 1996 through 2012 exclude first and last years_____________________________________________________
hist(apply(stoch.projection(notfenced.Site.matrix$'5'[2:18], c(0,25,0,26), tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 5 (open plots)",
	xlim = c(0,70))

hist(apply(stoch.projection(notfenced.Site.matrix$'15'[2:18], n0$n0[n0$site == 15], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 15 (all plots)",
	xlim = c(0,1250), 
	breaks=150)

hist(apply(stoch.projection(notfenced.Site.matrix$'19'[2:18], n0$n0[n0$site == 19], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 19 (open plots)",
	xlim = c(0,600),
	breaks=50)

hist(apply(stoch.projection(notfenced.Site.matrix$'26'[2:18], n0$n0[n0$site == 26], tmax = 4), 1, sum),
	col = "skyblue", xlab = "Final population size at t=4", main = "Site 26 (open plots)",
	xlim = c(0,1500),
	breaks=500)
#___________________________________________________________





# exclude 1995??

# Plot dominate eigenvalue per year from projection matrix (the population growth rate) 
str(matrix(c(fenced.pro.matrix[1], nrow = 4)))

# fenced
lbds <- c()
	for(i in 2:19){
		lbds <- rbind(lbds, lambda(mean(fenced.pro.matrix[i])))
	}

par(mfrow = c(1,3))
par(mar = c(4,4,.1,.1))
matplot(lbds, type = "p", pch = 20,
	xlab = "Annual transition",
	ylab = "growth rate")
boxplot(lbds)

styr <- data.frame(lbds, Years = 1996:2013)
plot(styr$Years, styr$lbds,
	xlab = "Year transistion starting",
	ylab = "growth rate")
abline(lm(styr$lbds~styr$Years))
summary(lm(styr$lbds~styr$Years))

## Not fenced
lbds <- c()
	for(i in 2:19){
		lbds <- rbind(lbds, lambda(mean(notfenced.pro.matrix[i])))
	}

par(mfrow = c(1,3))
par(mar = c(4,4,.1,.1))
matplot(lbds, type = "p", pch = 20,
	xlab = "Annual transition",
	ylab = "growth rate")
boxplot(lbds)

styr <- data.frame(lbds, Years = 1996:2013)
plot(styr$Years, styr$lbds,
	xlab = "Year transistion starting",
	ylab = "growth rate")
abline(lm(styr$lbds~styr$Years))
summary(lm(styr$lbds~styr$Years))


plot(styr$Years[styr$Years < 2011], styr$lbds[styr$Years < 2011],
	xlab = "Year transistion starting",
	ylab = "growth rate")
abline(lm(styr$lbds[styr$Years < 2011] 
	~ styr$Years[styr$Years < 2011]))
summary(lm(styr$lbds[styr$Years < 2011] 
	~ styr$Years[styr$Years < 2011]))

par(mar = c(4,4,.1,.1))
plot(styr$Years[styr$Years < 2006], styr$lbds[styr$Years < 2006],
	xlab = "Year transistion starting",
	ylab = "growth rate")
abline(lm(styr$lbds[styr$Years < 2006] 
	~ styr$Years[styr$Years < 2006]))
summary(lm(styr$lbds[styr$Years < 2006] 
	~ styr$Years[styr$Years < 2006]))



#_______________________________________________________________________________________
#Figure 2: Life cycle diagram of Astragalus microcymbus, 1996-2011 
#	mean transition rates (solid lines) and fecundity rates 
# 	(dashed lines), and resulting sensitivities and elasticities. 
#	Individuals that become reproductive in the first year 
#	are classified as seedlings.

notfenced.pro.matrix[18] #2012:2013
mean(notfenced.pro.matrix[2:18])

pro.matrix[17]	# 2011
pro.matrix[10]	# 2004

### Function for lambda and 
#	requires library(popbio)
#x is the list of matrices, i.e. notfenced.Site.matrix$"26", 
#					or fenced.Site.matrix$"19"[2:17])
#y is the start year
#z is the start of the last transition

lamdaSD <- function(x,y,z){

	yrs <- 1995:z
	st <- match(c(y,z),yrs)
	print(lambda(mean(x[st[1]:st[2]])))

	
	lambdaAll <- c()
	for(i in c(y:z)){
		lambdaAll <- cbind(lambdaAll, 
			eigen.analysis(x[[as.character(i)]])$lambda)
	}	
	lambda <<- lambdaAll
	StandDev <- sqrt(var(c(lambdaAll)))	#Standard Deviation
	print(StandDev)
}	

# excluding the first and last years
lamdaSD(pro.matrix, 1996, 2013)
[1] 0.9849525
[1] 0.343842

lamdaSD(pro.matrix, 1996, 2012)

# pre fencing (the 2005 to 2006 transition)
lamdaSD(pro.matrix, 1996, 2004)


#[1] 0.9935542 2013
#[1] 0.3366391

# pre fencing (the 2005 to 2006 transition)
lamdaSD(notfenced.pro.matrix, 1996, 2004)
[1] 0.9932473
[1] 0.3214646

lamdaSD(fenced.pro.matrix, 1996, 2004)
[1] 0.9949851
[1] 0.3215974

lambda(mean(fenced.pro.matrix[2:10]))	# 1996:1997 to 2004:2005
[1] 1.060817
lambda(mean(fenced.pro.matrix[11:18]))	# 2006:2007 to 2012:2013
[1] 1.053245

lambda(mean(notfenced.pro.matrix[2:10]))	
[1] 0.9932473
lambda(mean(notfenced.pro.matrix[11:18]))	
[1] 1.029018

	

___________________________________________
fenced.pro.matrix[16]	# 2010
fenced.pro.matrix[17]	# 2011	

lamdaSD(fenced.pro.matrix, 2006, 2012)
[1] 0.8220016
[1] 0.261503

fen <- lambda

lamdaSD(notfenced.pro.matrix, 2005, 2012)
lamdaSD(notfenced.pro.matrix, 2005, 2013)
[1] 1.006966
[1] 0.5367234

not <- lambda

t.test(fen, not, paired = TRUE)
	
# not significantly different lambdas in the fenced vs. not fenced

    Paired t-test

data:  fen and not
t = -0.6773, df = 8, p-value = 0.5173
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.4979307  0.2718423
sample estimates:
mean of the differences 
             -0.1130442 
 



# All sites
# excluding the first and last transitions for dormancy reasons
# any errors? try library(popbio)
__________________________________________________________________

lamdaSD(notfenced.pro.matrix, 1996, 2004)
prenotall <- lambda
[1] 0.9932473
[1] 0.3214646

lamdaSD(fenced.pro.matrix, 1996, 2004)	# 1996:1997 to 2004:2005
prefenall <- lambda
[1] 1.060817
[1] 0.4990608

t.test(prenotall, prefenall, paired = T)

         Paired t-test

data:  prenotall and prefenall
t = -0.9109, df = 8, p-value = 0.389
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.4390296  0.1904070
sample estimates:
mean of the differences 
             -0.1243113 

#####################################################
####### ###### Table 3 ####### ######  ####### ###### 
# 1996-2005
lamdaSD(pro.matrix, 1996, 2004)
lamdaSD(Site.matrix$"5", 1996, 2004)
lamdaSD(notfenced.Site.matrix$"5", 1996, 2004)

# 2005-2006 (fencing) and 2013-2013 to exclude last year (and first year)
lamdaSD(notfenced.pro.matrix, 1996, 2012)
lamdaSD(notfenced.pro.matrix, 2005, 2012)
lamdaSD(fenced.pro.matrix, 2005, 2012)

lamdaSD(notfenced.Site.matrix$"5", 1996, 2012)
lamdaSD(notfenced.Site.matrix$"5", 2005, 2012)
lamdaSD(fenced.Site.matrix$"5", 2005, 2012)

lamdaSD(Site.matrix$"15", 2005,2012)
lamdaSD(notfenced.Site.matrix$"15", 1996, 2012)

lamdaSD(notfenced.Site.matrix$"19", 1996, 2012)
lamdaSD(notfenced.Site.matrix$"19", 2005, 2012)
lamdaSD(fenced.Site.matrix$"19", 2005, 2012)

lamdaSD(notfenced.Site.matrix$"26", 1996, 2012)
lamdaSD(fenced.Site.matrix$"26", 2005, 2012)
lamdaSD(notfenced.Site.matrix$"26", 2005, 2012)

####### ###### Table 3 ####### ######  ####### ###### 
#####################################################



lamdaSD(notfenced.pro.matrix, 1996, 2013)
allnotall <- lambda



lamdaSD(fenced.pro.matrix, 1996, 2013)
allfenall <- lambda

t.test(allnotall, allfenall, paired = T)
         Paired t-test

data:  allnotall and allfenall
t = -0.2729, df = 17, p-value = 0.7882
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2509472  0.1934593
sample estimates:
mean of the differences 
            -0.02874394 



lamdaSD(notfenced.pro.matrix, 2005, 2013)
postnotall <- lambda
[1] 1.006496
[1] 0.5363372

lamdaSD(fenced.pro.matrix, 2005, 2013)
postfenall <- lambda
[1] 0.9962743
[1] 0.4506594

t.test(postnotall, postfenall, paired = T)

        Paired t-test

data:  postnotall and postfenall
t = 0.409, df = 8, p-value = 0.6933
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.3078910  0.4406578
sample estimates:
mean of the differences 
             0.06638344 

lamdaSD(notfenced.pro.matrix, 1996, 2012)
[1] 1.011629
[1] 0.414181


#Site 5
____________________

lamdaSD(Site.matrix$"5", 1996, 2004)
[1] 0.8814467
[1] 0.130617

lamdaSD(notfenced.Site.matrix$"5", 1996, 2013)
[1] 0.8333093
[1] 0.1559586

#1996:2012
[1] 0.8546023
[1] 0.1334427

lamdaSD(notfenced.Site.matrix$"5", 2005, 2013)
lamdaSD(fenced.Site.matrix$"5", 2005, 2013)

#Post fencing from 2005->2006 to 2012->2013
lamdaSD(notfenced.Site.matrix$"5", 2005, 2012)
not <- lambda
[1] 0.8129566
[1] 0.1444096

# Post fencing in fenced 
lamdaSD(fenced.Site.matrix$"5", 2005, 2012)
fen <- lambda
[1] 0.7103778
[1] 0.4537105

t.test(fen, not, paired = TRUE)
# no signficant difference in lambdas
                  Paired t-test

data:  fen and not
t = -0.9487, df = 7, p-value = 0.3744
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.5065115  0.2164532
sample estimates:
mean of the differences 
             -0.1450292 



#Was pre fencing different than post fencing?	- nope
lamdaSD(notfenced.Site.matrix$"5", 1996, 2004)
prenot <- lambda
[1] 0.8883698
[1] 0.1278313

lamdaSD(fenced.Site.matrix$"5", 1996, 2004)
prefen <- lambda
[1] 0.8439185
[1] 0.1945619

t.test(prenot, not)

        Welch Two Sample t-test

data:  prenot and not
t = 0.3801, df = 14.024, p-value = 0.7096
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.1186155  0.1697163
sample estimates:
mean of x mean of y 
0.8997462 0.8741958  

t.test(prefen, fen)

        Welch Two Sample t-test

data:  prefen and fen
t = 0.7038, df = 9.259, p-value = 0.4989
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2679778  0.5115172
sample estimates:
mean of x mean of y 
0.8509364 0.7291667 

__________________________________________________________
#Site 15

lamdaSD(notfenced.Site.matrix$"15", 1996, 2013)
[1] 1.045869
[1] 0.7452851

1996:2012
[1] 1.06535
[1] 0.7109874

#Post fencing

lamdaSD(Site.matrix$"15", 2005,2013)
post <- lambda
[1] 1.415353
[1] 1.014707

lamdaSD(Site.matrix$"15", 1996,2004)
pre <- lambda
[1] 0.9463314
[1] 0.2058395

#Was pre fencing different than post fencing?	-> nope, p = 0.066
t.test(post,pre)

Welch Two Sample t-test

data:  post and pre
t = 0.9121, df = 6.32, p-value = 0.3952
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.642033  1.420157
sample estimates:
mean of x mean of y 
 1.418382  1.029320 


_________________________________________________________
#Site 19

lamdaSD(notfenced.Site.matrix$"19", 1996, 2013)
[1] 0.959365
[1] 0.3113373

1996:2012
[1] 0.9484148
[1] 0.3208122

lamdaSD(Site.matrix$"19", 1996, 2004)
[1] 1.034356
[1] 0.63693

#Postfence
lamdaSD(notfenced.Site.matrix$"19", 2005, 2013)
notpost <- lambda
[1] 0.8578118
[1] 0.2166271

lamdaSD(fenced.Site.matrix$"19", 2005, 2013)
fenpost <- lambda
[1] 0.8805674
[1] 0.3634285

t.test(notpost, fenpost, paired = T)

        Paired t-test

data:  notpost and fenpost
t = 0.163, df = 7, p-value = 0.8751
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2833846  0.3253437
sample estimates:
mean of the differences 
             0.02097954  

#Pre fencing
lamdaSD(notfenced.Site.matrix$"19", 1996, 2004)
notpre <- lambda
[1] 1.014819
[1] 0.3703245

lamdaSD(fenced.Site.matrix$"19", 1996, 2004)
fenpre <- lambda
[1] 0.9618244
[1] 0.4099074

#Was pre fencing different than post fencing?	
t.test(fenpre, fenpost)

        Welch Two Sample t-test

data:  fenpre and fenpost
t = 0.6827, df = 14.804, p-value = 0.5053
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2874124  0.5578439
sample estimates:
mean of x mean of y 
1.0394450 0.9042293 


_________________________________________________________
#Site 26

lamdaSD(notfenced.Site.matrix$"26", 1996, 2013)
[1] 1.077053
[1] 2.616558

1996:2012
[1] 1.13219
[1] 2.673115

#pre
lamdaSD(Site.matrix$"26", 1996, 2004)
[1] 0.9730855
[1] 0.256507

lamdaSD(fenced.Site.matrix$"26", 2005, 2013)
lamdaSD(fenced.Site.matrix$"26", 2005, 2012)
fenpost <-  lambda
[1] 0.8525058
[1] 0.3518113

lamdaSD(notfenced.Site.matrix$"26", 2005, 2013)
lamdaSD(notfenced.Site.matrix$"26", 2005, 2012)
notpost <-  lambda
[1] 1.768279
[1] 3.875494

notfenced.Site.matrix$"26"[11:18]	# seedling 12 in 2007? 


 t.test(fenpost, notpost, paired = TRUE)

        Paired t-test

data:  fenpost and notpost
t = -1.0994, df = 7, p-value = 0.308
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -4.533135  1.655704
sample estimates:
mean of the differences 
              -1.438716  

#Was pre fencing different than post fencing?	
lamdaSD(notfenced.Site.matrix$"26", 1996, 2004)
notpre <- lambda
[1] 0.9242807
[1] 0.08439841

lamdaSD(fenced.Site.matrix$"26", 1996, 2004)
fenpre <- lambda
[1] 0.9406036
[1] 0.1440843

t.test(notpre, notpost)	
#no

        Welch Two Sample t-test

data:  notpre and notpost
t = -1.0716, df = 7.006, p-value = 0.3194
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -4.708796  1.771451
sample estimates:
mean of x mean of y 
0.9674503 2.4361230 


t.test(fenpre, fenpost)

        Welch Two Sample t-test

data:  fenpre and fenpost
t = 0.2782, df = 9.067, p-value = 0.7871
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2641876  0.3383851
sample estimates:
mean of x mean of y 
1.0345062 0.9974075 

__________________________________________________________________________________________________

################################################
# exclude Plot 598, from Site 26 that was added in 2004

table(ASMIF4$AsMi_plot_id, ASMIF4$AsMi_site_id)
table(ASMIF4$AsMi_plot_id, ASMIF4$year)

small <- subset(ASMIF4, ASMIF4$AsMi_plot_id != 598 & ASMIF4$AsMi_site_id != 1 & ASMIF4$AsMi_site_id != 2 )
small <- subset(merge(small, small, by = "AsMi_tag_id", sort = FALSE), year.x == year.y - 1) 	


names(small[, c(55, 1, 3, 4, 6, 58, 56, 8, 65)])
small<- small[, c(55, 1, 3, 4, 6, 58, 56, 8, 65)]
colnames(small) <- c("site", "tag", "year", "length", "fruits", 
	"browse", "fenced", "stage", "fate")
## re-number
rownames(small) <- 1:nrow(small)
# remove dead fate from stages because can't start dead!
small$stage <- ordered(small$stage, levels = stages[-5])

rm(Site.matrix)
rm(fenced.Site.matrix)
rm(notfenced.Site.matrix)
StagePVA(small$year, small$site, small$fate, small$stage, small)

#	requires library(popbio)
#x is the list of matrices, i.e. notfenced.Site.matrix$"26", 
#					or fenced.Site.matrix$"19"[2:17])
#y is the start year
#z is the start of the last transition

rm(x)
rm(y)
rm(z)
rm(lambdaAll)

lamdaSD <- function(x,y,z){

	yrs <- 1995:z
	st <- match(c(y,z),yrs)
	print(lambda(mean(x[st[1]:st[2]])))

	
	lambdaAll <- c()
	for(i in c(y:z)){
		lambdaAll <- cbind(lambdaAll, 
			eigen.analysis(x[[as.character(i)]])$lambda)
	}	
	lambda <<- lambdaAll
	StandDev <- sqrt(var(c(lambdaAll)))	#Standard Deviation
	print(StandDev)
}	

#### current year - use current -2
cyr <- 2014


# All
lamdaSD(pro.matrix, 1996, cyr-1)	# error if cyr = 2014
[1] 1.015799
[1] 0.3232709

# Pre-fencing all
lamdaSD(pro.matrix, 1996, 2004)
[1] 1.002954
[1] 0.3342551

# subset of fenced plots before fencing
lamdaSD(fenced.pro.matrix, 1996, 2004)
[1] 1.070346
[1] 0.5217279

# Fenced vs. Not after 2005 
# Post fencing
lamdaSD(fenced.pro.matrix, 2005, cyr-1)
[1] 1.034719
[1] 0.4300093

fenced <- lambda

lamdaSD(notfenced.pro.matrix, 2005, cyr-1)
[1] 1.042241
[1] 0.4493862

not <- lambda

t.test(fenced, not, paired = T)

         Paired t-test

data:  fenced and not
t = -0.4524, df = 8, p-value = 0.663
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.3973984  0.2670425
sample estimates:
mean of the differences 
            -0.06517795 
 

# Open long-term growth rate of all open sites
lamdaSD(notfenced.pro.matrix, 1996, cyr-1)
[1] 1.027181
[1] 0.3897888	vs. [1] 0.4287339 with plot 598 in 2013


# Site 05
lamdaSD(notfenced.Site.matrix$"5", 1996, cyr-2)
[1] 0.8516448
[1] 0.1401096

# Site 26 with its 598 that was added in 2004 excluded

lamdaSD(notfenced.Site.matrix$"26", 1996, 2004)
[1] 0.934384
[1] 0.08630211

lamdaSD(fenced.Site.matrix$"26", 1996, 2004)
[1] 0.9402134
[1] 0.1448742

lamdaSD(notfenced.Site.matrix$"26", 2005, cyr-2)
exnotpost <- lambda
[1] 1.019998
[1] 0.302997

lamdaSD(fenced.Site.matrix$"26", 2005, cyr-2)
exfenpost <- lambda
[1] 0.8393618
[1] 0.3444974

t.test(exnotpost, exfenpost)

        Welch Two Sample t-test

data:  exnotpost and exfenpost
t = 0.4944, df = 13.775, p-value = 0.6288
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.2682396  0.4286192
sample estimates:
mean of x mean of y 
 1.071836  0.991646 

#	end exclude Plot 598
################################################



____________________________________________________________
____________________________________________________________
### for sensitivites go to b_Vital Rate Sensitivities
# also at the bottom of this code...
asmiA <- ASMIF42
____________________________________________________________

@#$%@#%@#$%@#%$@#$%@#$%@#%@#$%@#%@#$%@#%$@#$%@#$%@#%@#$%@#%@#$%@#%$@#$%@#$%@#%
@#$%@#%@#$%@#%$@#$%@#$%@#%@#$%@#%@#$%@#%$@#$%@#$%@#%@#$%@#%@#$%@#%$@#$%@#$%@#%
@#$%@#%@#$%@#%$@#$%@#$%@#%         START HERE       @#$%@#%@#$%@#%$@#$%@#$%@#%
@#$%@#%@#$%@#%$@#$%@#$%@#%@#$%@#%@#$%@#%$@#$%@#$%@#%@#$%@#%@#$%@#%$@#$%@#$%@#%

___________________________________________________________________________________________________
___________________________________________________________________________________________________
____________________________________________________________
#multiple linear regressions from PCA

#Q:\Research\All_Projects_by_Species\Astragalus_microcymbus\R_Analysis\R_tables\PlotSummary_2012
### Add in heating degree days



setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))
ASMI<-read.csv("2014_PlotSummary_asmi.csv", header = T, as.is = T)
head(ASMI)
names(ASMI)  # aboitic factors are [,c(14:64)]
			# precipitation including snow: [,c(12,25:36)]
		# Heating Dregree Days are 51:64, with 56 = Annual and 64 is sum up throgh July 
		#	in the current year ('Year')




#combine variables into 4 month chuncks like Van Buren and Harper but for our case makes most 
# sense to do
#			Winter : 	December - March
#		Spring/Summer : 	April - July (time of census)
#		Preceding Fall :	August - November

grep("Dec",names(ASMI))
grep("Jan",names(ASMI))
grep("Feb",names(ASMI))
grep("Mar",names(ASMI))
grep("Apr",names(ASMI))
grep("May",names(ASMI))
grep("Jun",names(ASMI))
grep("Jul",names(ASMI))
grep("Aug",names(ASMI))
grep("Sept",names(ASMI))
grep("Oct",names(ASMI))
grep("Nov",names(ASMI))
grep("Rain", names(ASMI))
grep("MinTemp", names(ASMI))
grep("MaxTemp", names(ASMI))

ASMI <- cbind(ASMI,WinterRain = rowMeans(ASMI[,30:33], na.rm = TRUE), 	
		SummerRain = rowMeans(ASMI[,34:37], na.rm = TRUE), 
		FallRain = rowMeans(ASMI[,26:29], na.rm = TRUE),
		WinterMinTemp = rowMeans(ASMI[,42:45], na.rm = TRUE), 
		SummerMinTemp = rowMeans(ASMI[,46:49], na.rm = TRUE), 
		FallMinTemp = rowMeans(ASMI[,38:41], na.rm = TRUE),
		WinterMaxTemp = rowMeans(ASMI[,c(25,14:16)], na.rm = TRUE), 
		SummerMaxTemp = rowMeans(ASMI[,17:20], na.rm = TRUE), 
		FallMaxTemp = rowMeans(ASMI[,21:24], na.rm = TRUE))

head(ASMI)
names(ASMI)
names(ASMI[c(50:58)])

pairs(unique(ASMI[,c(50:58)]), main="", 
		pch=21, bg = c(1:18)[unclass(as.factor(unique(ASMI$Year)))])	
#WinterMinTemp to WinterMaxTemp correlated 

library(psych)
corr.test(ASMI[,c(50:58)])
# WinterTain:SummerMinTemp	cor: -0.59, p < 0.001
# WinterRain:SummerMaxTemp	cor: -0.4, p < 0.001
# SummerRain:WinterMinTemp	cor: 0.44, p < 0.001
# 



pairs(unique(ASMI[,c(50:55,57:58)]), main="", 
		pch=21, bg = c(1:18)[unclass(as.factor(unique(ASMI$Year)))])	
#removed WinterMaxTemp

cov(ASMI[,c(50:55,57:58)])
cor(ASMI[,c(50:55,57:58)])	# 
cor(ASMI[,c(50:58)])		# Over 50%: WinRain:SumMin, SumRain:SumMax, FallRain:FallMax
					# 		FallRain:FallMin, WintMin:WintMax, SumMin:SumMax
#	Keep: WinterRain, SummerRain, FallRain, WinterMin
#	discard:	SummerMin, SummerMax, FallMax, FallMin, WinterMax 

names(ASMI[,50:53])
pairs(unique(ASMI[,50:53]), main="", 
		pch=21, bg = c(1:18)[unclass(as.factor(unique(ASMI$Year)))])

__________________
#excluding nothing

nrow(unique(ASMI[,50:58]))
ASMI.pca <- princomp(unique(ASMI[,50:58]))
summary(ASMI.pca)
ASMI.pc <- predict(ASMI.pca)
ASMI.load <- loadings(ASMI.pca)
nrow(ASMI.pc)

---------------------------------------------------

# exclude WinterMinTemp; SummerHDD
ASMI.pca <- princomp(unique(ASMI[,50:53]), cor=T)

summary(ASMI.pca)
ASMI.load <- loadings(ASMI.pca)
plot(ASMI.pca)
ASMI.pc <- predict(ASMI.pca)
nrow(ASMI.pc)

write.table(ASMI.load, "clipboard", sep="\t", row.names = T, col.names = T)	

________________________________________________________
#Figure 4
# rgb(0,0,1, 0.5) <- is transparent


library(lattice)
library(latticeExtra)
library(ggplot2)
library(devtools)
library(digest)
#source_url("http://cran.r-project.org/bin/windows/Rtools/") #and then run find_rtools()
source_url("https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R")

attributes(ASMI.pc)
asmi.pc <- data.frame(ASMI.pc)
asmi.pc$Comp.1
head(ASMI)

asmi.pc$Year <- unique(ASMI$Year)
asmi.pc$Mast <- c(1,0,1,0,0,	# 1995:1999	When average fruit production is greater than the average 184.9 fr per plot
			0,0,0,0,0,	# 2000:2004
			0,0,0,1,1,	# 2005:2009
			0,0,0,0,0)	# 2010:2014
asmi.pc$Repro <- c(1,0,1,0,1,	# 1995:1999	1:5	Greater than average 17%
			0,0,0,1,0,	# 2000:2004	6:10
			0,0,0,1,1,	# 2005:2009	11:15
			1,1,0,0,0)	# 2010:2014	16:20
asmi.pc$Br <- c(0,1,0,1,1,	# 1995:1999	1:5	Greater than average 25%
			1,1,0,0,1,	# 2000:2004	6:10
			0,0,1,0,1,	# 2005:2009	11:15
			0,0,0,1,1)	# 2010:2014	16:20
asmi.pc$Dorm <- c(NA,0,0,1,1,	# 1995:1999	1:5	Greater than average 28%
			1,1,1,1,1,	# 2000:2004	6:10
			1,1,1,0,0,	# 2005:2009	11:15
			0,0,1,1,NA)	# 2010:2014	16:20

ggplot(asmi.pc, aes(Comp.1, Comp.2, colour = as.factor(Mast), label = Year)) +
	geom_text(alpha = 0.75, angle = 45, size = 3) +
	stat_ellipse() +
	xlab('PC1 (50%)') + #	xlab('PC1 (46%)') +
	ylab('PC2 (29%)') + #	ylab('PC2 (28%)') +
	theme_bw()


ggplot(asmi.pc, aes(Comp.1, Comp.2, colour = as.factor(Repro), label = Year)) +
	geom_text(alpha = 0.75, angle = 45, size = 3) +
	stat_ellipse() +	
	xlab('PC1 (50%)') + #	xlab('PC1 (46%)') +
	ylab('PC2 (29%)') + #	ylab('PC2 (28%)') +
	theme_bw()
	
ggplot(asmi.pc, aes(Comp.1, Comp.2, colour = as.factor(Br), label = Year)) +
	geom_text(alpha = 0.75, angle = 45, size = 3) +
	stat_ellipse() +	
	xlab('PC1 (50%)') + #	xlab('PC1 (46%)') +
	ylab('PC2 (29%)') + #	ylab('PC2 (28%)') +
	theme_bw()

ggplot(asmi.pc, aes(Comp.1, Comp.2, colour = as.factor(Dorm), label = Year)) +
	geom_text(alpha = 0.75, angle = 45, size = 3) +
	stat_ellipse() +	
	xlab('PC1 (50%)') + #	xlab('PC1 (46%)') +
	ylab('PC2 (29%)') + #	ylab('PC2 (28%)') +
	theme_bw()
	

library(MASS)

par(mfrow=c(1,1))
eqscplot(ASMI.pc[,1:2], tol = 0.1, type="n", xlab="PC1 (46%)", ylab="PC2 (28%)")

col.ASMI<-c('black',	"white",	"black",	"white",	"white",	#mast seeding years - circles 
		"white",	"white",	"white",	"white",	"white",	
		"white",	"white",	"white",	"black",	"black",	
		"white",	"white",	"white", 	"white")
points(ASMI.pc[,1],ASMI.pc[,2], pch = 21, , cex = 4, col=c(col.ASMI)[unclass(as.factor(unique(unique(ASMI$Year))))])
col.ASMI<-c('white',	"white",	"white",	"white",	"white",	#over 50% reproductive - triangles
		"white",	"white",	"white",	"white",	"white",	
		"white",	"white",	"white",	"black",	"black",	
		"black",	"white",	"white",	"white")
points(ASMI.pc[,1],ASMI.pc[,2], pch = 2, , cex = 5.3, col=c(col.ASMI)[unclass(as.factor(unique(unique(ASMI$Year))))])
col.ASMI<-c("white",	"white",	"white",	"grey",	"grey",	#dormant - grey boxes
		"grey",	"grey",	"grey",	"grey",	"grey",	
		"grey",	"white",	"grey",	"white",	"white",	
		"white",	"white",	"white",	"white")
points(ASMI.pc[,1],ASMI.pc[,2], pch = 15, , cex = 2.5, col=c(col.ASMI)[unclass(as.factor(unique(unique(ASMI$Year))))])
text(ASMI.pc[,1:2], labels=as.character(unique(unique(ASMI$Year))), cex=0.65, #srt = 45,
	col="black"[unclass(as.factor(unique(ASMI$Year)))])
-----------------------------------------------------------------------------

library(MASS)

#Second PC (mast years are negative on second PC)
#Mast Seeding years are red, the rest are blue, a little bit of fruit is grey
col.ASMI<-c('red',	'blue',	'red',	'blue',	'grey',
		'blue',	'blue',	'blue',	'blue',	'blue',
		'blue',	'grey',	'blue',	'red',	'red',	
		'grey',	'grey',	'blue',	'blue')

par(mfrow=c(1,3))
eqscplot(ASMI.pc[,1:2], type="n", xlab="First PC", ylab="Second PC")
par(cex=.9)
text(ASMI.pc[,1:2], labels=as.character(unique(unique(ASMI$Year))), cex=0.66,
	col=c(col.ASMI)[unclass(as.factor(unique(unique(ASMI$Year))))])

eqscplot(ASMI.pc[,c(1,3)], type="n", xlab="First PC", ylab="Third PC")
text(ASMI.pc[,c(1,3)], labels=as.character(unique(ASMI$Year)), cex=0.66,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

eqscplot(ASMI.pc[,2:3], type="n", xlab="Second PC", ylab="Third PC")
text(ASMI.pc[,2:3], labels=as.character(unique(ASMI$Year)), cex=0.66,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])


par(mfrow=c(1,1))
eqscplot(ASMI.pc[,1:2], type="n", xlab="PC1 (37%)", ylab="PC2 (36%)")
par(cex=.9)
text(ASMI.pc[,1:2], labels=as.character(unique(unique(ASMI$Year))), cex=0.66,
	col=c(col.ASMI)[unclass(as.factor(unique(unique(ASMI$Year))))])

--------------------------------------------------------

#First and second PC (above 50% reproductive when first and second are negative
# Percent Reproductive levels by year, average above ~50% is high/red, 
	#	between 50% and 20% mid/green, 
	#	below = low/blue
col.ASMI<-c('green',	'blue',	'green',	'blue',	'green',	#1995:1999
		'blue',	'blue',	'blue2',	'blue',	'blue',	#2000:2004
		'blue',	'blue',	'blue',	'red',	'red',	#2005:2009
		'red',	'green',	'blue',	'blue')			#2010:2013

par(mfrow=c(1,3))
eqscplot(ASMI.pc[,1:2], type="n", xlab="First PC", ylab="Second PC")
#par(cex=1)
text(ASMI.pc[,1:2], labels=as.character(unique(ASMI$Year)), cex=0.9,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

eqscplot(ASMI.pc[,c(1,3)], type="n", xlab="First PC", ylab="Third PC")	 
#par(cex=1)
text(ASMI.pc[,c(1,3)], labels=as.character(unique(ASMI$Year)), cex=0.9,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

eqscplot(ASMI.pc[,2:3], type="n", xlab="Second PC", ylab="Third PC")
#par(cex=1)
text(ASMI.pc[,2:3], labels=as.character(unique(ASMI$Year)), cex=0.9,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

par(mfrow=c(1,1))
eqscplot(ASMI.pc[,1:2], type="n", xlab="PC1 (37%)", ylab="PC2 (36%)")
#par(cex=1)
text(ASMI.pc[,1:2], labels=as.character(unique(ASMI$Year)), cex=0.9,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

------------------------------------------------

# Not explained well by any PC vector
#Number dormant each year, purple >60, red>/=30, grey <30 & >=10, blue <10
#1996 1997	1998	1999	2000	2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	2011	2012
#0.35	0.21	0.44	0.42	0.50	0.56	0.80	0.77	0.55	0.67	0.43	0.53	0.30	0.21	0.17	0.04	0.58
# r	g	r	r	r	r	p	p	r	p	r	r	r	g	g	b	r	

col.ASMI<-c('white',	'red',	'grey',	'red',	'red',	#1995:1999
		'red',	'red',	'purple',	'purple',	'red',	#2000:2004
		'purple',	'red',	'red',	'red',	'grey',	#2005:2009
		'grey',	'blue',	'red',	'white')


par(mfrow=c(1,3))
par(cex=.81)
eqscplot(ASMI.pc[,1:2], type="n", xlab="PC1 (37%)", ylab="PC2 (36%)")
text(ASMI.pc[,1:2], labels=as.character(unique(ASMI$Year)), cex=0.79, srt = 90,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

eqscplot(ASMI.pc[,c(1,3)], type="n", xlab="PC1 (37%)", ylab="PC2 (36%)")
text(ASMI.pc[,c(1,3)], labels=as.character(unique(ASMI$Year)), cex=0.79, srt = 90,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

eqscplot(ASMI.pc[,2:3], type="n", xlab="PC1 (37%)", ylab="PC2 (36%)")
text(ASMI.pc[,2:3], labels=as.character(unique(ASMI$Year)), cex=0.79, srt = 90,
	col=c(col.ASMI)[unclass(as.factor(unique(ASMI$Year)))])

________________________________________________________________________________________________

rain2014 <- read.csv("prism.oregonstate.edu_2014.csv")
mean(ddply(rain2014, .(Year), summarise,
	Rain = sum(Value))[,2])		# 291.14
____________________________________________________________
#multiple linear regression of growth factors on weather

#PC 1 has as the most influencial:
comp 1 important	Comp.1
FallRain		-0.737290452
SummerRain		-0.641989229
WinterRain		-0.143197373
WinterMinTemp	-0.105052136
SummerMaxTemp	 0.070634847
WinterMaxTemp	-0.069911994
FallMinTemp		-0.03830064
FallMaxTemp		 0.032463611
SummerMinTemp	 0.017702734


#PC 2
Comp 2 important	Comp.1
WinterRain		-0.143197373
FallRain		-0.737290452
SummerRain		-0.641989229
SummerMaxTemp	 0.070634847
WinterMaxTemp	-0.069911994
FallMaxTemp		 0.032463611
SummerMinTemp	 0.017702734
WinterMinTemp	-0.105052136
FallMinTemp		-0.03830064


ASMI2 <- subset(ASMI, ASMI$Year < 2013)

par(mfrow=c(2,2))
hist(log(ASMI2$Total.Plants))	#above ground and dormant
hist(log(ASMI2$Total.Alive))	#above ground/annual growth
hist(ASMI2$X..Browsed)
hist(log(ASMI2$Total.Fruit + 1))

________________________________________ # length

asmi.raw <-read.csv("2014_RawData_AsMi.csv", header = T, as.is = T, na.strings = 'na')
Ln.year <- ddply(asmi.raw, .(AsMi_site_id, AsMi_plot_id, year), summarise,
	Ln = mean(length, na.rm = T)) 

asmi.ln <- merge(ASMI2, Ln.year, by.x = c('Year','Site','Plot'), by.y = c('year','AsMi_site_id','AsMi_plot_id'))

lm.ASMI.length <- lm(Ln ~ WinterRain, data= asmi.ln)
summary(lm.ASMI.length)

ggplot(asmi.ln, aes(WinterRain, Ln)) +
	geom_point() +
	stat_smooth(method = 'lm')

summary(lm(log(Total.Fruit+1) ~ Ln, data = asmi.ln))

ggplot(asmi.ln, aes(Ln, log(Total.Fruit)+1)) +
	geom_point() +
  theme_bw() 


summary(lm(log(Total.Fruit+1) ~ Ln + WinterRain, data = asmi.ln))

ggplot(asmi.ln, aes(WinterRain, log(Total.Fruit)+1), colour = as.factor(Ln)) +
	geom_point() +
  theme_bw() +
	stat_smooth(method = 'lm')




_______________________________________ #Fruit

#PC2 more important: WinterRain, FallRain, SummerRain
table(ASMI$Year)
table(ASMI$Year, ASMI$Total.Fruit)

lm.ASMI.fruit <- lm(log(Total.Fruit + 1) ~ ( FallRain+ WinterRain)^2, 
		data=ASMI)
summary(lm.ASMI.fruit)


lm.ASMI.fruit <- lm(log(Total.Fruit + 1) ~ FallRain, 
		data=ASMI)
summary(lm.ASMI.fruit)


lm.ASMI.fruit <- lm(log(Total.Fruit + 1) ~ FallRain+ WinterRain, 
		data=ASMI)
summary(lm.ASMI.fruit)

lm.ASMI.fruit <- lm(log(Total.Fruit + 1) ~ ( FallRain+ WinterRain + SummerRain + Ln)^2, 
		data=asmi.ln)
summary(lm.ASMI.fruit)
summary(step(lm.ASMI.fruit))

hist(log(ASMI$Total.Fruit+1))

library(ggplot2)
qplot(ASMI2$FallRain, ASMI2$WinterRain, 
	colour = log(ASMI2$Total.Fruit+1), size = log(ASMI2$Total.Fruit+1))
ggplot(ASMI, aes(WinterRain, log(Total.Fruit +1), colour = as.factor(Site))) +
	geom_point() +
	stat_smooth(method = 'lm')

ggplot(ASMI, aes(WinterRain, log(Total.Fruit +1), colour = as.factor(Site))) + 
	geom_point() +
	stat_smooth(method = 'lm', formula = y ~ poly(x,2),
		aes(colour = 'polynomial'),se = FALSE) + 
	stat_smooth(method = 'nls', formula = y ~ a*log(x) +b,
		aes(colour = 'logarithmic'),se = FALSE, start = list(a=1,b=1)) 

ggplot(ASMI, aes(FallRain, log(Total.Fruit +1), colour = as.factor(Site))) + 
	geom_point() +
	stat_smooth(method = 'lm', formula = y ~ poly(x,2),
		aes(colour = 'polynomial'),se = FALSE) + 
	stat_smooth(method = 'nls', formula = y ~ a*log(x) +b,
		aes(colour = 'logarithmic'),se = FALSE, start = list(a=1,b=1)) 

ggplot(ASMI, aes(SummerRain, log(Total.Fruit +1), colour = as.factor(Site))) + 
	geom_point() +
	stat_smooth(method = 'lm', formula = y ~ poly(x,2),
		aes(colour = 'polynomial'),se = FALSE) + 
	stat_smooth(method = 'nls', formula = y ~ a*log(x) +b,
		aes(colour = 'logarithmic'),se = FALSE, start = list(a=1,b=1)) 


______________________________________________________________________________________

#WinterRain:FallRain

rm(MidL)
rm(MidH)

#Mid: 	 
MidL <- unique(ave(unique(ASMI$WinterRain)))-unique(sd(unique(ASMI$WinterRain)))	
MidH <- unique(ave(unique(ASMI$WinterRain)))+unique(sd(unique(ASMI$WinterRain)))	

#Low: < MidL
#High: > MidH


#Mid: 	 
MidL <- unique(ave(unique(ASMI$FallRain)))-unique(sd(unique(ASMI$FallRain)))	
MidH <- unique(ave(unique(ASMI$FallRain)))+unique(sd(unique(ASMI$FallRain)))	




_________________________________________

lmFruit <- lm(log(Total.Fruit+1) ~ (WinterRain+ FallRain)^2, 
	data = ASMI)
summary(lmFruit)


lm.SumMin.low <- lm(log(Total.Fruit[FallRain< MidL] + 1) 
	~ WinterRain[FallRain< MidL] , data=ASMI)	
summary(lm.SumMin.low )
plot(ASMI$WinterRain[ASMI$FallRain< MidL] , 
	log(ASMI$Total.Fruit[ASMI$FallRain< MidL] + 1))
abline(lm.SumMin.low )

lm.SumMin.mid <- lm(log(Total.Fruit[FallRain>= MidL & FallRain<= MidH] + 1) ~ 
	WinterRain[FallRain>= MidL & FallRain<= MidH] , data=ASMI)	
summary(lm.SumMin.mid )
plot(ASMI$WinterRain[ASMI$FallRain>= MidL & ASMI$FallRain<= MidH] , 
	log(ASMI$Total.Fruit[ASMI$FallRain>= MidL & ASMI$FallRain<= MidH] + 1))
abline(lm.SumMin.mid )

lm.SumMin.high <- lm(log(Total.Fruit[FallRain> MidH] + 1) ~ WinterRain[FallRain> MidH] , data=ASMI)	
summary(lm.SumMin.high )
plot(ASMI$WinterRain[ASMI$FallRain> MidH] , log(ASMI$Total.Fruit[ASMI$FallRain> MidH] + 1))
abline(lm.SumMin.high )

#_________________________________________________
# spring/summer rain and length

summary(lm(log(Total.Fruit+1) ~ Ln * SummerRain, data = asmi.ln))
summary(lm(log(Total.Fruit+1) ~ Ln + SummerRain, data = asmi.ln))

mean(asmi.ln$Ln, na.rm = T)	#8.744
sd(asmi.ln$Ln, na.rm = T)	#8.55

asmi.ln$LnGrp <- 'Large'
asmi.ln$LnGrp[asmi.ln$Ln < mean(asmi.ln$Ln, na.rm = T)] <- 'Small'

ggplot(asmi.ln, aes(SummerRain, log(Total.Fruit +1), colour = LnGrp)) + 
	geom_point() +
	stat_smooth(method = 'lm', formula = y ~ poly(x,2),
		aes(colour = 'polynomial'),se = FALSE) + 
	stat_smooth(method = 'nls', formula = y ~ a*log(x) +b,
		aes(colour = 'logarithmic'),se = FALSE, start = list(a=1,b=1)) +
	stat_smooth(method = 'lm') +
	theme_bw() +
	xlab('Spring/Summer Rainfall') +
	ylab('Total Fruit per plot (log transformed)')  +
	facet_wrap(~LnGrp) 
#_________________________________________________
	
_________________________________________
# FallRain and WinterRain and SummerRain on Dormancy
hist(ASMI$X..Dormant)
hist(log(ASMI$X..Dormant+1))	#better

summary(step(lm(log(X..Dormant+1) ~ (WinterRain + FallRain + SummerRain)^2, 
		data = subset(ASMI, Year > 1995 & Year < 2014))))

lmDorm <- lm(log(X..Dormant+1) ~ FallRain, 
	data = subset(ASMI, Year > 1995 & Year < 2014))
summary(lmDorm)

summary(lm(log(X..Dormant+1) ~ WinterRain, 
	data = subset(ASMI, Year > 1995 & Year < 2014))

_________________________________
# WinterMin:FallRain on %Dormant

ggplot(ASMI, aes(X..Dormant/Total.Plants)) +
	geom_histogram()


summary(step(lm(X..Dormant/Total.Plants ~ (WinterRain + FallRain + SummerRain)^2, 
		data = subset(ASMI, Year > 1995 & Year < 2014))))


ASMI[300:330,1:5]	# Total.Plants are AGG and Dormant, Total.Alive is AGG




#@#$@#$@#$	Start here
#@#$@#$@#$#@#$@#$@#$
#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$
#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$
#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$#@#$@#$@#$




_______________________________________ #percent flowered

lm.ASMI.perfl <- lm(log(X..Flowered + 1) ~ ( WinterRain + FallRain + SummerRain)^2, data=ASMI)
summary(step(lm.ASMI.perfl))

#FallRain:SummerRain
#WinterRain:SummerRain
rm(MidL)
rm(MidH)

#Mid: 	 
MidL <- unique(ave(unique(ASMI$SummerRain)))-unique(sd(unique(ASMI$SummerRain)))	
MidH <- unique(ave(unique(ASMI$SummerRain)))+unique(sd(unique(ASMI$SummerRain)))

#Low: < MidL #[1] 15.80524
#High: > MidH #[1] 31.71101


_________________________________________

lm.SumMax.low <- lm(log(X..Flowered[SummerRain< MidL] + 1) ~ WinterRain[SummerRain< MidL] , data=ASMI)	
summary(lm.SumMax.low )
plot(ASMI$WinterRain[ASMI$SummerRain< MidL] , log(ASMI$X..Flowered[ASMI$SummerRain< MidL] + 1))
abline(lm.SumMax.low )

MidL <- unique(ave(unique(ASMI$WinterRain)))-unique(sd(unique(ASMI$WinterRain)))
# [1] 12.17907

MidH <- unique(ave(unique(ASMI$WinterRain)))+unique(sd(unique(ASMI$WinterRain)))
# [1] 26.02618

lm.SumMax.low <- lm(log(X..Flowered[WinterRain< MidL] + 1) ~ SummerRain[WinterRain< MidL] , data=ASMI)	
summary(lm.SumMax.low )
plot(ASMI$SummerRain[ASMI$WinterRain< MidL] , log(ASMI$X..Flowered[ASMI$WinterRain< MidL] + 1))
abline(lm.SumMax.low )

lm.SumMax.mid <- lm(log(X..Flowered[SummerRain >= MidL & SummerRain <= MidH] + 1) ~ 
	WinterRain[SummerRain >= MidL & SummerRain <= MidH] , data=ASMI)	
summary(lm.SumMax.mid )
plot(ASMI$WinterRain[ASMI$SummerRain >= MidL & ASMI$SummerRain <= MidH] , 
	log(ASMI$Total.Fruit[ASMI$SummerRain >= MidL & ASMI$SummerRain <= MidH] + 1))
abline(lm.SumMax.mid )

lm.SumMax.mid <- lm(log(X..Flowered[SummerRain > MidL] + 1) ~ 
	WinterRain[SummerRain > MidL] , data=ASMI)	
summary(lm.SumMax.mid )
plot(ASMI$WinterRain[ASMI$SummerRain > MidL] , 
	log(ASMI$Total.Fruit[ASMI$SummerRain > MidL] + 1))
abline(lm.SumMax.mid )




lm.SumMax.high <- lm(log(X..Flowered[WinterRain > MidH] + 1) ~ SummerRain[WinterRain > MidH] , data=ASMI)	
summary(lm.SumMax.high )
plot(ASMI$SummerRain[ASMI$WinterRain > MidH] , log(ASMI$X..Flowered[ASMI$WinterRain > MidH] + 1))
abline(lm.SumMax.high )

lm.SumMax.mid <- lm(log(X..Flowered[WinterRain >= MidL & WinterRain <= MidH] + 1) ~ 
	SummerRain[WinterRain >= MidL & WinterRain <= MidH] , data=ASMI)	
summary(lm.SumMax.mid )
plot(ASMI$SummerRain[ASMI$WinterRain >= MidL & ASMI$WinterRain <= MidH] , 
	log(ASMI$Total.Fruit[ASMI$WinterRain >= MidL & ASMI$WinterRain <= MidH] + 1))
abline(lm.SumMax.mid )

lm.SumMax.high <- lm(log(X..Flowered[WinterRain < MidL] + 1) ~ SummerRain[WinterRain < MidL] , data=ASMI)	
summary(lm.SumMax.high )
plot(ASMI$SummerRain[ASMI$WinterRain < MidL] , log(ASMI$X..Flowered[ASMI$WinterRain < MidL] + 1))
abline(lm.SumMax.high )



__________________________________________
####################################
## Sensitivities and Elasticities ##
####################################

## from a_Step 4 Building Projection Matrix....
## and run the first loops from b_Step 4_fencing.R

s1 	probabiltiy a seedling survivies
s2	probability a vegetative survives
s3	probability a reproductive surives
s4	probability a dormant survives (always 1)

g1	probability a seedling survives and grows (transitions to something other than dormant)
g2	probability a veg survives and grows
g3	probability	a reproductive survives and grows
g4 	probability a dormant survives and grows

f1	probaility that a seedling surivies, grows, produces fruits next year (transitions to reproductive)
f2	probability that veg survives, grows, produces fruits next year (to repro)
f3	probability that repro survives, maintains, produces fruits next year

r1	number of fruits produced by seedling and number that survives as seedlings next year
r3 	number of fruits produced by repro and number that survives as seedlings next year

--------------------------------------------Survival rates (s)
asmiA <- ASMIF42

head(asmiA)
table(asmiA$fate != "dead")

round(prop.table( table(Browsed = asmiA$browse, survived = asmiA$fate !="dead"), 1)*100, 1)

2012:
       survived
        FALSE TRUE
  FALSE  17.8 82.2	not browsed - much more likely to survive
  TRUE   27.6 72.4	browsed - 

2013:
       survived
Browsed FALSE TRUE
  FALSE  19.8 80.2
  TRUE   32.2 67.8

2014:
       survived
Browsed FALSE TRUE
  FALSE  18.6 81.4
  TRUE   31.5 68.5



round(prop.table( table(Fenced = asmiA$fenced, survived = asmiA$fate !="dead"), 1)*100, 1)

2012:
       survived
        FALSE TRUE
  FALSE  18.9 81.1	
  TRUE   21.8 78.2

2013:
      survived
Fenced FALSE TRUE
     n  21.8 78.2
     y  23.6 76.4

2014:
      survived
Fenced FALSE TRUE
     n  20.7 79.3
     y  22.4 77.6



round(prop.table( table(Stage = asmiA$stage, survived = asmiA$fate !="dead"), 1)*100, 1)	#same as asmiA

2012:
              survived
               FALSE  TRUE
  seedling      33.5  66.5
  vegetative    37.3  62.7
  reproductive  22.1  77.9
  dormant        0.0 100.0

2013:
              survived
Stage          FALSE  TRUE
  seedling      34.5  65.5
  vegetative    40.7  59.3
  reproductive  28.3  71.7
  dormant        0.0 100.0

2014:
              survived
Stage          FALSE TRUE
  seedling      33.8 66.2
  vegetative    40.2 59.8
  reproductive  26.9 73.1
  dormant        0.1 99.9	#??? what?! some error here!

subset(asmiA, stage == 'dormant' & fate == 'dead')	
# tag_id 890 (site 19) and 1453 site(26) in year 2012



s1 <- round(prop.table( table(asmiA$stage, survived = asmiA$fate !="dead"), 1)*100, 1)[1,2]
s2 <- round(prop.table( table(asmiA$stage, survived = asmiA$fate !="dead"), 1)*100, 1)[2,2]
s3 <- round(prop.table( table(asmiA$stage, survived = asmiA$fate !="dead"), 1)*100, 1)[3,2]
s4 <- round(prop.table( table(asmiA$stage, survived = asmiA$fate !="dead"), 1)*100, 1)[4,2]


---------------------------------------------Growth rates (g)

x <- subset(asmiA, fate!="dead", c(stage, fate))

head(x)

 #percent that a stage doesn't go to dormant and survives
round(prop.table( table(became_dormant = x$fate!="dormant", stage = x$stage), 2)*100, 1)
     
2012:
        seedling vegetative reproductive dormant
  FALSE     23.6       24.1         14.0    76.5
  TRUE      76.4       75.9         86.0    23.5

2013:
              stage
became_dormant seedling vegetative reproductive dormant
         FALSE     24.3       24.6         15.1    76.6
         TRUE      75.7       75.4         84.9    23.4

g1 <- round(prop.table( table(x$fate!="dormant", x$stage), 2)*100, 1)[2,1]
g2 <- round(prop.table( table(x$fate!="dormant", x$stage), 2)*100, 1)[2,2]
g3 <- round(prop.table( table(x$fate!="dormant", x$stage), 2)*100, 1)[2,3]
g4 <- round(prop.table( table(x$fate!="dormant", x$stage), 2)*100, 1)[2,4]

----------------------------------------------Flowering (f)

x2 <- subset(x, fate!="dormant")	#stubben did this it changes the percents after removing dormant..

#percent that a stage transitions to reproductive and survives
round(prop.table( table(x2$fate=="reproductive", x2$stage), 2)*100, 1) 

2012:
        seedling vegetative reproductive dormant
  FALSE     63.6       58.6         45.4    64.8
  TRUE      36.4       41.4         54.6    35.2

2013:
        seedling vegetative reproductive dormant
  FALSE     63.9       58.9         45.8    66.2
  TRUE      36.1       41.1         54.2    33.8


f1 <- round(prop.table( table(x2$fate=="reproductive", x2$stage), 2)*100, 1)[2,1]
f2 <- round(prop.table( table(x2$fate=="reproductive", x2$stage), 2)*100, 1)[2,2]
f3 <- round(prop.table( table(x2$fate=="reproductive", x2$stage), 2)*100, 1)[2,3]
f4 <- round(prop.table( table(x2$fate=="reproductive", x2$stage), 2)*100, 1)[2,4]


------------------------------------------------Fruit that survive to seedlings (r)

tapply(asmiA$fruits, asmiA$stage, mean, na.rm=TRUE)

2012:
    seedling   vegetative reproductive      dormant 
    5.859122     0.000000    57.379164     0.000000 

2013:
    seedling   vegetative reproductive      dormant 
    5.697648     0.000000    54.173224     0.000000 

#ratio of fruits to seedlings next year

r1 <- tapply(asmiA$fruits, asmiA$stage, mean, na.rm=TRUE)[1]
r3 <- tapply(asmiA$fruits, asmiA$stage, mean, na.rm=TRUE)[3]


library(popbio) # Mean of the matrices won't work without 'popbio' loaded

s03 = mean(pro.matrix)[1,3] / r3	#seedlings from reproductive plants 2+ years - this is the mean of the 
s01 = mean(pro.matrix)[1,1] / r1	#seedlings from seedlings 1 year

--------------------------------------------------Vital Rates

vr <- c(prop.table(table(asmiA$stage, surived=asmiA$fate !="dead"), 1)[1:3,2], prop.table( table(x$fate!='dormant', x$stage), 2)[2,],
		prop.table( table(x2$fate=='reproductive', x2$stage), 2)[2,], r1, r3, s01, s03)	#s01 and s03 are seedlings


## essentially: c(c(s1, s2, s3),c(g1, g2, g3, g4),c(f1, f2, f3, f4),s01, s03)

asmi.vr <- as.list(vr)

names(asmi.vr) <-c ( "s1", "s2", "s3", "g1", "g2", "g3", "g4", "f1", "f2", "f3", "f4", "r1", "r3", "s01", "s03")

asmi.el <-expression(
r1*s01,		0,			r3*s03,		0,
s1*g1*(1-f1),	s2*g2*(1-f2),	s3*g3*(1-f3),     g4*(1-f4),
s1*g1*f1,         s2*g2*f2,         s3*g3*f3,         g4*f4,
s1*(1-g1),        s2*(1-g2),        s3*(1-g3),        (1-g4)  )

matrix( sapply(asmi.el, eval, asmi.vr, NULL), nrow=4, byrow=TRUE)

prop.table(table(asmiA$fate, asmiA$stage),2)[2:4,]	#is the same 
projection.matrix(asmiA)

--------------------------------------------------Plot effects of changing vital rates

n <- length(asmi.vr)
vr <- seq(0,1,0.1)
vrsen <- matrix(numeric(n*length(vr)), ncol=n, dimnames=list(vr, names(asmi.vr)))

for(h in 1:n){
	asmi.vr2 <- asmi.vr
		for(i in 1:length(vr)){
			asmi.vr2[[h]]<-vr[i]
       		A<-matrix(sapply(asmi.el, eval,asmi.vr2 , NULL), nrow=sqrt (length(asmi.el)), byrow=TRUE)
       		vrsen[i,h] <- max(Re(eigen(A)$values))
    		}
}



matplot(vrsen, type="l", lwd=2, las=1, ylab = "AsMi population growth", xlab = "Value of vital rate",
	main = "Effects of changing vital rates")
text(seq(1,10,length.out = length(asmi.vr)), vrsen[10,], names(asmi.vr))

matplot(vrsen[, 8:11],  type='l', lwd=2, las=1, ylab="Asmi population growth", 
	xlab="Value of survival rate")
text(10, vrsen[10,8:11], names(asmi.vr)[8:11])


matplot(vrsen[, 1:3],  type='l', lwd=2, las=1, ylab="Asmi population growth", 
	xlab="Value of survival rate")


# s1 and s3
matplot(vrsen[, c(1,3)],  type='l', lwd=2, las=1, ylab="Asmi population growth", 
	xlab="Value of survival rate")
text(10, vrsen[10,c(1,3)], names(asmi.vr)[c(1,3)])


---------------------------------------------------------Sensitivities

x <- vitalsens(asmi.el, asmi.vr)

x[11,]
sum(x$elasticity)

barplot(t(x[1:11,2:3]), beside = TRUE, legend=TRUE, las=1, xlab="Vital rate")		#t() transposes a matrix, 1:11 includes up through flowering and the 2nd and 3rd are the sensitivity and elasticity
abline(h = 0)

#But reproductive to seedling is so big, dwarfs the others
barplot(t(x[,2:3]), beside = TRUE, legend=TRUE, las=1, xlab="Vital rate")	




---------------------------------------------------------Sensitivities

eigen.analysis(mean(notfenced.pro.matrix[2:18]))


eigen.analysis(mean(notfenced.pro.matrix))	# I think missleading with the first year and last...

	
par(mfrow=c(2,2))
stage.vector.plot(pop.projection(mean(Site.matrix$"5"[2:17]), 
	colSums(mean(Site.matrix$"5"[2])) )$stage.vectors, col = 2:4)
stage.vector.plot(pop.projection(mean(Site.matrix$"15"[2:17]), 
	colSums(mean(Site.matrix$"15"[2])) )$stage.vectors, col = 2:4)
stage.vector.plot(pop.projection(mean(Site.matrix$"19"[2:17]), 
	colSums(mean(Site.matrix$"19"[2])) )$stage.vectors, col = 2:4)
stage.vector.plot(pop.projection(mean(Site.matrix$"26"[2:17]), 
	colSums(mean(Site.matrix$"26"[2])) )$stage.vectors, col = 2:4)


mean(notfenced.pro.matrix)	# use this for transition figure













