
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
library(MARSS)	# model-selection using datasets with missing values

setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))
 asmi <-read.csv("2014_RawData_AsMi.csv", header = T, as.is = T, na.strings = 'na')

nrow(asmi)
asmi <- asmi[complete.cases(asmi[,1:3]),]

asmi$direction[asmi$direction == 'w'] <- 'W'
asmi$direction[asmi$direction == 'E '] <- 'E'
asmi$browsing[asmi$browsing == 'mammal'] <- 'Mammal'

asmi <- subset(asmi, AsMi_site_id != 1 & AsMi_site_id != 2 &
		AsMi_plot_id != 598)	# Should keep Beaver and Cebolla seperate, remove plot 598

ASMIF4 <- asmi[order(asmi$AsMi_site_id, asmi$AsMi_tag_id, asmi$year),] 

#### Set the order of the classes
stages <- c("seedling", "vegetative", "reproductive", "dormant","dead") 
	#Orders the status by the stage levels
  ASMIF4$status <- ordered(ASMIF4$status, levels = stages)	


# One length that was recorded crazy
ASMIF4$length[ASMIF4$length == 921] <- NA	
		
# Change insect browsing, mammal browsing, 
#	both to True or False any browsing 
ASMIF4$browsed[ASMIF4$browsing=="None"]<-FALSE		
ASMIF4$browsed[ASMIF4$browsing!="None"]<-TRUE


################################################
# AsMi_tag_id is unique per plant over all sites
# .y is the following year
# remove dead fate from stages because can't start dead!
#################### Merge year to year ########
ASMIF42 <- subset(merge(ASMIF4, ASMIF4, by = "AsMi_tag_id", sort = FALSE), year.x == year.y - 1) 	


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



setwd(path.expand("Q:/Research/Stats & Software/R CODE/Functions"))
load("StagePVAFunction.R")


### Figure out how to up the AGG by half or so and correspondingly decrease the dormant by that much
# assign either vegetative or reproductiv by that ratio seen in the given year


#	x The column with the years of the study 	'table$year'
#	y The column with the site names		'table$site'
#	z The column with fate				'table$fate'
#	a The column with stage				'table$stage'
#	df The whole data frame that has (1) site, (2) tag, (3) year, (4) measurment, (5) fruit, (6) browsed/measurment
#							(7) stage, (8) fate


#StagePVA(ASMIF42$year, ASMIF42$site, ASMIF42$fate, ASMIF42$stage, ASMIF42)

x <- ASMIF42$year
y <- ASMIF42$site
z <- ASMIF42$fate
a <- ASMIF42$stage
df <- ASMIF42


	library(popbio)
	library(reshape2)
#_________________________________________
# Creates transition tables, not used below
	rm(transtable)
	years <- sort(unique(x))				# table has been merged so first through second to last year
	transtable <- vector("list", length(years))	# yearly transitions
	names(transtable) <- years

	rm(Sitetable)
	Sitetable <- vector("list", length(years))
	sites <- unique(y)


### 
# "Within South Beaver Creek, only 56% of the total individuals with AGG in 2014 were above ground in July..."
# Either move number from dormant to Veg and Repro or just increase veg and repro by 
# can determine the number of 'new' so seedling individuals not there during July
# split 56% into the percent per stage class

#         status      Percent seen in July of total up throughout the season
# 1     seedling 0.4007918
# 2   vegetative 0.5369368
# 3 reproductive 0.7385417
# 4      dormant       0.6391351 (from below)

1/0.4007918	#2.495061 fewer from seedling to dormant
1/0.5369368	#1.862417 fewer from vegetative to dormant
1/0.7385417	#1.35402 fewer from reproductive to dormant
1/0.6391351	#1.564614 fewer from dormant to dormant

for(j in sites){
	for(i in years){
		# Fate down, stage across
		transtable[[as.character(i)]] <- table(z[x == i & y == j],	# table(rows: fate Year 1+1, Columns: stage year 1)
			a[x == i & y == j])
		mult.matrix <- matrix(c(0.4,0.4,0.4,0.4, 		# transition to seedling remains the same - delt with below
					0.54,0.54,0.54,0.54, 	# transition to vegetiatve increased by 1/.56 to reach total
					0.74,0.74,0.74,0.74, 	# transition to reproductive increased by 1/.56 to reach total 
					2.50,1.86,1.35,1.56, 	# transtion to dormant reduced by .56 (1/1.79)
					0.64,0.64,0.64,0.64), byrow = TRUE, nrow = 5,	# transition to dead remains the same
				dimnames = (list(rownames(transtable[[as.character(i)]]),colnames(transtable[[as.character(i)]]) ) ))
		transtable[[as.character(i)]] <- (as.matrix(transtable[[as.character(i)]]) %/% mult.matrix)
	}
		Sitetable[[as.character(j)]] <- transtable
}

print(Sitetable)

	fenced.transtable <- vector("list", length(years))	# yearly transitions
	names(fenced.transtable) <- years
	fenced.Sitetable <- vector("list", length(years))
	notfenced.Sitetable <- vector("list", length(years))


for(j in sites){
	for(i in years){
			fenced.transtable[[as.character(i)]] <- ((table(z[x == i & y == j & df$fenced == "y"],
										    a[x == i & y == j & df$fenced == "y"]))%/% mult.matrix)
	}
		fenced.Sitetable[[as.character(j)]] <- fenced.transtable
}


	notfenced.transtable <- vector("list", length(years))	# yearly transitions
	names(notfenced.transtable) <- years

for(j in sites){
	for(i in years){
			notfenced.transtable[[as.character(i)]] <- ((table(z[x == i & y == j & df$fenced == "n"],
										    a[x == i & y == j & df$fenced == "n"])) %/% mult.matrix)
	}
		notfenced.Sitetable[[as.character(j)]] <- notfenced.transtable
}

# Creates transition tables, not used below
#__________________________________________
# @#$@#$@#$@#$@#$@#$@#$@#$@#$@#$@#$@#$@#$ #
# Need to run 2014_PVA_asmi_editProjection.r
## "Q:\Research\All_Projects_by_Species\Astragalus SPECIES\Astragalus_microcymbus\R_Analysis\2014_PVA_asmi_editProjection.r.matrix"




seedlingsfert <- c()
 fencedseedlingsfert <- c()
 notfencedseedlingsfert <- c()

pro.matrix <- vector("list", length(years))
names(pro.matrix) <- years
 fenced.pro.matrix <- vector("list", length(years))
 names(fenced.pro.matrix) <- years
 notfenced.pro.matrix <- vector("list", length(years))
 names(notfenced.pro.matrix) <- years


####################################################################
## Fencing
	for(i in years){

		# Not split by site
		fert <- subset(df, x == i)
		 fencedfert <- subset(df, x == i & df$fenced == "y")
		 notfencedfert <- subset(df, x == i & df$fenced == "n")		

# ?projection.matrix uses a column with a stage name as a fertility measure per plant 
# fruit production per individual as a percent of the total production that year. Time t
# this times the number of seedlings that survived the next year

## The number of seedlings is short by 40% so increase to get the amount we'd see if we looked all year long
		seedlings <- nrow(subset(df, x == i+1 & a == "seedling"))	#/0.4		# because done in mult.matirx in projection.matrix2014
		 fencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & df$fenced == "y"))	#/0.4		# because done in mult.matirx in projection.matrix2014
		 notfencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & df$fenced == "n"))	#/0.4		# because done in mult.matirx in projection.matrix2014

		fert$seedling <- seedlings * ((fert$fruits / 0.4289253) / sum((fert$fruits / 0.4289253), na.rm = T))
		 fencedfert$seedling <- fencedseedlings * ((fencedfert$fruits / 0.4289253) / sum((fencedfert$fruits / 0.4289253), na.rm = T))
		 notfencedfert$seedling <- notfencedseedlings * ((notfencedfert$fruits / 0.4289253) / sum((notfencedfert$fruits / 0.4289253), na.rm = T))

## 2014projection.matrix() was updated to use the mult.matrix from above to increase transitions to vegetative and reproductive
##	and correspondingly decrease those changes to dormant.  
		pro.matrix[[as.character(i)]] <- projection.matrix2014(fert)
		 fenced.pro.matrix[[as.character(i)]] <- projection.matrix2014(fencedfert)
		 notfenced.pro.matrix[[as.character(i)]] <- projection.matrix2014(notfencedfert)
	}

	pro.matrix <<- pro.matrix
	 fenced.pro.matrix <<- fenced.pro.matrix
	 notfenced.pro.matrix <<- notfenced.pro.matrix



Site.matrix <- vector("list", length(years))
 fenced.Site.matrix <- vector("list", length(years))
 notfenced.Site.matrix <- vector("list", length(years))

promatrix <- vector("list", length(years))
names(promatrix) <- years
 fenced.promatrix <- vector("list", length(years))
 names(fenced.promatrix) <- years
 notfenced.promatrix <- vector("list", length(years))
 names(notfenced.promatrix) <- years

## Fencing by site
for(j in sites){
	for(i in years){

# y is Site
# x is Year
		 fencedfert <- subset(df, x == i & y == j & df$fenced == "y")
		 fencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & y == j & df$fenced == "y"))	#/0.4		# because done in mult.matirx in projection.matrix2014

	if(nrow(fencedfert) == 0){ 
		 fencedfert$seedling <- 0 * ((fencedfert$fruits / 0.4289253) / sum((fencedfert$fruits / 0.4289253), na.rm = T))
		 fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
		 fenced.promatrix[[as.character(i)]] <- projection.matrix2014(fencedfert)
		print("No fencing")
		} else {
		 fencedfert$seedling <- fencedseedlings * ((fencedfert$fruits / 0.4289253) / sum((fencedfert$fruits / 0.4289253), na.rm = T))
		 fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
		 fenced.promatrix[[as.character(i)]] <- projection.matrix2014(fencedfert)}	# end if else

		fert <- subset(df, x == i & y == j)
		 notfencedfert <- subset(df, x == i & y == j & df$fenced == "n")

# ?projection.matrix uses a column with a stage name as a fertility measure per plant 
# fruit production per individual as a percent of the total production that year. Time t
# this times the number of seedlings that survived the next year

		seedlings <- nrow(subset(df, x == i+1 & a == "seedling" & y == j))	#/0.4		# because done in mult.matirx in projection.matrix2014
		 notfencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & y == j & df$fenced == "n"))

		fert$seedling <- seedlings * ((fert$fruits / 0.4289253) / sum((fert$fruits / 0.4289253), na.rm = T))
		fert$seedling[is.nan(fert$seedling)] <- 0
		fert$seedling[is.na(fert$seedling)] <- 0
		 notfencedfert$seedling <- notfencedseedlings * ((notfencedfert$fruits / 0.4289253) / sum((notfencedfert$fruits / 0.4289253), na.rm = T))
		 notfencedfert$seedling[is.nan(notfencedfert$seedling)] <- 0
		 notfencedfert$seedling[is.na(notfencedfert$seedling)] <- 0		 
	
	 print(fert)
	
		promatrix[[as.character(i)]] <- projection.matrix2014(fert)
		 notfenced.promatrix[[as.character(i)]] <- projection.matrix2014(notfencedfert)
	}
		Site.matrix[[as.character(j)]] <- promatrix
		 fenced.Site.matrix[[as.character(j)]] <- fenced.promatrix
		 notfenced.Site.matrix[[as.character(j)]] <- notfenced.promatrix
}
	

Site.matrix <<- Site.matrix
fenced.Site.matrix <<- fenced.Site.matrix 
notfenced.Site.matrix <<- notfenced.Site.matrix 



splitA(mean(fenced.pro.matrix))
stable.stage(mean(fenced.pro.matrix))
fundamental.matrix(mean(fenced.pro.matrix))

mean(notfenced.pro.matrix[2:18])


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


lambda(mean(notfenced.pro.matrix[2:10]))	
[1] 0.9361567	# [1] 1.266468, when /0.4 twice	# [1] 0.9949851	#Changing the matrix.mult isn't changing the results...

notfenced.pro.matrix[11]	#2005 to 2006
notfenced.pro.matrix[18]	#2012 to 2013

lambda(mean(notfenced.pro.matrix[11:18]))	
[1] 0.9434027	# [1] 1.262042	#[1] 1.029018



lamdaSD(notfenced.pro.matrix, 1996, 2012)
[1] 0.9395024
[1] 0.4376207




#### current year - use current -1
cyr <- 2013


# All
lamdaSD(notfenced.pro.matrix, 1996, 2013)
[1] 0.9309918
[1] 0.444923
#[1] 1.250813, when did /0.4 for seedlings twice
#[1] 0.8929673
lamdaSD(notfenced.pro.matrix, 1996, cyr-1)
[1] 0.9395024
[1] 0.4376207


lamdaSD(pro.matrix, 1996, cyr-1)	# so transition is current year-2 to current year-1
[1] 0.9326848
[1] 0.3848734

# Pre-fencing all
lamdaSD(pro.matrix, 1996, 2004)
[1] 0.9357094
[1] 0.38351

# subset of fenced plots before fencing
lamdaSD(fenced.pro.matrix, 1996, 2004)
[1] 0.9952056
[1] 0.5406495

# Fenced vs. Not after 2005 
# Post fencing
lamdaSD(fenced.pro.matrix, 2005, cyr-1)
[1] 1.017542
[1] 0.5332993
[1] 1.33912	#[1] 1.034719 through 2013-2014, with /0.4 twice
[1] 0.8019848 #[1] 0.4300093 through 2013-2014

fenced <- lambda

lamdaSD(notfenced.pro.matrix, 2005, cyr-1)
[1] 0.9434027
[1] 0.5298791


not <- lambda
t.test(fenced, not, paired = T)
 

# Open long-term growth rate of all open sites
lamdaSD(notfenced.pro.matrix, 1996, cyr)
[1] 0.9309918
[1] 0.444923
lamdaSD(notfenced.pro.matrix, 1996, cyr-1)
[1] 0.9395024
[1] 0.4376207

# Site 05
lamdaSD(notfenced.Site.matrix$"5", 1996, cyr-1)
[1] 0.6957017
[1] 0.207777

cyr <- 2012
lamdaSD(notfenced.pro.matrix, 1996, cyr)
lamdaSD(notfenced.Site.matrix$"5", 1996, cyr)
lamdaSD(notfenced.Site.matrix$"15", 1996, cyr)
lamdaSD(notfenced.Site.matrix$"19", 1996, cyr)
lamdaSD(notfenced.Site.matrix$"26", 1996, cyr)


# Site 26 with its 598 that was added in 2004 excluded

lamdaSD(notfenced.Site.matrix$"26", 1996, 2004)
[1] 0.7928656
[1] 0.1337714

lamdaSD(fenced.Site.matrix$"26", 1996, 2004)
[1] 0.8201158
[1] 0.1715554

lamdaSD(notfenced.Site.matrix$"26", 2005, cyr-2)
exnotpost <- lambda
[1] 1.088511
[1] 0.2711699

lamdaSD(fenced.Site.matrix$"26", 2005, cyr-1)
exfenpost <- lambda
[1] 0.8809571
[1] 0.4547368

t.test(exnotpost, exfenpost)


