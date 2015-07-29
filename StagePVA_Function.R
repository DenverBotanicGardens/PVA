##############################################################################
###############3#####			StagePVA			##################

# all variables with <<- assignment will be avaliable outside of the function
# 	Extract variable:
# Each lambda for the duration of the study as "Sitemu"	
# r squared	as "R.sq"
# mu with lower and upper 95% CI as "mu.s" with three columns of "mu.s$fit", "mu.s$lwr", and "mu.s$upr" 
# the set of year intervals of the start year of study and each consecutive addition of years as "years"
#		"years$RangeStart" and "years$RangeEnd"
# Lambda and the lower and upper 95% CIs as "growth.exp" with "growth.exp$fit", "growth.exp$lwr", and "growth.exp$upr"  

#	To use the function, enter "CountPVA(x,y)
#	x The column with the years of the study 	'table$year'
#	y The column with the site names		'table$site'
#	z The column with fate				'table$fate'
#	a The column with stage				'table$stage'
#	table The whole data frame that has (1) site, (2) tag, (3) year, (4) measurment, (5) fruit, (6) browsed/measurment
#							(7) stage, (8) fate


# To create the function in your R Console, run the following block of code:
#############################################################################################
############################### Stage PVA Function ##########################################
##################################### Start #################################################
#############################################################################################


StagePVA <- function(x,y,z,a,df){

	library(popbio)
#_________________________________________
# Creates transition tables, not used below
	rm(transtable)
	years <- sort(unique(x))				# table has been merged so first through second to last year
	transtable <- vector("list", length(years))	# yearly transitions
	names(transtable) <- years

	rm(Sitetable)
	Sitetable <- vector("list", length(years))
	sites <- unique(y)

for(j in sites){
	for(i in years){
		# Fate down, stage across
		transtable[[as.character(i)]] <- table(z[x == i & y == j],
			a[x == i & y == j])
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
			fenced.transtable[[as.character(i)]] <- table(z[x == i & y == j & df$fenced == "y"],
										    a[x == i & y == j & df$fenced == "y"])
	}
		fenced.Sitetable[[as.character(j)]] <- fenced.transtable
}


	notfenced.transtable <- vector("list", length(years))	# yearly transitions
	names(notfenced.transtable) <- years

for(j in sites){
	for(i in years){
			notfenced.transtable[[as.character(i)]] <- table(z[x == i & y == j & df$fenced == "n"],
										    a[x == i & y == j & df$fenced == "n"])
	}
		notfenced.Sitetable[[as.character(j)]] <- notfenced.transtable
}
# Creates transition tables, not used below
#_________________________________________


seedlingsfert <- c()
 fencedseedlingsfert <- c()
 notfencedseedlingsfert <- c()

pro.matrix <- vector("list", length(years))
names(pro.matrix) <- years
 fenced.pro.matrix <- vector("list", length(years))
 names(fenced.pro.matrix) <- years
 notfenced.pro.matrix <- vector("list", length(years))
 names(notfenced.pro.matrix) <- years


## Fencing
	for(i in years){

		# Not split by site
		fert <- subset(df, x == i)
		 fencedfert <- subset(df, x == i & df$fenced == "y")
		 notfencedfert <- subset(df, x == i & df$fenced == "n")		

# ?projection.matrix uses a column with a stage name as a fertility measure per plant 
# fruit production per individual as a percent of the total production that year. Time t
# this times the number of seedlings that survived the next year

		seedlings <- nrow(subset(df, x == i+1 & a == "seedling"))
		 fencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & df$fenced == "y"))
		 notfencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & df$fenced == "n"))

		fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
		 fencedfert$seedling <- fencedseedlings * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
		 notfencedfert$seedling <- notfencedseedlings * (notfencedfert$fruits / sum(notfencedfert$fruits, na.rm = T))

		pro.matrix[[as.character(i)]] <- projection.matrix(fert)
		 fenced.pro.matrix[[as.character(i)]] <- projection.matrix(fencedfert)
		 notfenced.pro.matrix[[as.character(i)]] <- projection.matrix(notfencedfert)
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
		 fencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & y == j & df$fenced == "y"))

	if(nrow(fencedfert) == 0){ 
		 fencedfert$seedling <- 0 * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
		 fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
		 fenced.promatrix[[as.character(i)]] <- projection.matrix(fencedfert)
		print("No fencing")
		} else {
		 fencedfert$seedling <- fencedseedlings * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
		 fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
		 fenced.promatrix[[as.character(i)]] <- projection.matrix(fencedfert)}	# end if else

		fert <- subset(df, x == i & y == j)
		 notfencedfert <- subset(df, x == i & y == j & df$fenced == "n")

# ?projection.matrix uses a column with a stage name as a fertility measure per plant 
# fruit production per individual as a percent of the total production that year. Time t
# this times the number of seedlings that survived the next year

		seedlings <- nrow(subset(df, x == i+1 & a == "seedling" & y == j))
		 notfencedseedlings <- nrow(subset(df, x == i+1 & a == "seedling" & y == j & df$fenced == "n"))

		fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
		fert$seedling[is.nan(fert$seedling)] <- 0
		fert$seedling[is.na(fert$seedling)] <- 0
		 notfencedfert$seedling <- notfencedseedlings * (notfencedfert$fruits / sum(notfencedfert$fruits, na.rm = T))
		 notfencedfert$seedling[is.nan(notfencedfert$seedling)] <- 0
		 notfencedfert$seedling[is.na(notfencedfert$seedling)] <- 0		 
	
	 
	
		promatrix[[as.character(i)]] <- projection.matrix(fert)
		 notfenced.promatrix[[as.character(i)]] <- projection.matrix(notfencedfert)
	}
		Site.matrix[[as.character(j)]] <- promatrix
		 fenced.Site.matrix[[as.character(j)]] <- fenced.promatrix
		 notfenced.Site.matrix[[as.character(j)]] <- notfenced.promatrix
}
	

Site.matrix <<- Site.matrix
fenced.Site.matrix <<- fenced.Site.matrix 
notfenced.Site.matrix <<- notfenced.Site.matrix 

}


save(StagePVA, file = "StagePVAFunction.R")

#############################################################################################
############################### Stage PVA Function ##########################################
####################################### End #################################################
#############################################################################################
