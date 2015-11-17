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
#	df$year The column with the years of the study 	'table$year'
#	df$site The column with the site names		'table$site'
#	df$stage The column with stage				'table$stage'
#	table The whole data frame that has (1) site, (2) tag, (3) year, (4) measurment, (5) fruit, (6) browsed/measurment
#							(7) stage, (8) fate (9) plot - in some other order...


# To create the function in your R Console, run the following block of code:
#############################################################################################
############################### Stage PVA Function ##########################################
##################################### Start #################################################
#############################################################################################


StagePVA <- function(df,dormancy = 1){
  
  years <- sort(unique(df$year))
  
  # Create a list to hold output 
  names <- c("plot.matrix",   #Stage PVA per plot
             "pro.matrix","fenced.pro.matirx","notfenced.pro.matrix",    #Stage PVA overall
             "Site", "fenced", "notfenced") #Stage PVA per site: fenced vs. not fenced plots split per site
  SiteMatrix <- vector("list", length(names))
  names(SiteMatrix) <- names
  
  require(popbio)
  
  # Individuals that are new at the current census are considered seedlings, some are reproductive
  # These measure the fertility (fruit output) of seedlings
  # "Seedlings" that are reproductive came from seed at an earlier time, won't be reproductive 
  # if they were a seedling that year - should add to seed stage somehow....
  
  ## 1
  # overall
  seedlingsfert <- c()
  fencedseedlingsfert <- c()
  notfencedseedlingsfert <- c()
  
  # projection matrices of of class specific vital rates
  pro.matrix <- vector("list", length(years))
  names(pro.matrix) <- years
  fenced.pro.matrix <- vector("list", length(years))
  names(fenced.pro.matrix) <- years
  notfenced.pro.matrix <- vector("list", length(years))
  names(notfenced.pro.matrix) <- years
  
  ## Fencing
  for(i in years){
    
    # Not split by site, overall
    fert <- subset(df, x == i)
    fencedfert <- subset(df, x == i & df$fenced == "y")
    notfencedfert <- subset(df, x == i & df$fenced == "n")		
    
    # Pre-breeding census
    # ?projection.matrix uses a column with a stage name as a fertility measure per plant 
    # fruit production per individual as a percent of the total production that year. Time t
    # this times the number of seedlings that survived the next year
  
    seedlings <- nrow(subset(df, x == i+1 & df$stage== "seedling"))
    fencedseedlings <- nrow(subset(df, x == i+1 & df$stage== "seedling" & df$fenced == "y"))
    notfencedseedlings <- nrow(subset(df, x == i+1 & df$stage== "seedling" & df$fenced == "n"))
    
    # Adding a fertility column to seedlings. 
    # instead could Do something like if seedling and length > 10cm then call it vegetative/repro and add to 
    #  seed count???
    fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
    fencedfert$seedling <- fencedseedlings * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
    notfencedfert$seedling <- notfencedseedlings * (notfencedfert$fruits / sum(notfencedfert$fruits, na.rm = T))
  
    # Add some survival to dormant individuals other than 100% since we only note dormat indivdiuals when they are
    #seen above ground again so 100% survive # the column sums to 1, want some to transition to dead, 
    # lowering dormant to dormant
    death.to.dormants <- projection.matrix(fert)[5,5]*dormancy 
    
    pro.matrix[[as.character(i)]] <- projection.matrix(fert, add = c(4,4, death.to.dormants))
    fenced.pro.matrix[[as.character(i)]] <- projection.matrix(fencedfert, add = c(4,4, death.to.dormants))
    notfenced.pro.matrix[[as.character(i)]] <- projection.matrix(notfencedfert, add = c(4,4, death.to.dormants))
  }
 
  SiteMatrix$pro.matrix <- pro.matrix
  SiteMatrix$fenced.pro.matrix <- fenced.pro.matrix
  SiteMatrix$notfenced.pro.matrix <- notfenced.pro.matrix

  
  ## 2
  # Projection matrices divdied by Site
  
  Site.matrix <- vector("list", length(years))
  fenced.Site.matrix <- vector("list", length(years))
  notfenced.Site.matrix <- vector("list", length(years))
  
  promatrix <- vector("list", length(years))
  names(promatrix) <- years
  fenced.promatrix <- vector("list", length(years))
  names(fenced.promatrix) <- years
  notfenced.promatrix <- vector("list", length(years))
  names(notfenced.promatrix) <- years
  
  #Set variables
  sites <- unique(df$site)
  
  ## Fencing by site
  for(j in sites){
    for(i in years){
      
      # df$site is Site
      # x is Year
      fencedfert <- subset(df, x == i & df$site == j & df$fenced == "y")
      fencedseedlings <- nrow(subset(df, x == i+1 & df$stage == "seedling" & df$site == j & df$fenced == "y"))
      
      # no fencing in site 15 creates errors. Fencing removed from all in 2015
      if(nrow(fencedfert) == 0){ 
        fencedfert$seedling <- 0 * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
        fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
        fenced.promatrix[[as.character(i)]] <- projection.matrix(fencedfert)
      } else {
        fencedfert$seedling <- fencedseedlings * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
        fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
        fenced.promatrix[[as.character(i)]] <- projection.matrix(fencedfert)}	# end if else
      
      fert <- subset(df, x == i & df$site == j)
      notfencedfert <- subset(df, x == i & df$site == j & df$fenced == "n")
      
      # ?projection.matrix uses a column with a stage name as a fertility measure per plant 
      # fruit production per individual as a percent of the total production that year. Time t
      # this times the number of seedlings that survived the next year
      
      seedlings <- nrow(subset(df, x == i+1 & df$stage == "seedling" & df$site == j))
      notfencedseedlings <- nrow(subset(df, x == i+1 & df$stage == "seedling" & df$site == j & df$fenced == "n"))
      
      fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
      fert$seedling[is.nan(fert$seedling)] <- 0
      fert$seedling[is.na(fert$seedling)] <- 0
      notfencedfert$seedling <- notfencedseedlings * (notfencedfert$fruits / sum(notfencedfert$fruits, na.rm = T))
      notfencedfert$seedling[is.nan(notfencedfert$seedling)] <- 0
      notfencedfert$seedling[is.na(notfencedfert$seedling)] <- 0		 
      
      
      
      promatrix[[as.character(i)]] <- projection.matrix(fert, add = c(4,4, death.to.dormants))
      notfenced.promatrix[[as.character(i)]] <- projection.matrix(notfencedfert, add = c(4,4, death.to.dormants))
    }
    Site.matrix[[as.character(j)]] <- promatrix
    fenced.Site.matrix[[as.character(j)]] <- fenced.promatrix
    notfenced.Site.matrix[[as.character(j)]] <- notfenced.promatrix
  }
  
  # add each year list of matrices to each by site and for only fenced and only not fenced
  SiteMatrix$Site <- Site.matrix
  SiteMatrix$fenced <- fenced.Site.matrix
  SiteMatrix$notfenced <- notfenced.Site.matrix
  
  ## 3
  # Projection matrices divdied by Plot
  
  plot.matrix <- vector("list", length(years))
  names(plot.matrix) <- years
  plotpromatrix <- vector("list", length(years))
  names(plotpromatrix) <- years
  
  #Set variables
  Plots <- unique(df$plot)
  
  # sample size might be too small per plot
  ## Plot
  for(j in Plots){
    for(i in years){

      # df$year is Year
      fertplot <- subset(df, df$year == i & df$plot == j)
      
      # ?projection.matrix uses a column with a stage name as a fertility measure per plant 
      # fruit production per individual as a percent of the total production that year. Time t
      # this times the number of seedlings that survived the next year
      
      seedlings <- nrow(subset(df, df$year == i+1 & df$stage == "seedling" & df$plot == j))
      fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
      fert$seedling[is.nan(fert$seedling)] <- 0
      fert$seedling[is.na(fert$seedling)] <- 0
      
      death.to.dormants <- projection.matrix(fertplot)[5,5]*dormancy 
      
      plotpromatrix[[as.character(i)]] <- projection.matrix(fert, add = c(4,4, death.to.dormants))
      
    }
    
    plot.matrix[[as.character(j)]] <- plotpromatrix
  }
  
  # add each year list of matrices to each by site and for only fenced and only not fenced
  SiteMatrix$plot.matrix <- plot.matrix
  
  # The list returned from the function
  SiteMatrix
}


save(StagePVA, file = path.expand("P:/hackathon/GitPVA/StagePVAFunction.R"))
