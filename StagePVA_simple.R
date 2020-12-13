##############################################################################
###############3#####			StagePVA			##################

# Each lambda for the duration of the study = "Sitemu"
# r squared	= "R.sq"
# mu with lower and upper 95% CI as "mu.s" with three columns of "mu.s$fit", "mu.s$lwr", and "mu.s$upr"
# the set of year intervals of the start year of study and each consecutive addition of years as "years"
#		"years$RangeStart" and "years$RangeEnd"
# Lambda and the lower and upper 95% CIs as "growth.exp" with "growth.exp$fit", "growth.exp$lwr", and "growth.exp$upr"

#	table The whole data frame that has (1) site, (2) tag, (3) year, (4) measurment, (5) fruit, (6) browsed/measurment
#							(7) stage, (8) fate (9) plot - in some other order...

#http://www.noamross.net/blog/2013/4/4/oleary-popbio-presentation.html
#df <- asmi.2
#table(asmi.2$plot,asmi.2$year)

# df should have a stage and fate, fruit like asmi.all2:
# > head(asmi.all2)
#    tag year length fruits    stage plot site fenced browse       fate
# 1 2150 2014      6      0 seedling    1    1      n  FALSE       dead
# 2 2151 2014      7      0 seedling    1    1      n  FALSE       dead
# 3 2152 2014      5      0 seedling    1    1      n   TRUE       dead
# 4 2153 2014      3      0 seedling    1    1      n  FALSE       dead
# 5 2154 2014      7      0 seedling    1    1      n   TRUE       dead
# 6 2155 2014     19      0 seedling    1    1      n  FALSE vegetative

# make it assign fencing or not but do it by each plot separately, I can go through and average later

StagePVA_single <- function(df){
  require(dplyr); require(popbio)

  #Starting point vector, select among numbers each year
  n_options_plot <- ddply(df, c("year","site","plot"), function(x) return(table(x$stage)))
  years <- sort(unique(df$year))[-c(1,length(unique(df$year)))] #start years of each transition, year:year+1

  # projection matrices of class specific vital rates
  projection.matrix <- vector("list", length(years))
  names(projection.matrix) <- years
  
  ## Project matrix
  for(i in years){
    fert <- subset(df, df$year == i)
  
    # Pre-breeding census
    # ?projection.matrix uses a column with a stage name as a fertility measure per plant
    # fruit production per individual as a percent of the total production that year. Time t
    # this times the number of seedlings in the next year

    seedlings <- nrow(df[df$year == i+1 & df$stage=="seedling" & df$length < 10, ]) + # small individuals called seedlings 
      nrow(df[df$tag %in% setdiff(df$tag[df$year == i+2],df$tag[df$year == i+1]) &
                  df$length >= 10 & df$year == i+2,]) # new but not small seedlings should be added from two years from now, likely germinated but were missed in t+1
    
    # Adding a fertility column to seedlings. For each individual, the seedlings in t+1 given how many fruit out of the total each individual produced
    fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
    
    projection.matrix[[as.character(i)]] <- projection.matrix(fert)
  }
  projection.matrix
}
