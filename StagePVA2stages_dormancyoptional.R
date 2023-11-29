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

## Testing
# load("C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Astragalus-microcymbus/Astragalus-microcymbus_Data/stage-fate2023.Rdata")
# df <- asmi.all2

## Too few seedlings so need a different way (diff of tag numbers) to note new, and only have vegetative and reproductive

## Need by plot to have many MPMs per site over which to estimate VCV matrix and uncertainty
StagePVA <- function(df,dormancy = 1){

  # Seedlings that are reproductive couldn't have come from the preceding year's fruits, it was alive at least in the preceding year
  # Change after counting what we marked as seedlings to not being seedling if reproductive, just missed
  df$stage[df$fruits > 0] <- "reproductive"

  require(dplyr)
  library(plyr)

  ## Remove 'dormant' state
  ## Dormant all to dead, next time alive should be seedling or reproductive depending on status
  # df %>%
  #   filter(stage == "dormant")

  stages <- c("vegetative","reproductive","dormant")

  require(popbio)

  # Projection matrices divided by Plot
  # years <- unique(df$year)
  # plot.matrix <- vector("list", length(years))
  # names(plot.matrix) <- years
  # plotpromatrix <- vector("list", length(years))
  # names(plotpromatrix) <- years

  plotpromatrix <- vector("list")
  plot.matrix <- vector("list")
  #Set variables
  Plots <- unique(df$plot)


  ## Plot
  for(j in Plots){
    years <- unique(df$year[df$plot == j])

    for(i in years){

      # df$year is Year
      fert <- subset(df, df$year == i & df$plot == j)

      # ?projection.matrix uses a column with a stage name as a fertility measure per plant
      # fruit production per individual as a percent of the total production that year. Time t
      # this times the number of seedlings that survived the next year
      # seedlings <- nrow(subset(df, df$year == i+1 & df$stage == "seedling" & df$plot == j))
      newtags <- match(unique(df$tag[df$year == i+1 & df$plot == j]),unique(df$tag[df$year == i & df$plot == j]))
      # deadtags <- match(unique(df$tag[df$year == i & df$plot == j]),unique(df$tag[df$year == i+1 & df$plot == j]))
      # length(deadtags[is.na(deadtags)])
      seedlings <- length(newtags[is.na(newtags)])


      ## Seedlings are new vegetative ones
      fert$stage[fert$stage == "seedling"] <- "vegetative"
      # fert$stage <- droplevels(fert$stage) ## Will drop reproductive when there are none
      fert$stage <- factor(fert$stage, levels = stages)

      fert$vegetative <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
      fert$vegetative[is.nan(fert$vegetative)] <- 0
      fert$vegetative[is.na(fert$vegetative)] <- 0

      plotpromatrix[[as.character(i)]] <- projection.matrix(fert)

    }
    # First remove nulls
    plot.matrix[[as.character(j)]] <- plotpromatrix
  }

  # The list returned from the function
  plot.matrix
}


