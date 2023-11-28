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
load("C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Astragalus-microcymbus/Astragalus-microcymbus_Data/stage-fate2023.Rdata")
df <- asmi.all2



# make it assign fencing or not but do it by each plot seperately, I can go through and average later
StagePVA <- function(df,dormancy = 1){

  # Seedlings that are reproductive couldn't have come from the preceding year's fruits, it was alive at least in the preceding year
  # Change after counting what we marked as seedlings to not being seedling if reproductive, just missed
  df$stage[df$fruits > 0] <- "reproductive"

  require(dplyr)
  library(plyr)
  #Starting point vector, select among numbers each year
  # Data frame of years per plot, plot per site
  n_options_plot <- plyr::ddply(df, c("year","site","plot", "fenced"), function(x) return(table(x$stage)))

  ## Remove 'dormant' state
  ## Dormant all to dead, next time alive should be seedling or reproductive depending on status
  years <- sort(unique(df$year)) #start years of each transition, year:year+1

  # Create a list to hold output
  names <- c("plot.matrix",
             "pro.matrix",
             "Site",
             "Plot")
  SiteMatrix <- vector("list", length(names))
  names(SiteMatrix) <- names

  require(popbio)

  # Individuals that are new at the current census are considered seedlings,
  # some are reproductive when they were missed in their first year
  # These measure the fertility (fruit output) of seedlings
  # "Seedlings" that are reproductive came from seed at an earlier time, won't be reproductive
  # if they were a seedling that year - should add to seed stage somehow....

  ## 1
  # overall
  # seedlingsfert <- c()
  #
  # # projection matrices of of class specific vital rates
  # pro.matrix <- vector("list", length(years))
  # names(pro.matrix) <- years

  site.matrix <- vector("list", length(years))
  names(site.matrix) <- years
  sitepromatrix <- vector("list", length(years))
  names(sitepromatrix) <- years

  #Set variables
  Sites <- unique(df$site)

  ## All plots within a site
  for(s in Sites){
    for(i in years){

      fert <- df %>%
        dplyr::filter(year == i) %>%
        dplyr::filter(site == s)


      # Pre-breeding census
      # ?projection.matrix uses a column with a stage name as a fertility measure per plant
      # fruit production per individual as a percent of the total production that year. Time t
      # this times the number of seedlings in the next year

      # Newly tagged == seedlings
      seedlings <- nrow(df[df$year == i+1 & df$stage=="seedling", ])

      # Adding a fertility column to seedlings.
      fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))

      pro.matrix[[as.character(i)]] <- projection.matrix(fert)
      } # end for years loop
    }

  SiteMatrix$pro.matrix <- pro.matrix


  ## 3
  # Projection matrices divided by Plot

  plot.matrix <- vector("list", length(years))
  names(plot.matrix) <- years
  plotpromatrix <- vector("list", length(years))
  names(plotpromatrix) <- years

  #Set variables
  Plots <- unique(df$plot)


  ## Plot
  for(j in Plots){
    years <- unique(df$year[df$plot == j])

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


      fertplot$seedling <- seedlings * (fertplot$fruits / sum(fertplot$fruits, na.rm = T))

      plotpromatrix[[as.character(i)]] <- projection.matrix(fert)

    }

    plot.matrix[[as.character(j)]] <- plotpromatrix
  }

  # add each year list of matrices to each by site and for only fenced and only not fenced
  SiteMatrix$plot.matrix <- plot.matrix

  # The list returned from the function
  SiteMatrix
}
