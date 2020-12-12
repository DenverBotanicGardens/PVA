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

# make it assign fencing or not but do it by each plot seperately, I can go through and average later
StagePVA <- function(df,dormancy = 1){

  '%ni%' <- Negate('%in%')

  require(dplyr)
  #Starting point vector, select among numbers each year
  # Here is where it matters that I've condensed all plots into a site since a few extra plots were added in site 26 and another...
  n_options <- ddply(df, c("year","site"), function(x) return(table(x$stage)))
  n_options_plot <- ddply(df, c("year","site","plot"), function(x) return(table(x$stage)))

  n_options_plot[n_options_plot$vegetative==0,]

  years <- sort(unique(df$year)) #start years of each transition, year:year+1


  # Create a list to hold output
  names <- c("plot.matrix",   #Stage PVA per plot
             "pro.matrix","fenced.pro.matirx","notfenced.pro.matrix",    #Stage PVA overall projection matrix
             "Site", "fenced", "notfenced", #Stage PVA per site: fenced vs. not fenced plots split per site
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

  fencedplots <- unique(df$Plot[df$Fence=='y'])

  ## All, Fenced, Open
  for(i in years){

    # Although now saying that these are from one population when really are multiple
    # Not split by site, overall
    fert <- subset(df, df$year == i)
    fencedfert <- subset(df, df$year == i & df$Plot %in% fencedplots)
    notfencedfert <- subset(df, df$year == i & df$Plot %ni% fencedplots)

    # Pre-breeding census
    # ?projection.matrix uses a column with a stage name as a fertility measure per plant
    # fruit production per individual as a percent of the total production that year. Time t
    # this times the number of seedlings in the next year

    seedlings <- nrow(df[df$year == i+1 & df$stage=="seedling" & df$fruits == 0, ]) +
      nrow(df[df$year == i+2 & df$stage=="seedling" & df$fruits > 0, ]) # the number of new to us i.e. seedling but clearly not seedling two years later

    fencedseedlings <- nrow(df[df$year == i+1 & df$Plot %in% fencedplots & df$stage=="seedling" & df$fruits == 0, ]) +
      nrow(df[df$year == i+2  & df$Plot %in% fencedplots &  df$stage=="seedling" & df$fruits > 0, ])

    notfencedseedlings <- nrow(df[df$year == i+1 & df$Plot %ni% fencedplots & df$stage=="seedling" & df$fruits == 0, ]) +
      nrow(df[df$year == i+2  & df$Plot %ni% fencedplots &  df$stage=="seedling" & df$fruits > 0, ])

    # Seedlings that are reproductive couldn't have come from the preceding year's fruits, it was alive at least in the preceding year
    # Change after counting what we marked as seedlings to not being seedling if reproductive, just missed
    df$stage[df$fruits > 0] <- "reproductive"

    # Adding a fertility column to seedlings.
    # instead could Do something like if seedling and length > 10cm then call it vegetative/repro and add to
    #  seed count???
    fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
    fencedfert$seedling <- fencedseedlings * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
    notfencedfert$seedling <- notfencedseedlings * (notfencedfert$fruits / sum(notfencedfert$fruits, na.rm = T))

    # Add some survival to dormant individuals other than 100% since we only note dormat indivdiuals when they are
    #seen above ground again so 100% survive # the column sums to 1, want some to transition to dead,
    # lowering dormant to dormant
    death.to.dormants_fert <- projection.matrix(fert)[length(levels(df$stage)),length(levels(df$stage))]*dormancy
    death.to.dormants_fencedfert <- projection.matrix(fencedfert)[length(levels(df$stage)),length(levels(df$stage))]*dormancy
    death.to.dormants_notfencedfert <- projection.matrix(notfencedfert)[length(levels(df$stage)),length(levels(df$stage))]*dormancy

    #alter the surviving dormants to something less than one
    pro.matrix[[as.character(i)]] <- projection.matrix(fert, add = c(length(levels(df$stage)),
                                                                     length(levels(df$stage)),
                                                                     death.to.dormants_fert))
    fenced.pro.matrix[[as.character(i)]] <- projection.matrix(fencedfert, add = c(length(levels(df$stage)),
                                                                                  length(levels(df$stage)),
                                                                                  death.to.dormants_fencedfert))
    notfenced.pro.matrix[[as.character(i)]] <- projection.matrix(notfencedfert, add = c(length(levels(df$stage)),
                                                                                        length(levels(df$stage)),
                                                                                        death.to.dormants_notfencedfert))
  }

  SiteMatrix$pro.matrix <- pro.matrix
  SiteMatrix$fenced.pro.matrix <- fenced.pro.matrix
  SiteMatrix$notfenced.pro.matrix <- notfenced.pro.matrix


  ## 2
  # Projection matrices divided by Site

  #Set variables
  sites <- unique(df$site)

  Site.matrix <- c()
  fenced.Site.matrix <- c()
  notfenced.Site.matrix <- c()

  promatrix <- vector("list", length(years))
  names(promatrix) <- years
  fenced.promatrix <- vector("list", length(years))
  names(fenced.promatrix) <- years
  notfenced.promatrix <- vector("list", length(years))
  names(notfenced.promatrix) <- years

  ## Fencing by site
  for(j in sites){
    for(i in years){

      # df$site is Site
      # x is Year
      rm(fencedfert, fencedseedlings)
      fencedfert <- subset(df, df$year == i & df$site == j & df$fenced == "y")
      fencedseedlings <- nrow(subset(df, df$year == i+1 & df$stage == "seedling" & df$site == j & df$fenced == "y"))

      # no fencing in site 15 creates errors. Fencing removed from all in 2015
      if(nrow(fencedfert) == 0){
        fencedfert$seedling <- 0 * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
        fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
        fenced.promatrix[[as.character(i)]] <- projection.matrix(fencedfert)
      } else {
        fencedfert$seedling <- fencedseedlings * (fencedfert$fruits / sum(fencedfert$fruits, na.rm = T))
        fencedfert$seedling[is.nan(fencedfert$seedling)] <- 0
        fenced.promatrix[[as.character(i)]] <- projection.matrix(fencedfert)}	# end if else

      rm(fert,notfencedfert)
      fert <- subset(df, df$year == i & df$site == j)
      notfencedfert <- subset(df, df$year == i & df$site == j & df$fenced == "n")

      # ?projection.matrix uses a column with a stage name as a fertility measure per plant
      # fruit production per individual as a percent of the total production that year. Time t
      # this times the number of seedlings that survived the next year

      seedlings <- nrow(subset(df, df$year == i+1 & df$stage == "seedling" & df$site == j))
      notfencedseedlings <- nrow(subset(df, df$year == i+1 & df$stage == "seedling" & df$site == j & df$fenced == "n"))

      fert$seedling <- seedlings * (fert$fruits / sum(fert$fruits, na.rm = T))
      fert$seedling[is.nan(fert$seedling)] <- 0
      fert$seedling[is.na(fert$seedling)] <- 0
      notfencedfert$seedling <- notfencedseedlings * (notfencedfert$fruits / sum(notfencedfert$fruits, na.rm = T))
      notfencedfert$seedling[is.nan(notfencedfert$seedling)] <- 0
      notfencedfert$seedling[is.na(notfencedfert$seedling)] <- 0

      death.to.dormants <- projection.matrix(fert)[length(levels(df$stage)),length(levels(df$stage))]*dormancy

      promatrix[[as.character(i)]] <- projection.matrix(fert,
                                                        add = c(length(levels(df$stage)),
                                                                length(levels(df$stage)),
                                                                death.to.dormants))
      notfenced.promatrix[[as.character(i)]] <- projection.matrix(notfencedfert,
                                                                  add = c(length(levels(df$stage)),
                                                                          length(levels(df$stage)),
                                                                          death.to.dormants))
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


      fertplot$seedling <- seedlings * (fertplot$fruits / sum(fertplot$fruits, na.rm = T))

      ## error, missing a fertility column with individual fertility rates, where's seedling?
      death.to.dormants <- projection.matrix(fertplot)[length(levels(df$stage)),length(levels(df$stage))]*dormancy

      plotpromatrix[[as.character(i)]] <- projection.matrix(fert, add = c(length(levels(df$stage)),
                                                                          length(levels(df$stage)),
                                                                          death.to.dormants))

    }

    plot.matrix[[as.character(j)]] <- plotpromatrix
  }

  # add each year list of matrices to each by site and for only fenced and only not fenced
  SiteMatrix$plot.matrix <- plot.matrix

  # The list returned from the function
  SiteMatrix
}
