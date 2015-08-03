library(popbio)
library(reshape2)

names <- c("Site", "fenced", "notfenced") 
SiteMatrix <- vector("list", length(names))
names(SiteMatrix) <- names

SiteMatrix[2]


#setwd(path.expand("Q:/Research/All_Projects_by_Species/Astragalus SPECIES/Astragalus_microcymbus/R_Analysis/R_tables"))
#asmi <-read.csv("2014_RawData_AsMi.csv", header = T, as.is = T, na.strings = 'na')

head(asmi)

# remove plot 598 which was added in 2004 and remove Cebolla Creek sites which were added in 2014
asmi <- subset(asmi, AsMi_site_id != 1 & AsMi_site_id != 2 &
                 AsMi_plot_id != 598)	# Should keep Beaver and Cebolla seperate, 

## Run a bit of the code from 2014_PVA_asmi through line 123
am <- ASMIF42


names(am)
head(am)
am$sizebinary <- 1
am$sizebinary[am$status.y == "dead"] <- 0

# Cebolla will have to wait until this year to have enough data to do anything
cebolla$sizebinary <- 1
cebolla$sizebinary[cebolla$status.y == "dead"] <- 0


#Check if annual survival (i+1) is related to size, perform logistic regression of survival against size
# at census i
summary(glm(sizebinary ~ length.x, family = "binomial", data = am))

  #size of plants by survival, growth, and reproduction
  ggplot(am, aes(length.x, sizebinary)) +
   geom_point()

  ggplot(am, aes(length.x, flower.x)) +
    geom_point()
  
  ggplot(subset(am, browsing.x == "None"), aes(length.x, flower.x)) +
    geom_point()

  # looks like above about 10cm is when you might get some fruit. Could do two, small and big? 
  ggplot(subset(am, browsing.x == "None"), aes(length.x, fruit.x)) +
    geom_point()

# check by year
summary(glm(sizebinary ~ length.x, subset = year.x == 1995, family = "binomial", data = am))

# Not significant in 1996 to 1997
summary(glm(sizebinary ~ length.x, subset = year.x == 1996, family = "binomial", data = am))

# Not significantly related to size in years 1996-1997, 1998-1999, 2006-2007, 2008-2009, 2010-2011, 
# and 2011-2012
for(i in 1995:2013){
  print(i)
  print(summary(glm(sizebinary ~ length.x, subset = year.x == i, family = "binomial", data = am)))
}

# Morris and Doak - statistical justification for classifying individuals by size in projection matrix
# pg 189


# Log-linear model of suvival versus stage
# log-linear of size in time i to surivival in i+1
#library(lme4)
data(HairEyeColor)
HairEyeColor
fm <- loglin(table(am$sizebinary, am$status.x), margin = c(1,2))
1-pchisq(fm$lrt, fm$df)

stsur <- table(survived = am$sizebinary, status = am$status.x)
longstsur <- melt(stsur)
glm.asmi <- glm(value~survived*status, data=longstsur, family=poisson)
anova(glm.asmi, test="Chisq")



# Stage significantly different survival rates?

summary(glm(sizebinary ~ status.x, family = poisson, data = am))
  # need weights
  w <- as.vector(margin.table(table(am$status.x, am$sizebinary), 1))
summary(glm(sizebinary ~ status.x, weights = w, family = binomial, data = am))
chisq.test(margin.table(table(am$status.x, am$sizebinary),1), p = ()
           
           library(MASS)
loglm(sizebinary ~ (status.x * year.x), data = am)


# Structured PVA in deterministic environment

# Table of density of individual in each class at a time
n.t <- table(am$stage,am$year)

# Projection matrices
# 4 stages: seedling, vegetative, reproductive, dormant
# 1 reproductive term: combining seeds that germinate right away to ones in the seed bank
# basic matrix is s,v,r,d by s,v,r,d and number from one to other 
grep("year",names(am))#3,60
grep("status",names(am))#8,65
grep("fruit",names(am))#6,63
grep("fence", names(am))#56,113

#http://www.inside-r.org/packages/cran/popbio/docs/projection.matrix
am.pro <- am[,c(3,8,65,6,56)]
names(am.pro) <- c("Year","stage","fate","fruit","fence")
head(am.pro)
am.pro <- subset(am.pro, fate!="dead")
# Order stages in stage name
stages<-c("seed","seedling", "vegetative", "reproductive", "dormant")
am.pro$stage<-ordered(am.pro$stage,stages)
projection.matrix(am.pro, "stage","fate","fruit",stages, TF=TRUE)


# testing from the popbio package
data(aq.trans)
head(aq.trans)

# number of seedlings in 1996, The default values for seed.survival is 0.126
rec <- nrow(subset(am.pro, Year == 1997 & stage == "seedling"))
x <- subset(am.pro, Year == 1996)
aq.matrix(x, recruits=rec, seed.bank.size = 500)

# the transition matrix from 1996 to 1997
projection.matrix(x, fertility = "fruit")

## individual fertility estimates for recruits and seeds to contribute
## run from 2014_PVA_asmi line 147

am <- ASMIF42
names(am)
str(am)
years <- sort(unique(am$year))

# Create projection matrices for each site
# Pij: combining the vital rate of sj: survival probability, 
#   and gij: the probabily that the individual transitions
# Reproduction: fertility and sruvival - survival being seedlings and transition being 
# pre-breeding census: number of seeds per individual times number of seedlings that survived
# Need to account for what proportion of all seeds produced an individual has produced resulting
# in new seedlings since we cannot tell which individual came from which parent plant. 
# combining seedlings as number of seed produced per individual * fraction that don't germinate *
# seedlings that survive to the next census: fj*gij*sj

# Year matrices
yearmat <- vector("list", length(years))
names(yearmat) <- years
promat <- vector("list", length(unique(am$site)))
names(promat) <- unique(am$site)

for(s in unique(am$site)){
  for(yr in years){
    #Divide the data by site
    by.site <- subset(am, site == s & year == yr)
     
    #Need number of seedlings in <year+1> per site per year to determine how many fruit from 
    # <year> per individual out of all the fruit produced that year contributed to how many
    # seedlings the next year.
    seedlings <- nrow(subset(am, site == s & year == yr+1 & stage == "seedling"))
    
    #fertility per individual is calculated as the individual fruit production (~1 seed per fruit)
    # divided by all the fruit that were produced times the number of germinants that survived
    # to the next census
    
    #are we missing something by not including percent that doesn't germinate...??
    
    #Seeds germinated that were old so seed bank seeds likely will still germiante years
    # later
    
    #Should add a seed stage
    
    #From Yasemin's germination findings, average germination success over protocols
    seed.survival <- 0.3
    
    #A conservative estimate of the seed bank in the plot
    #  seed.bank.size <- 500
    #Seed bank size is likely just what was left over from the previous year? 
    seed.bank.size <- sum(subset(am, site == s & year == yr-1)$fruits, na.rm = TRUE)
    
    seeds.from.plants <- sum(by.site$fruits, na.rm = TRUE)
    recruit.rate <- seedlings/(seed.bank.size + seeds.from.plants + 1)
    
    #Fertility columns
    by.site$seedling <- by.site$fruits/sum(by.site$fruits, na.rm = TRUE) * seeds.from.plants * recruit.rate
    by.site$seed <- by.site$fruits * seed.survival

    
    # fruits should probably be swapped with a measured amount that survive. 
    # Using just fruit assumes they all survive and produce individuals the next year
    #Add seed bank survival and seed bank recruitment rate to the transition matrix
    
    # May want to add some death to dormants, say only half survive
    # death.to.dormants <- projection.matrix(by.site)[5,5]*0.5
    
    yearmat[[as.character(yr)]] <- projection.matrix(by.site, add=c(1,1, seed.survival, 2,1, recruit.rate)) #, 5,5, death.to.dormants))
  }
  promat[[as.character(s)]] <- yearmat
}

promat


stable.stage(mean(promat$'26'))
exp(stoch.growth.rate(promat$'26')$sim)
stoch.growth.rate(promat$'15')

