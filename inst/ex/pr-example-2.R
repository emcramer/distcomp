# local test of a poisson regression
library(distcomp)

## First we read in some sample data that was randomly split to different "sites"
football <- read.csv("football.csv")
idx <- sample(1:nrow(football), 200)
football <- football[idx, ]

poisOrig <- glm(formula= totalGoals ~ year + month, data=football, family='poisson')
summary(poisOrig)

## We define the computation

poisDef <- data.frame(compType = names(availableComputations())[5],
                      formula = "totalGoals ~ year + month",
                      id = "POIS",
                      stringsAsFactors=FALSE)

library(opencpu)
## We split the data by site
siteData <- with(football, split(x=football, f=site))
nSites <- length(siteData)
sites <- list(
  list(name = "Site1", url = "http://127.0.0.1:5656/ocpu"),
  list(name = "Site2", url = "http://127.0.0.1:5656/ocpu")
)

ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) poisDef),
          siteData)

#stopifnot(all(as.logical(ok)))

master <- makeMaster(poisDef)

for (site in sites) {
  master$addSite(name = site$name, url = site$url)
}

result <- master$run()
# master$summary()
print(master$summary(), digits=5)
# sessionInfo()
