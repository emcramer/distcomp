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
sites <- lapply(seq_along(siteData),
                function(x) list(name = paste0("site", x),
                                 worker = makeWorker(defn = poisDef, data = siteData[[x]])
                ))

ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) coxDef),
          siteData)

#stopifnot(all(as.logical(ok)))

master <- makeMaster(poisDef)

for (site in sites) {
  master$addSite(name = site$name, worker = site$worker)
}

result <- master$run()
# master$summary()
print(master$summary(), digits=5)
sessionInfo()
