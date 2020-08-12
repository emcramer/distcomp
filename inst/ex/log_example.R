# local test of a logisticregression
library(stats)
library(distcomp)

## First we read in some sample data that was randomly split to different "sites"
cars <- read.csv("inst/ex/cars_new.csv")
idx <- sample(1:nrow(cars), 32)
cars <-cars[idx, ]

log_ex <- glm(formula = vs ~ wt + disp, data = mtcars, family = binomial)
summary(log_ex)

#Define the computation
print(names(availableComputations()))

logDef <- data.frame(compType = names(availableComputations())[6],
                     formula = "vs ~ wt + disp",
                     id = "LOG",
                     stringsAsFactors=FALSE)

print(logDef)

library(opencpu)
siteData <- with(cars, split(x=cars, f=sites))
nSites <- length(siteData)
sites <- lapply(seq_along(siteData),
                function(x) list(name = paste0("site", x),
                                 worker = makeWorker(defn = logDef, data = siteData[[x]])
                ))

ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) logDef),
          siteData)

master <- makeMaster(logDef)

for (site in sites) {
  master$addSite(name = site$name, worker = site$worker)
}

result <- master$run()
print(master$summary(), digits=5)

