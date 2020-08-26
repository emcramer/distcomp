# local test of a logisticregression
library(stats)
library(distcomp)

## First we read in some sample data that was randomly split to different "sites"
admit_res <- read.csv("inst/ex/admit_new.csv")
idx <- sample(1:nrow(admit_res), 400)
admit_res <-admit_res[idx, ]

log_ex <- glm(formula = admit ~ gre + gpa, data = admit_res, family = binomial)
summary(log_ex)

#Define the computation

logDef <- data.frame(compType = names(availableComputations())[6],
                     formula = "admit ~ gre + gpa",
                     id = "LOG",
                     stringsAsFactors=FALSE)


library(opencpu)
siteData <- with(admit_res, split(x=admit_res, f=sites))
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
