# univariate linear model (ULM) example
library(distcomp)

# using the mtcars data set included with base R
# add a 'site' variable to mtcars
cardata <- mtcars
cardata$site <- c(rep(0, 16), rep(1, 16))

## First we apply the base R linear model to get the true model
# EX: regress disp ~ wt

lmOrig <- lm(disp ~ wt, data=cardata)
summary(lmOrig)

## We define the computation

lmDef <- data.frame(compType = names(availableComputations())[4],
                     id = "mtcar",
                     stringsAsFactors=FALSE)

library(opencpu)

## We split the data by site
siteData <- with(cardata, split(x=cardata, f=site))
nSites <- length(siteData)
sites <- lapply(seq.int(nSites),
                function(x) list(name = paste0("site", x),
                                 url = opencpu$url()))

ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) lmDef),
          siteData)

stopifnot(all(as.logical(ok)))

master <- LinearRegressionMaster$new(defnId = lmDef)

for (site in sites) {
  master$addSite(name = site$name, url = site$url)
}

result <- master$run()

master$summary()

print(master$summary(), digits=5)

sessionInfo()
