# univariate linear model (ULM) example with local data
library(distcomp)

# using the mtcars data set included with base R
# add a 'site' variable to mtcars
cardata <- mtcars
cardata$site <- c(rep(0, 16), rep(1, 16))

## First we apply the base R linear model to get the true model
# EX: regress disp ~ wt -> rename to x and y
colnames(cardata)[which(colnames(cardata)=='disp')] <- 'y'
colnames(cardata)[which(colnames(cardata)=='wt')] <- 'x'

lmOrig <- lm(y ~ x, data=cardata)
summary(lmOrig)

## We define the computation

lmDef <- data.frame(compType = names(availableComputations())[4],
                     id = "mtcar",
                     stringsAsFactors=FALSE)

# library(opencpu) # unnecessary for a local test

## We split the data by site
siteData <- with(cardata, split(x=cardata, f=site))
nSites <- length(siteData)

sites <- lapply(seq_along(siteData),
                function(x) list(name = paste0("site", x),
                                 worker = makeWorker(defn = lmDef, data = siteData[[x]])
                ))

ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) lmDef),
          siteData)

#stopifnot(all(as.logical(ok)))

master <- makeMaster(lmDef, debug=FALSE) 

for (site in sites) {
  master$addSite(name = site$name, worker = site$worker)
}

result <- master$run()
#master$summary()
print(master$summary(), digits=5)
print(coef(lmOrig), digits=5)
sessionInfo()
