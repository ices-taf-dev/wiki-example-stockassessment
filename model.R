## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(stockassessment)

mkdir("model")

load("data/data.RData", verbose = TRUE)

conf <- loadConf(dat, taf.data.path("sam_config", "model.cfg"), patch = TRUE)
par <- defpar(dat, conf)

fit <- sam.fit(dat, conf, par)

retro_fit <- retro(fit, year = 5)

save(fit, file = "model/fit.RData")
save(retro_fit, file = "model/retro_fit.RData")
