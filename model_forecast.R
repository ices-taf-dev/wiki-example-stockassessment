
library(stockassessment)
load("model/fit.RData")

# scenarios:

###################
## USER edits here
###################

Ry <- 2012:2022

Fpa <- 0.69
Flim <- 1.47
Fmsy <- 0.37
Fmsyadvice <- Fmsy * 15519 / 24739

#######################
## USER edits end here
#######################

forecast <- list()

# run forecast scenarios
set.seed(12345)
forecast[[length(forecast) + 1]] <-
  forecast(
    fit,
    fscale = c(1, 1, 1, 1),
    rec.years = Ry,
    processNoiseF = FALSE,
    label = "SQ all years",
    addTSB = TRUE
  )

set.seed(12345)
zeroF <- 0.000001
forecast[[length(forecast) + 1]] <-
  forecast(
    fit,
    fscale = c(1, NA, NA, NA),
    fval = c(NA, zeroF, zeroF, zeroF),
    rec.years = Ry,
    processNoiseF = FALSE,
    label = "SQ then zero F",
    addTSB = TRUE
  )

set.seed(12345)
forecast[[length(forecast) + 1]] <-
  forecast(
    fit,
    fscale = c(1, NA, NA, NA),
    fval = c(NA, Fpa, Fpa, Fpa),
    rec.years = Ry,
    processNoiseF = FALSE,
    label = "SQ then Fpa",
    addTSB = TRUE
  )

set.seed(12345)
forecast[[length(forecast) + 1]] <-
  forecast(
    fit,
    fscale = c(1, NA, NA, NA),
    fval = c(NA, Flim, Flim, Flim),
    rec.years = Ry,
    processNoiseF = FALSE,
    label = "SQ then Flim",
    addTSB = TRUE
  )

set.seed(12345)
forecast[[length(forecast) + 1]] <-
  forecast(
    fit,
    fscale = c(1, NA, NA, NA),
    fval = c(NA, Fmsy, Fmsy, Fmsy),
    rec.years = Ry,
    processNoiseF = FALSE,
    label = "SQ then Fmsy",
    addTSB = TRUE
  )

set.seed(12345)
forecast[[length(forecast) + 1]] <-
  forecast(
    fit,
    fscale = c(1, NA, NA, NA),
    fval = c(NA, Fmsyadvice, Fmsyadvice, Fmsyadvice),
    rec.years = Ry,
    processNoiseF = FALSE,
    label = "SQ then Fmsyadvice",
    addTSB = TRUE
  )

save(forecast, file = "model/forecast.RData")
