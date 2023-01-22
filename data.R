## Preprocess data, write TAF data tables

## Before:
## After:

## load libraries
library(icesTAF)
library(stockassessment)

# ensure directory
mkdir("data")

#  Read underlying data from bootstrap/data

# quick utility function
read.ices.taf <- function(...) {
  read.ices(taf.data.path("sam_data", ...))
}

#  ## Catch-numbers-at-age ##
catage <- read.ices.taf("cn.dat")

#  ## Catch-weight-at-age ##
wcatch <- read.ices.taf("cw.dat")
wdiscards <- read.ices.taf("dw.dat")
wlandings <- read.ices.taf("lw.dat")

#  ## Natural-mortality ##
natmort <- read.ices.taf("nm.dat")

# maturity
maturity <- read.ices.taf("mo.dat")

#  ## Proportion of F before spawning ##
propf <- read.ices.taf("pf.dat")

#  ## Proportion of M before spawning ##
propm <- read.ices.taf("pm.dat")

#  ## Stock-weight-at-age ##
wstock <- read.ices.taf("sw.dat")

# Landing fraction in catch at age
landfrac <- read.ices.taf("lf.dat")

# survey data
surveys <- read.ices.taf("survey.dat")

## 2 Preprocess data

# landings
latage <- catage * landfrac
datage <- catage * (1 - landfrac)

## 3 Write TAF tables to data directory
write.taf(
  c(
    "catage", "latage", "datage", "wstock", "wcatch",
    "wdiscards", "wlandings", "natmort", "maturity", "propf", "propm",
    "landfrac"
  ),
  dir = "data"
)


## write model files

dat <- setup.sam.data(
  surveys = surveys,
  residual.fleet = catage,
  prop.mature = maturity,
  stock.mean.weight = wstock,
  catch.mean.weight = wcatch,
  dis.mean.weight = wdiscards,
  land.mean.weight = wlandings,
  prop.f = propf,
  prop.m = propm,
  natural.mortality = natmort,
  land.frac = landfrac
)

save(dat, file = "data/data.RData")
