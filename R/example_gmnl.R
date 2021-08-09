# library(remotes)
# detach("package:modelsummary", unload = TRUE)
# remotes::install_github('vincentarelbundock/modelsummary')
#install.packages("gmnl")

library(modelsummary)
library(gmnl)
library(mlogit)
#library(tidyverse)
#library(data.table)
#library(stringr)

source("R/tidy.gmnl.R")

## Examples using the Fishing data set from the AER package
data("TravelMode", package = "AER")
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                alt.levels = c("air", "train", "bus", "car"), chid.var = "individual")
## S-MNL model, ASCs not scaled
smnl <- gmnl(choice ~ wait + vcost + travel + gcost| 1, data = TM, 
            model = "smnl", R = 100, 
            notscale = c(1, 1, 1, rep(0, 4)))
summary(smnl)

## MIXL model with observed heterogeneity
mixl.hier <- gmnl(choice ~ vcost + gcost + travel + wait | 1 | 0 | income + size - 1,
                data = TM,
                model = "mixl",
                ranp = c(travel = "t", wait = "n"),
                mvar = list(travel = c("income","size"), wait = c("income")),
                R = 30,
                haltons = list("primes"= c(2, 17), "drop" = rep(19, 2)))
summary(mixl.hier)


modelsummary(
  list("S-MNL model, ASCs not scaled" = smnl,
       "MIXL model with observed heterogeneity" = mixl.hier),
  estimate = "{estimate}{stars} ({std.error})",  statistic = NULL, 
  "markdown", 
  title = "Examples using the between Sydney and Melbourne, Australia from the AER package") 

## Examples using the Electricity data set from the mlogit package
data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice",
                    varying = 3:26, shape = "wide", sep = "")
                    
## Estimate a MIXL model with correlated random parameters
Elec.cor <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0, data = Electr,
                subset = 1:3000,
                model = 'mixl',
                R = 10,
                panel = TRUE,
                ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n"),
                correlation = TRUE)
summary(Elec.cor)
cov.gmnl(Elec.cor)
se.cov.gmnl(Elec.cor)
se.cov.gmnl(Elec.cor, sd = TRUE)
cor.gmnl(Elec.cor)

## Estimate a G-MNL model, where ASCs are also random
Electr$asc2 <- as.numeric(Electr$alt == 2)
Electr$asc3 <- as.numeric(Electr$alt == 3)
Electr$asc4 <- as.numeric(Electr$alt == 4)

Elec.gmnl <- gmnl(choice ~ pf + cl + loc + wk + tod + seas + asc2 + asc3 + asc4 | 0,
                data = Electr,
                subset = 1:3000,
                model = 'gmnl',
                R = 30,
                panel = TRUE,
                notscale = c(rep(0, 6), 1, 1, 1),
                ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n",
                asc2 = "n", asc3 = "n", asc4 = "n"))
summary(Elec.gmnl)

modelsummary(
  list("MIXL with correlated random pars" = Elec.cor,
       "G-MNL with random ASCsm" = Elec.gmnl),
  estimate = "{estimate}{stars} ({std.error})",  statistic = NULL, 
  "markdown", 
  title = "Examples using the Electricity data set from the mlogit package") 

modelsummary(
  list("MIXL with correlated random pars" = Elec.cor,
       "G-MNL with random ASCs" = Elec.gmnl),
  wrt = "pf",
  estimate = "{estimate}{stars} ({std.error})",  statistic = NULL, 
  "markdown", 
  title = "Examples using the Electricity data set from the mlogit package") 


## Estimate a LC model with 2 classes
Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1 + loc + wk,
              data = Electr,
              subset = 1:3000,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(Elec.lc)

## Estimate a MM-MIXL model
Elec.mm <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1  + loc + wk,
                data = Electr,
                subset = 1:3000,
                model = 'mm',
                R = 30,
                panel = TRUE,
                ranp = c(pf = "n", cl = "n", loc = "n", wk = "n", tod = "n",
                seas = "n"),
                Q = 2,
                iterlim = 500)
summary(Elec.mm)

modelsummary_wide(
  list("LC model" = Elec.lc, "MM-MIXL model" = Elec.mm),
  coef_group = "class", 
  statistic = NULL, 
  estimate = "{estimate}{stars} ({std.error})",
  "markdown",
  title = "Examples of latent class models using the Electricity data set from the mlogit package"
) 

############


