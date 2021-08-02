install.packages("modelsummary")
library("gmnl")
library("mlogit")
library(tidyverse)
library(data.table)
library(modelsummary)
library(stringr)

source("R/tidy.gmnl.R")

data("Electricity", package = "mlogit")

Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice",
                      varying = 3:26, shape = "wide", sep = "")

## Estimate a MNL model
Elec.mnl <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0, data = Electr,
                 subset = 1:3000,
                 model = 'mnl'
)

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

modelsummary(list(Elec.cor, Elec.gmnl),
             statistic = NULL, estimate = "{estimate}{stars} ({std.error})",
             "markdown"
) 

modelsummary(list(Elec.mnl, Elec.cor, Elec.gmnl),
             wrt = "pf",
             statistic = NULL, estimate = "{estimate}{stars} ({std.error})",
             "markdown"
) 


## Estimate a LC model with 2 classes
Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod | 0 | 0 | 0 | 1 + seas,
                data = Electr,
                subset = 1:3000,
                model = 'lc',
                panel = TRUE,
                Q = 2)
summary(Elec.lc)


## Estimate a MM-MIXL model
Elec.mm <- gmnl(choice ~ pf + cl + loc + wk + tod | 0 | 0 | 0 | 1 + seas,
                data = Electr,
                subset = 1:3000,
                model = 'mm',
                R = 30,
                panel = TRUE,
                ranp = c(pf = "n", cl = "n", loc = "n", wk = "n", tod = "n" ),
                Q = 2,
                iterlim = 500)
summary(Elec.mm)

source("R/tidy.gmnl.R")

get_estimates(Elec.lc) 

modelsummary(list(Elec.mm,Elec.lc), group = term ~ model + class, 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 
modelsummary(list(Elec.mm,Elec.lc), group = term ~ class + model, 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 
modelsummary(list(Elec.mm,Elec.lc), group = term + model ~ class , 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 
modelsummary(list(Elec.mm,Elec.lc), group = model + term ~ class , 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 


modelsummary(Elec.lc, group = term ~ model + class, 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 

###########################
###########################

library(nnet)

dat_multinom <- mtcars
dat_multinom$cyl <- sprintf("Cyl: %s", dat_multinom$cyl)

mod <- list(
  nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
  nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))

modelsummary(mod, group = term ~ y.level + model,"markdown")
modelsummary(mod, group = term ~ model + y.level,"markdown")
modelsummary(mod, group = y.level + term ~ model ,"markdown")
