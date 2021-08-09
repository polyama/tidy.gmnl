# library(remotes)
# detach("package:modelsummary", unload = TRUE)
# remotes::install_github('vincentarelbundock/modelsummary')
#install.packages("gmnl")

library(modelsummary)
library(gmnl)
library("mlogit")
library(tidyverse)
library(data.table)
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
se.cov.gmnl(Elec.cor)
se.cov.gmnl(Elec.cor, sd = TRUE)
cor.gmnl(Elec.cor)

wtp.gmnl(Elec.cor,wrt="pf")

vcov(Elec.cor, what = "coefficient")
vcov(Elec.cor, what = "ranp")
vcov(Elec.cor, what = "ranp", type = "sd")

wtp.gmnl(Elec.mnl,wrt="pf")

getSummary.gmnl(Elec.cor)

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

Elec.gmnl.cor <- gmnl(choice ~ pf + cl + loc + wk + tod + seas + asc2 + asc3 + asc4 | 0,
                  data = Electr,
                  subset = 1:3000,
                  model = 'gmnl',
                  R = 30,
                  panel = TRUE, correlation = TRUE,
                  notscale = c(rep(0, 6), 1, 1, 1),
                  ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n",
                           asc2 = "n", asc3 = "n", asc4 = "n"))
summary(Elec.gmnl)
wtp.gmnl(Elec.gmnl,wrt="pf")
wtp.gmnl(Elec.gmnl.cor, wrt="pf")

broom::tidy(Elec.gmnl)

modelsummary(list(Elec.mnl, Elec.cor, Elec.gmnl),
             wrt = "pf",
             statistic = NULL, estimate = "{estimate}{stars} ({std.error})",
             "markdown"
) 

modelsummary(list(Elec.mnl, Elec.cor, Elec.gmnl, Elec.gmnl.cor),
             statistic = NULL, estimate = "{estimate}{stars} ({std.error})",
             "markdown"
) 

modelsummary(list(Elec.mnl),
             wrt = "pf",
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

shares <- function(obj){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  #if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  Q <- length(cons_class)
  shares <- exp(cons_class) / sum(exp(cons_class))
  names(shares) <- paste("share q", 1:Q, sep = "=")  
  return(shares)
}

shares <- shares(Elec.lc)
shares <- shares(Elec.mm)

Elec.mm$model

exp(coef(lc)["(class)2"]) / (exp(0) + exp(coef(lc)["(class)2"]))

source('R/lc_helpers.R')

source("R/tidy.gmnl.R")

modelsummary(list(Elec.mm,Elec.lc), group = class + term ~ model , 
             statistic = NULL, 
             estimate = "{estimate} ({std.error}){stars}", "markdown") 

modlc = list("mm mnl" = Elec.mm, "latent class" = Elec.lc)

modelsummary(mod, group = term ~ model + class, 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 
modelsummary(list(Elec.mm,Elec.lc), group = term ~ class + model, 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 
modelsummary(list(Elec.mm,Elec.lc), group = term + model ~ class , 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 
modelsummary(list(Elec.mm,Elec.lc), group = model + term ~ class , 
             statistic = NULL, estimate = "{estimate} ({std.error}){stars}", "markdown") 


modelsummary(Elec.lc, group = term ~ model + class, "markdown") 


modelsummary_wide(
  list("mm mnl" = Elec.mm, "latent class" = Elec.lc), coef_group = "class", 
  statistic = NULL, estimate = "{estimate} ({std.error}){stars}",
  "markdown") 

modelsummary_wide(
  list("mm mnl" = Elec.mm), coef_group = "class",
  #"markdown", 
  statistic = NULL, estimate = "{estimate} ({std.error}){stars}") 


modelsummary(
  list("mm mnl" = Elec.mm, "latent class" = Elec.lc), 
  coef_group = "class", 
  group = term ~ model + class , 
  #"markdown",
  statistic = NULL, estimate = "{estimate} ({std.error}){stars}") 

# ###########################
# ###########################
# 
# library(nnet)
# 
# dat_multinom <- mtcars
# dat_multinom$cyl <- sprintf("Cyl: %s", dat_multinom$cyl)
# 
# gm <- modelsummary::gof_map
# gm$omit <- FALSE
# 
# 
# mod <- list(
#   nnet::multinom(cyl ~ mpg, data = dat_multinom, trace = FALSE),
#   nnet::multinom(cyl ~ mpg + drat, data = dat_multinom, trace = FALSE))
# 
# modelsummary(mod, group = term ~ y.level + model,"markdown")
# modelsummary(mod, group = term ~ model + y.level,"markdown")
# 
# 
# 
# modelsummary(mod, group = y.level + term ~ model,"markdown")
# modelsummary(mod, group = term + y.level ~ model,"markdown")
# 
# modelsummary_wide(mod, group = term  ~ y.level + model,"markdown")
# 
# modelsummary_wide(mod, "markdown")
# 
# 
