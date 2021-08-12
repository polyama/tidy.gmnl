# library(remotes)
# remotes::install_github('vincentarelbundock/modelsummary')

library(modelsummary)
library(gmnl)
library(mlogit)

# load tidy and glance for gnml
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

# add format for number of individuals
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nind",     "Num.Ind",       0,
  "nobs",      "Num.Obs",        0,
  "AIC",       "AIC",            1,
  "BIC",       "BIC",            1,
  "logLik",    "Log.Lik",        3
)


modelsummary(
  list("S-MNL model, ASCs not scaled" = smnl,
       "MIXL model with observed heterogeneity" = mixl.hier),
  estimate = "{estimate}{stars} ({std.error})",  statistic = NULL, 
  gof_map = gm,
  "markdown", 
  title = "Examples using the between Sydney and Melbourne, Australia from the AER package") 

#   Table: Examples using the between Sydney and Melbourne, Australia from the AER package
# 
#   |                    | S-MNL model, ASCs not scaled | MIXL model with observed heterogeneity |
#   |:-------------------|:----------------------------:|:--------------------------------------:|
#   |train × (intercept) |       -1.161* (0.588)        |             1.241 (1.899)              |
#   |bus × (intercept)   |       -1.914** (0.703)       |             0.295 (1.867)              |
#   |car × (intercept)   |      -7.309*** (1.281)       |           -8.915*** (2.451)            |
#   |wait                |      -0.139*** (0.021)       |            -0.180** (0.062)            |
#   |vcost               |      -0.126*** (0.034)       |             -0.056 (0.076)             |
#   |travel              |      -0.018*** (0.004)       |            -0.019* (0.009)             |
#   |gcost               |       0.099*** (0.029)       |             0.026 (0.080)              |
#   |tau                 |       0.465*** (0.123)       |                                        |
#   |travel.income       |                              |             0.000* (0.000)             |
#   |travel.size         |                              |             0.002 (0.002)              |
#   |wait.income         |                              |            -0.001+ (0.001)             |
#   |sd.travel           |                              |             0.009 (0.006)              |
#   |sd.wait             |                              |             0.101* (0.042)             |
#   |Num.Ind             |             210              |                  210                   |
#   |Num.Obs             |             210              |                  210                   |
#   |AIC                 |            372.3             |                 354.0                  |
#   |BIC                 |            399.1             |                 394.1                  |
#   |Log.Lik             |           -178.165           |                -164.987                |


####################
# Examples using the Electricity data set from the mlogit package
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

# coefficients estimates
modelsummary(
  list("MIXL with correlated random pars" = Elec.cor,
       "G-MNL with random ASCsm" = Elec.gmnl),
  estimate = "{estimate}{stars} ({std.error})",  statistic = NULL, 
  gof_map = gm,
  title = "Coefficients for MIXL and G-MNL models using the Electricity data from mlogit package",
  "markdown"
) 

#   Table: Coefficients for MIXL and G-MNL models using the Electricity data from mlogit package
#
#   |        | MIXL with correlated random pars | G-MNL with random ASCsm |
#   |:-------|:--------------------------------:|:-----------------------:|
#   |pf      |        -0.807*** (0.069)         |    -0.864*** (0.091)    |
#   |cl      |        -0.233*** (0.036)         |    -0.261*** (0.049)    |
#   |loc     |         1.644*** (0.191)         |    1.642*** (0.246)     |
#   |wk      |         1.456*** (0.164)         |    1.381*** (0.180)     |
#   |tod     |        -7.263*** (0.568)         |    -8.105*** (0.826)    |
#   |seas    |        -7.646*** (0.581)         |    -8.163*** (0.813)    |
#   |sd.cl   |         0.423*** (0.049)         |    0.472*** (0.068)     |
#   |sd.loc  |         1.796*** (0.210)         |    1.104*** (0.229)     |
#   |sd.wk   |         1.389*** (0.196)         |    0.977*** (0.147)     |
#   |sd.tod  |         2.474*** (0.289)         |    1.343*** (0.212)     |
#   |sd.seas |         2.262*** (0.263)         |    1.819*** (0.228)     |
#   |asc2    |                                  |     0.242+ (0.143)      |
#   |asc3    |                                  |      0.189 (0.154)      |
#   |asc4    |                                  |      0.158 (0.152)      |
#   |sd.asc2 |                                  |      0.227 (0.154)      |
#   |sd.asc3 |                                  |      0.163 (0.146)      |
#   |sd.asc4 |                                  |     0.371* (0.149)      |
#   |tau     |                                  |     0.364+ (0.194)      |
#   |gamma   |                                  |     0.533+ (0.293)      |
#   |Num.Ind |                63                |           63            |
#   |Num.Obs |               750                |           750           |
#   |AIC     |              1455.8              |         1506.2          |
#   |BIC     |              1552.9              |         1594.0          |
#   |Log.Lik |             -706.918             |        -734.107         |

# wtp (omits wtp for standard deviations)
modelsummary(
  list("MIXL with correlated random pars" = Elec.cor,
       "G-MNL with random ASCs" = Elec.gmnl),
  gof_map = gm,
  wrt = "pf",
  estimate = "{estimate}{stars} ({std.error})",  statistic = NULL, 
  title = "WTP for MIXL and G-MNL models using the Electricity data from mlogit package",
  "markdown"
) 

#   Table: WTP for MIXL and G-MNL models using the Electricity data from mlogit package
#
#   |          | MIXL with correlated random pars | G-MNL with random ASCs |
#   |:---------|:--------------------------------:|:----------------------:|
#   |wtp.cl    |         0.289*** (0.047)         |    0.302*** (0.054)    |
#   |wtp.loc   |        -2.036*** (0.248)         |   -1.899*** (0.246)    |
#   |wtp.wk    |        -1.803*** (0.206)         |   -1.598*** (0.189)    |
#   |wtp.tod   |         8.995*** (0.233)         |    9.378*** (0.296)    |
#   |wtp.seas  |         9.469*** (0.244)         |    9.444*** (0.303)    |
#   |wtp.asc2  |                                  |     -0.280 (0.171)     |
#   |wtp.asc3  |                                  |     -0.219 (0.179)     |
#   |wtp.asc4  |                                  |     -0.182 (0.179)     |
#   |wtp.tau   |                                  |    -0.421* (0.209)     |
#   |wtp.gamma |                                  |    -0.616+ (0.362)     |
#   |Num.Ind   |                63                |           63           |
#   |Num.Obs   |               750                |          750           |
#   |AIC       |              1455.8              |         1506.2         |
#   |BIC       |              1552.9              |         1594.0         |
#   |Log.Lik   |             -706.918             |        -734.107        |

####################
## Estimate a LC model with 2 classes
## added loc and wk as determinants of class membership 
Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1 + loc + wk,
              data = Electr,
              subset = 1:3000,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(Elec.lc)

## Estimate a MM-MIXL (latent class mixed model)
Elec.mm <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1 + loc + wk,
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

## Estimate a MM-MIXL (latent class mixed model) with correlated random parameters
Elec.mm.c <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1 + loc + wk,
                data = Electr,
                subset = 1:3000,
                model = 'mm',
                R = 100,
                panel = TRUE,
                correlation = TRUE,
                ranp = c(pf = "n", cl = "n", loc = "n", wk = "n", tod = "n",
                         seas = "n"),
                Q = 2,
                iterlim = 500)
summary(Elec.mm.c)

# coefficients estimates
modelsummary_wide(
  list("LC model" = Elec.lc, "MM-MIXL" = Elec.mm, "MM-MIXL corr" = Elec.mm.c),
  coef_group = "class", 
  statistic = NULL, estimate = "{estimate}{stars} ({std.error})",
  gof_map = gm,
  title = "Coefficients estimates for latent class models using the Electricity data set from the mlogit package",
  "markdown"
) 

#   Table: Coefficients estimates for latent class models using the Electricity data set from the mlogit package
# 
#   |               | LC model Class 1  | LC model Class 2  |  MM-MIXL Class 1   |  MM-MIXL Class 2  | MM-MIXL corr Class 1 | MM-MIXL corr Class 2 |
#   |:--------------|:-----------------:|:-----------------:|:------------------:|:-----------------:|:--------------------:|:--------------------:|
#   |pf             | -0.435*** (0.087) | -0.849*** (0.097) | -1.235*** (0.138)  | -0.385** (0.121)  |  -1.000*** (0.106)   |   -3.143** (1.057)   |
#   |cl             | -0.187*** (0.030) |  -0.116* (0.046)  |  -0.346* (0.136)   |   0.008 (0.073)   |   -0.111* (0.051)    |   -1.766** (0.612)   |
#   |loc            | 1.222*** (0.161)  | 1.610*** (0.271)  |   0.609* (0.238)   | 3.620*** (0.487)  |   2.088*** (0.286)   |   18.572** (6.132)   |
#   |wk             | 0.964*** (0.142)  | 1.397*** (0.214)  |  0.613** (0.197)   | 2.714*** (0.378)  |   1.842*** (0.241)   |    4.409* (1.919)    |
#   |tod            | -3.154*** (0.689) | -9.432*** (0.862) | -11.454*** (1.351) | -4.641*** (1.110) |  -9.540*** (0.919)   |  -21.974** (7.828)   |
#   |seas           | -3.422*** (0.692) | -9.300*** (0.878) | -11.639*** (1.347) | -4.983*** (1.077) |  -9.690*** (0.920)   |  -23.681** (8.641)   |
#   |cl.shares      |       0.562       |       0.438       |       0.545        |       0.455       |        0.800         |        0.200         |
#   |cl.m Intercept |                   | -1.549*** (0.245) |                    |  0.358+ (0.199)   |                      |  -1.108*** (0.194)   |
#   |cl.m loc       |                   | 1.696*** (0.271)  |                    |  -0.523* (0.225)  |                      |    -0.160 (0.239)    |
#   |cl.m wk        |                   | 1.427*** (0.273)  |                    | -0.818*** (0.229) |                      |   -0.577* (0.260)    |
#   |sd.pf          |                   |                   |  0.091** (0.030)   | 0.222*** (0.041)  |    0.319* (0.158)    |   3.782** (1.164)    |
#   |sd.cl          |                   |                   |  0.407*** (0.080)  | 0.384*** (0.091)  |   0.325*** (0.053)   |   6.928** (2.621)    |
#   |sd.loc         |                   |                   |   0.167 (0.331)    |   0.174 (0.300)   |   1.831*** (0.327)   |   17.267** (5.731)   |
#   |sd.wk          |                   |                   |   0.060 (0.212)    |  1.177** (0.406)  |   1.535*** (0.247)   |   15.404** (5.242)   |
#   |sd.tod         |                   |                   |  1.666*** (0.340)  |   0.363 (0.308)   |   2.511*** (0.618)   |   24.417** (7.874)   |
#   |sd.seas        |                   |                   |  1.967*** (0.361)  |   0.107 (0.252)   |   2.660*** (0.570)   |   24.740** (8.234)   |
#   |Num.Ind        |        63         |                   |         63         |                   |          63          |                      |
#   |Num.Obs        |        750        |                   |        750         |                   |         750          |                      |
#   |AIC            |      1611.6       |                   |       1395.3       |                   |        1422.7        |                      |
#   |BIC            |      1680.9       |                   |       1520.0       |                   |        1686.0        |                      |
#   |Log.Lik        |     -790.805      |                   |      -670.628      |                   |       -654.331       |                      |
#   

# wtp (omits wtp for standard deviations and for class membership coefficients)
modelsummary_wide(
  list("LC model" = Elec.lc, "MM-MIXL" = Elec.mm, "MM-MIXL corr" = Elec.mm.c),
  coef_group = "class", 
  wrt = "pf",
  statistic = NULL, estimate = "{estimate}{stars} ({std.error})",
  #estimate = "{estimate}{stars}",
  gof_map = gm,
  title = "WTP for latent class models using the Electricity data set from the mlogit package", 
  "markdown"
) 

#   Table: WTP for latent class models using the Electricity data set from the mlogit package
# 
#   |          | LC model Class 1  | LC model Class 2  | MM-MIXL Class 1  |  MM-MIXL Class 2  | MM-MIXL corr Class 1 | MM-MIXL corr Class 2 |
#   |:---------|:-----------------:|:-----------------:|:----------------:|:-----------------:|:--------------------:|:--------------------:|
#   |wtp.cl    | 0.429*** (0.101)  |  0.136** (0.051)  | 0.280** (0.101)  |  -0.021 (0.191)   |    0.111* (0.054)    |   0.562*** (0.109)   |
#   |wtp.loc   | -2.806*** (0.635) | -1.896*** (0.308) | -0.493* (0.194)  | -9.396** (2.934)  |  -2.088*** (0.332)   |  -5.908*** (1.065)   |
#   |wtp.wk    | -2.213*** (0.506) | -1.645*** (0.217) | -0.497** (0.158) | -7.044** (2.307)  |  -1.842*** (0.262)   |   -1.403** (0.500)   |
#   |wtp.tod   | 7.242*** (0.334)  | 11.109*** (0.530) | 9.278*** (0.407) | 12.046*** (1.893) |   9.541*** (0.469)   |   6.991*** (0.497)   |
#   |wtp.seas  | 7.859*** (0.314)  | 10.953*** (0.485) | 9.428*** (0.421) | 12.933*** (1.955) |   9.691*** (0.478)   |   7.534*** (0.528)   |
#   |cl.shares |       0.562       |       0.438       |      0.545       |       0.455       |        0.800         |        0.200         |
#   |Num.Ind   |        63         |                   |        63        |                   |          63          |                      |
#   |Num.Obs   |        750        |                   |       750        |                   |         750          |                      |
#   |AIC       |      1611.6       |                   |      1395.3      |                   |        1422.7        |                      |
#   |BIC       |      1680.9       |                   |      1520.0      |                   |        1686.0        |                      |
#   |Log.Lik   |     -790.805      |                   |     -670.628     |                   |       -654.331       |                      |