library("lavaan")
library("semPlot")
library("readr")
# Example of fitting and plotting a cv_regsem model in semPaths

library(psych)
library(lavaan)
library(regsem)

V1 <-read.table("E:/Rworkplace/GLM/V1.txt",header=TRUE, sep = " ")
V4 <-read.table("E:/Rworkplace/GLM/V4.txt",header=TRUE, sep = " ")
V1m <-read.table("E:/Rworkplace/GLM/V1m.txt",header=TRUE, sep = " ")
V4m <-read.table("E:/Rworkplace/GLM/V4m.txt",header=TRUE, sep = " ")


model <- ' 
  # latent variable definitions
     TC =~ TIC + TOC
     # x2 =~ Na + a*Mg + b*Ca + c*Mg
     # x3 =~ SiO4 + a*TIC
     # x1 =~ NO3 + Fe + DTN
     # x2 =~ Na + a*Mg + b*Ca + c*Mg
     # x3 =~ SiO4 + a*TIC

  # # regressions
  #   dem60 ~ ind60
  #   dem65 ~ ind60 + dem60
  # 
  # # residual correlations
  #   y1 ~~ y5
  #   y2 ~~ y4 + y6
  #   y3 ~~ y7
  #   y4 ~~ y8
  #   y6 ~~ y8
'
fit1 <- sem(model, data = V1)
summary(fit1, fit1.measures = TRUE)
par(mfrow=c(1,2))
semPaths(fit1, what = 'est',layout = 'tree',
         curveAdjacent = TRUE, style = "lisrel")
semPaths(fit1, what = 'paths',layout = 'tree',edge.label.cex = 0.5)







# The Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- lavaan(HS.model, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
summary(fit, fit.measures=TRUE)

## The industrialization and Political Democracy Example 
## Bollen (1989), page 332
model <- ' 
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # # regressions
  #   dem60 ~ ind60
  #   dem65 ~ ind60 + dem60
  # 
  # # residual correlations
  #   y1 ~~ y5
  #   y2 ~~ y4 + y6
  #   y3 ~~ y7
  #   y4 ~~ y8
  #   y6 ~~ y8
'

fit <- sem(model, data = PoliticalDemocracy)
summary(fit, fit.measures = TRUE)
par(mfrow=c(1,2))
semPaths(fit, what = 'est',layout = 'tree')
semPaths(fit, what = 'std',layout = 'tree')




## The famous Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
summary(fit, fit.measures = TRUE)



## linear growth model with a time-varying covariate
model.syntax <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # regressions
    i ~ x1 + x2
    s ~ x1 + x2

  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
'

fit <- growth(model.syntax, data = Demo.growth)
summary(fit)