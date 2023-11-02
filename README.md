
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NetBenReg

<!-- badges: start -->
<!-- badges: end -->

The ‘NetBenReg’ package implements the net-benefit regression methods
for possibly censored observational cost-effectiveness data

## Installing the ‘NetBenReg’ package

You can install the development version of NetBenReg using the devtools
package:

``` r
devtools::install_github("shuaichencode/NetBenReg")
```

## Example

``` r
library(NetBenReg)

#Access help file for main function:
?NetBenReg

#load data
data(CEdata)

# fit covariate-adjusted NBR using Partitioned (PT) method,
# with Treatment x LBBB and Treatment x Female interactions, 
# effectiveness is quality-adjusted life years (QALY) 
fit1<-NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22],
    Eff=CEdata[,24:38], Part.times=1:15, Method='PT',Z=CEdata[,5:7],
    interaction=c("LBBB","Female"), Eff.only=TRUE, lambda=seq(0,6,1), L=10)
print(fit1)

# fit doubly robust covariate-adjusted NBR using Simple Weighted (SW) method, 
# effectiveness is survival time
fit2<-NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], 
    Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, 
    PS.Z=CEdata[,5:7], Doubly.Robust=TRUE, lambda=seq(0,6,1), L=10)
print(fit2)
```
