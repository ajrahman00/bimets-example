library(usethis)
#install.packages("bimets")
library(bimets)

#See the link https://rviews.rstudio.com/2021/01/22/sem-time-series-modeling/
#define the Klein model
kleinModelDef <- "
MODEL

COMMENT> Modified Klein Model 1 of the U.S. Economy with PDL,
COMMENT> autocorrelation on errors, restrictions and conditional equation evaluations

COMMENT> Consumption with autocorrelation on errors
BEHAVIORAL> cn
TSRANGE 1923 1 1940 1
EQ> cn =  a1 + a2*p + a3*TSLAG(p,1) + a4*(wp+wg)
COEFF> a1 a2 a3 a4
ERROR> AUTO(2)

COMMENT> Investment with restrictions
BEHAVIORAL> i
TSRANGE 1923 1 1940 1
EQ> i = b1 + b2*p + b3*TSLAG(p,1) + b4*TSLAG(k,1)
COEFF> b1 b2 b3 b4
RESTRICT> b2 + b3 = 1

COMMENT> Demand for Labor with PDL
BEHAVIORAL> wp
TSRANGE 1923 1 1940 1
EQ> wp = c1 + c2*(y+t-wg) + c3*TSLAG(y+t-wg,1) + c4*time
COEFF> c1 c2 c3 c4
PDL> c3 1 2

COMMENT> Gross National Product
IDENTITY> y
EQ> y = cn + i + g - t

COMMENT> Profits
IDENTITY> p
EQ> p = y - (wp+wg)

COMMENT> Capital Stock with IF switches
IDENTITY> k
EQ> k = TSLAG(k,1) + i
IF> i > 0
IDENTITY> k
EQ> k = TSLAG(k,1)
IF> i <= 0

END
"
#load the model
kleinModel <- LOAD_MODEL(modelText = kleinModelDef)

## Analyzing behaviorals...
## Analyzing identities...
## Optimizing...
## Loaded model "kleinModelDef":
##     3 behaviorals
##     3 identities
##    12 coefficients
## ...LOAD MODEL OK

#define data
kleinModelData <- list(
    cn  =TIMESERIES(39.8,41.9,45,49.2,50.6,52.6,55.1,56.2,57.3,57.8,
                    55,50.9,45.6,46.5,48.7,51.3,57.7,58.7,57.5,61.6,65,69.7,
                    START=c(1920,1),FREQ=1),
    g   =TIMESERIES(4.6,6.6,6.1,5.7,6.6,6.5,6.6,7.6,7.9,8.1,9.4,10.7,
                    10.2,9.3,10,10.5,10.3,11,13,14.4,15.4,22.3,
                    START=c(1920,1),FREQ=1),
    i   =TIMESERIES(2.7,-.2,1.9,5.2,3,5.1,5.6,4.2,3,5.1,1,-3.4,-6.2,
                    -5.1,-3,-1.3,2.1,2,-1.9,1.3,3.3,4.9,
                    START=c(1920,1),FREQ=1),
    k   =TIMESERIES(182.8,182.6,184.5,189.7,192.7,197.8,203.4,207.6,
                    210.6,215.7,216.7,213.3,207.1,202,199,197.7,199.8,
                    201.8,199.9,201.2,204.5,209.4,
                    START=c(1920,1),FREQ=1),
    p   =TIMESERIES(12.7,12.4,16.9,18.4,19.4,20.1,19.6,19.8,21.1,21.7,
                    15.6,11.4,7,11.2,12.3,14,17.6,17.3,15.3,19,21.1,23.5,
                    START=c(1920,1),FREQ=1),
    wp  =TIMESERIES(28.8,25.5,29.3,34.1,33.9,35.4,37.4,37.9,39.2,41.3,
                    37.9,34.5,29,28.5,30.6,33.2,36.8,41,38.2,41.6,45,53.3,
                    START=c(1920,1),FREQ=1),
    y   =TIMESERIES(43.7,40.6,49.1,55.4,56.4,58.7,60.3,61.3,64,67,57.7,
                    50.7,41.3,45.3,48.9,53.3,61.8,65,61.2,68.4,74.1,85.3,
                    START=c(1920,1),FREQ=1),
    t   =TIMESERIES(3.4,7.7,3.9,4.7,3.8,5.5,7,6.7,4.2,4,7.7,7.5,8.3,5.4,
                    6.8,7.2,8.3,6.7,7.4,8.9,9.6,11.6,
                    START=c(1920,1),FREQ=1),
    time=TIMESERIES(NA,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,
                    1,2,3,4,5,6,7,8,9,10,
                    START=c(1920,1),FREQ=1),
    wg  =TIMESERIES(2.2,2.7,2.9,2.9,3.1,3.2,3.3,3.6,3.7,4,4.2,4.8,
                    5.3,5.6,6,6.1,7.4,6.7,7.7,7.8,8,8.5,
                    START=c(1920,1),FREQ=1)
)

#load time series into the model object
kleinModel <- LOAD_MODEL_DATA(kleinModel,kleinModelData)

## Load model data "kleinModelData" into model "kleinModelDef"...
## ...LOAD MODEL DATA OK

#estimate the model
kleinModel <- ESTIMATE(kleinModel, quietly=TRUE)

kleinModel <- ESTIMATE(kleinModel, eqList='cn')

##
## Estimate the Model kleinModelDef:
## the number of behavioral equations to be estimated is 1.
## The total number of coefficients is 4.
##
## _________________________________________
##
## BEHAVIORAL EQUATION: cn
## Estimation Technique: OLS
## Autoregression of Order  2  (Cochrane-Orcutt procedure)
##
## Convergence was reached in  6  /  20  iterations.
##
##
## cn                  =   14.83
##                         T-stat. 7.608    ***
##
##                     +   0.2589   p
##                         T-stat. 2.96     *
##
##                     +   0.01424  TSLAG(p,1)
##                         T-stat. 0.1735
##
##                     +   0.839    (wp+wg)
##                         T-stat. 14.68    ***
##
## ERROR STRUCTURE:  AUTO(2)
##
## AUTOREGRESSIVE PARAMETERS:
## Rho          Std. Error   T-stat.
##  0.2542       0.2589       0.9817
## -0.05251      0.2594      -0.2024
##
##
## STATs:
## R-Squared                      : 0.9827
## Adjusted R-Squared             : 0.9755
## Durbin-Watson Statistic        : 2.256
## Sum of squares of residuals    : 8.072
## Standard Error of Regression   : 0.8201
## Log of the Likelihood Function : -18.32
## F-statistic                    : 136.2
## F-probability                  : 3.874e-10
## Akaike's IC                    : 50.65
## Schwarz's IC                   : 56.88
## Mean of Dependent Variable     : 54.29
## Number of Observations         : 18
## Number of Degrees of Freedom   : 12
## Current Sample (year-period)   : 1923-1 / 1940-1
##
##
## Signif. codes:   *** 0.001  ** 0.01  * 0.05
##
##
## ...ESTIMATE OK

#we need to extend exogenous variables up to 1944
kleinModel$modelData <- within(kleinModel$modelData,{
    wg    = TSEXTEND(wg,  UPTO=c(1944,1),EXTMODE='CONSTANT')
    t     = TSEXTEND(t,   UPTO=c(1944,1),EXTMODE='LINEAR')
    g     = TSEXTEND(g,   UPTO=c(1944,1),EXTMODE='CONSTANT')
    k     = TSEXTEND(k,   UPTO=c(1944,1),EXTMODE='LINEAR')
    time  = TSEXTEND(time,UPTO=c(1944,1),EXTMODE='LINEAR')
})

#forecast model
kleinModel <- SIMULATE(kleinModel
                       ,simType='FORECAST'
                       ,TSRANGE=c(1941,1,1944,1)
                       ,simConvergence=0.00001
                       ,simIterLimit=100
                       ,quietly=TRUE
)

#get forecasted GNP
TABIT(kleinModel$simulation$y)

ts.plot(kleinModel$simulation$y, col = "red", ylab = "Klein Model GNP: Forecast")

ts.plot(kleinModel$modelData$y, col = "blue", ylab = "Klein Model GNP: Historical")

ts.plot(kleinModel$simulation$y, kleinModel$modelData$y, gpars = list(col = c("red", "blue")),
        ylab = "Klein Model GNP: Actual and Forecast")



