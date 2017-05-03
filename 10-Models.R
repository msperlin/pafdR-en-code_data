#' # Financial Econometrics with R {#models}
#' 
#' 
#' The modelling tools from financial econometrics
#' 
#' The variety of models used in financial econome
#' 
#' - Linear models (OLS)
#' - Generalized linear models (GLS)
#' - Panel data models
#' - Arima models (Integrated Autoregressive Movin
#' - Garch models (generalized autoregressive cond
#' - Regime switching models
#' 
#' Here, we will not present a full description of
#' 
#' 
#' ## Linear Models (OLS)
#' 
#' A linear  model is, without a doubt, one of the
#' 
#' In finance, the most direct and popular use of 
#' 
#' A linear model with _N_ explanatory variables c
#' 
#' 
#' 
#' The left side of the equation, (`r if (my.engin
#' 
#' 
#' ### Simulating a Linear Model
#' 
#' Consider the following equation:
#' 
#' 
#' 
#' We can use R to simulate _1000_ observations fo
#' 
## ------------------------------------------------------------------------
set.seed(50)

# number of obs
nT <- 1000 

# set x as Normal (0, 1)
x <- rnorm(nT)

# set coefficients
my.alpha <- 0.5
my.beta <- 2

# build y
y <- my.alpha + my.beta*x + rnorm(nT)

#' 
#' Using `ggplot`, we can create a scatter plot to
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)

# set temp df
temp.df <- data.frame(x, y)

# plot it
p <- ggplot(temp.df, aes(x = x, y = y))
p <- p + geom_point(size=0.5)

print(p)

#' 
#' Clearly, there is a positive linear correlation
#' 
#' 
#' ### Estimating a Linear Model {#estimating-ols}
#' 
#' In R, the main function for estimating a linear
#' 
## ------------------------------------------------------------------------
# set df
lm.df <- data.frame(x, y)

# estimate linear model
my.lm <- lm(data = lm.df, formula = y ~ x)
print(my.lm)

#' 
#' The `formula` argument defines the shape of the
#' 
#' Argument `formula` allows other custom options,
#' 
## ------------------------------------------------------------------------
set.seed(15)

# set simulated dataset
N <- 100
df <- data.frame(x = runif(N),
                 y = runif(N),
                 z = runif(N),
                 group = sample(LETTERS[1:3],
                                N,
                                replace = TRUE ))

# Vanilla formula
#
# example: y ~ x + z
# model: y(t) = alpha + beta(1)*x(t) + beta(2)*z(t) + error(t)
my.formula <- y ~ x + z
print(lm(data = df, 
         formula = my.formula))

# vannila formula with dummies
#
# example: y ~ group + x + z
# model: y(t) = alpha + beta(1)*D_1(t)+beta(2)*D_2(t) + 
#               beta(3)*x(t) + beta(4)*z(t) + error(t)
# D_i(t) - dummy for group i
my.formula <- y ~ group + x + z
print(lm(data = df, 
         formula = my.formula))

# Without intercept
#
# example: y ~ -1 + x + z
# model: y(t) = beta(1)*x(t) + beta(2)*z(t) + error(t)
my.formula <- y ~ -1 + x + z
print(lm(data = df, 
         formula = my.formula))

# Using combinations of variables
# example: y ~ x*z
# model: y(t) = alpha + beta(1)*x(t) + beta(2)*z(t) + 
#               beta(3)*x(t)*z(t) + error(t)
my.formula <- y ~ x*z
print(lm(data = df, 
         formula = my.formula))

# Interacting variables
# example: y ~ x:group + z
# model: y(t) = alpha + beta(1)*z(t) + beta(2)*x(t)*D_1(t) + 
#               beta(3)*x(t)*D_2(t) + beta(4)*x(t)*D_3(t) + 
#               error(t)
# D_i(t) - dummy for group i
my.formula <- y ~ x:group + z
print(lm(data = df, 
         formula = my.formula))


#' 
#' The different options in the `formula` input al
#' 
#' The output of function `lm` is an object simila
#' 
## ------------------------------------------------------------------------
# print names in model
print(names(my.lm))

#' 
#' As you can see, there is a slot, called `coeffi
#' 
## ------------------------------------------------------------------------
print(my.lm$coefficients)

#' 
#' All coefficients are stored in `my.lm$coefficie
#' 
#' In our example of using `lm` with simulated dat
#' 
#' Experienced researchers have probably noted, fr
#' 
## ------------------------------------------------------------------------
print(summary(my.lm))

#' 
#' 
#' The estimated coefficients have high _T_ values
#' 
#' Additional information is available in the resu
#' 
## ------------------------------------------------------------------------
my.summary <- summary(my.lm)
print(names(my.summary))

#' 
#' Each of these elements contains information tha
#' 
#' Now, let's move to an example with real data. F
#' 
#' 
#' 
#' 
#' 
#' First, let's load the stock and SP500 data. \in
#' 
## ------------------------------------------------------------------------
# load stock data
load('data/SP500-Stocks-WithRet.RData')

# select rnd asset and filter data 
set.seed(10)
my.asset <- sample(my.df$ticker,1)
my.df.asset <- my.df[my.df$ticker == my.asset, ]

# load SP500 data
df.sp500 <- read.csv(file = 'data/SP500.csv', 
                     colClasses = c('Date','numeric'))

# calculate return
df.sp500$ret <- calc.ret(df.sp500$price)

# print datasets
print(nrow(my.df.asset))
print(nrow(df.sp500))

#' 
#' You can see the number of rows of the dataset f
#' 
## ------------------------------------------------------------------------
# find location of dates in df.sp500
idx <- match(my.df.asset$ref.date, df.sp500$date)

# create column in my.df with sp500 returns
my.df.asset$ret.sp500 <- df.sp500$ret[idx]

#' 
#' As a start, let's create the scatter plot with 
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)

p <- ggplot(data = my.df.asset, aes(x=ret.sp500, y=ret))
p <- p + geom_point()
p <- p + geom_smooth(method = 'lm')
print(p)

#' 
#' The figure shows a clear linear tendency;  the 
#' 
## ------------------------------------------------------------------------
# estimate beta model
my.beta.model <- lm(data = my.df.asset, formula = ret ~ ret.sp500)

# print it
print(summary(my.beta.model))

#' 
#' 
#' Previous output shows stock `r my.asset` has a 
#' 
#' 
#' ### Statistical Inference in Linear Models {#te
#' 
#' After estimating a model with function `lm`, th
#' 
#' 
#' In this example, the F statistic is `r summary(
#' 
#' Another type of test automatically executed by 
#' 
#' In the practice of research, it is likely that 
#' 
#' As a simple example, let's test a linear hypoth
#' 
## ------------------------------------------------------------------------
set.seed(10)

# number of time periods
nT <- 1000

# set parameters
my.intercept <- 0.5
my.beta <- 1.5

# simulate
x <- rnorm(nT)
y <- my.intercept + my.beta*x + rnorm(nT)

# set df
df <- data.frame(y, x)

# estimate model
my.lm <- lm(data = df, 
            formula = y ~ x )

#' 
#' After the estimation of the model, we use funct
#' 
#' 
#' 
#' With this matrix operation, we test the joint h
#' 
## ------------------------------------------------------------------------
library(car)

# set test matrix
test.matrix <- matrix(c(my.intercept,  # alpha test value
                        my.beta))  # beta test value

# hypothesis matrix 
hyp.mat <- matrix(c(1,0,
                    0,1),nrow = 2)

# do test
my.waldtest <- linearHypothesis(my.lm, 
                                hypothesis.matrix = hyp.mat, 
                                rhs = test.matrix)

# print result
print(my.waldtest)

#' 
#' As we can see, the test fails to reject the nul
#' 
#' Another family of tests commonly applied to lin
#' 
#' In R, we can use package `lmtest` [@lmtest] to 
#' 
## ---- results='hold'-----------------------------------------------------
library(lmtest)

# Breush Pagan test 1 - Serial correlation
# Null Hypothesis: No serial correlation in residual
print(bgtest(my.lm, order = 5))

# Breush Pagan test 2 - Homocesdasticity of residuals
# Null Hypothesis: homocesdasticity 
#                  (constant variance of residuals)
print(ncvTest(my.lm))

# Durbin Watson test - Serial correlation
# Null Hypothesis: No serial correlation in residual
print(dwtest(my.lm))

# Shapiro test  - Normality
# Null Hypothesis: Data is normally distributed
print(shapiro.test(my.lm$residuals))

#' 
#' As expected, the model with artificial data pas
#' 
#' Another interesting approach for validating lin
#' 
## ------------------------------------------------------------------------
library(gvlma)

# global validation of model
gvmodel <- gvlma(my.lm) 

# print result
summary(gvmodel)

#' 
#' The output of `gvlma` shows several tests perfo
#' 
#' 
#' ## Generalized Linear Models (GLM)
#' 
#' The generalized linear model (GLM) is a flexibl
#' 
#' We can write a general univariate GLM specifica
#' 
#' 
#' 
#' The main difference of a GLM model and a OLS mo
#' 
#' 
#' 
#' Notice, in this case, function _g()_ ensures an
#' 
#' 
#' ### Simulating a GLM Model
#' 
#' As an example, let's simulate the following GLM
#' 
#' 
#' 
#' 
#' In R, we use the following code to build the re
#' 
## ------------------------------------------------------------------------
set.seed(15)

# set number of obs
nT <- 500

# set x
x = rnorm(nT)

my.alpha <- 2
my.beta <- 5

# set probabilities
z = my.alpha + my.beta*x
p = exp(z)/(1+exp(z))

# set response variable
y = rbinom(n = nT,size = 1, prob = p)

#' 
#' Function `rbinom` creates a vector of 1s and 0s
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)

# set df for ggplot
df = data.frame(y, x)

# plot GLM sim
p <- ggplot(data = df, aes(x=seq_along(y) ,y=y))
p <- p + geom_point(size=0.5)
print(p)

#' 
#' Object `y` contains zeros and ones, as expected
#' 
#' 
#' ### Estimating a GLM Model
#' 
#' In R, the estimation of GLM models is accomplis
#' 
#' First, let's use the previously simulated data 
#' 
## ------------------------------------------------------------------------
# estimate GLM
my.glm <- glm(data=df, 
              formula = y~x , 
              family= binomial(link = "logit"))

# print it with summary
print(summary(my.glm))

#' 
#' The estimated coefficients are close to what we
#' 
#' Function `glm` offers many options for setting 
#' 
#' 
#' The first step in using a GLM model is to ident
#' 
#' As an example with real data from financial mar
#' 
#' 
## ------------------------------------------------------------------------
set.seed(15)

# select stock
my.stock <- sample(unique(my.df$ticker), 1)
my.df.asset <- my.df[my.df$ticker == my.stock, ]

# find location of dates in df.sp500
idx <- match(my.df.asset$ref.date, df.sp500$date)

# create column in my.df with sp500 returns
my.df.asset$ret.sp500 <- df.sp500$ret[idx]

# set column with dummy variable
my.df.asset$D_ret <- my.df.asset$ret > 0

# estimate model
my.glm <- glm(data=my.df.asset,
              formula = D_ret~ret.sp500 , 
              family= binomial(link = "probit"))

print(summary(my.glm))


#' 
#' The parameter for the market index is positive 
#' 
#' 
#' ## Panel Data Models 
#' 
#' Panel data models are advised when the modelled
#' 
#' The main motivation to use panel data models is
#' 
#' We can represent the simplest case of a panel d
#' 
#' 
#' 
#' Notice we now use index _i_ in the dependent an
#' 
#' 
#' ### Simulating Panel Data Models
#' 
#' Let's simulate a balanced panel data with fixed
#' 
## ------------------------------------------------------------------------
set.seed(25)

# number of obs for each case
nT <- 5

# set number of groups
N <- 12

# set possible cases
possible.cases <- LETTERS[1:N]

# set parameters
my.alphas <- seq(-10,10,length.out = N)
my.beta <- 1.5

# set indep var (x) and dates
indep.var <- sapply(rep(nT,N), rnorm)
my.dates <- Sys.Date() + 1:nT

# create response matrix (y)
response.matrix <- matrix(rep(my.alphas,nT), 
                          nrow = nT, 
                          byrow = TRUE) + 
  indep.var*my.beta + sapply(rep(nT,N),rnorm, sd = 0.25) 

# set df
sim.df <- data.frame(G = as.character(sapply(possible.cases, 
                                             rep, 
                                             times=nT )),
                     dates = rep(my.dates, times=N),
                     y = as.numeric(response.matrix),
                     x = as.numeric(indep.var), 
                     stringsAsFactors = FALSE)

# print result
print(str(sim.df))

#' 
#' The result is a `dataframe` object with `r nrow
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)

p <- ggplot(sim.df, aes(x=x, y=y))
p <- p + geom_point()
p <- p + facet_wrap(~G)

print(p)

#' 
#' The figure shows the strong linear relationship
#' 
#' 
#' ### Estimating Panel Data Models
#' 
#' With the artificial data simulated in the previ
#' 
## ---- message=FALSE------------------------------------------------------
library(plm)

# estimate panel data model with fixed effects
my.pdm <- plm(data = sim.df, 
              formula = y ~ x, 
              model = 'within',
              index = c('G','dates'))

# print result
print(summary(my.pdm,))

#' 
#' As expected, the parameters were correctly retr
#' 
## ------------------------------------------------------------------------
print(fixef(my.pdm))

#' 
#' Again, the simulated intercept values are close
#' 
#' As an example with real data, let's use the dat
#' 
## ---- message=FALSE------------------------------------------------------
library(plm)

# data from Grunfeld
data("Grunfeld")

# print it
print(str(Grunfeld))

#' 
#' The `Grunfeld` dataset contains company informa
#' 
#' A note here is important; given its high number
#' 
#' First, let's explore the raw data by estimating
#' 
## ------------------------------------------------------------------------
my.fct <- function(df) {
  # Estimates a linear model from Grunfeld data
  #
  # Args:
  #   df - dataframe from Grunfeld
  #
  # Returns:
  #   lm object
  
  my.model <- lm(data = df, 
                 formula = inv ~  value + capital)
  
  return(my.model)
}

# estimate model for each firm
my.l <- by(Grunfeld, 
           INDICES = Grunfeld$firm, 
           FUN = my.fct)

# print result
my.coefs <- sapply(my.l, coef)
print(my.coefs)

#' 
#' 
#' The results show a great discrepancy between th
#' 
## ------------------------------------------------------------------------
# test if all coef are the same across firms
my.pooltest <- pooltest(inv~value+capital, 
                        data = Grunfeld, 
                        model = "pooling")

# print result
print(my.pooltest)

#' 
#' The high F test and small p-value suggest the r
#' 
#' Before estimating the model, we need to underst
#' 
#' We can test the model specification using packa
#' 
## ------------------------------------------------------------------------
# set options for Hausman test
my.formula <- inv ~ value + capital
my.index <- c('firm','year')

# do Hausman test
my.hausman.test <- phtest(x = my.formula, 
                          data = Grunfeld,
                          model = c('within', 'random'),
                          index = my.index)

# print result
print(my.hausman.test)

#' 
#' The p-value of `r format(my.hausman.test$p.valu
#' 
#' After identifying the model, let's estimate it 
#' 
## ------------------------------------------------------------------------
# set panel data model with random effects
my.model <- 'random'
my.formula <- inv ~ value + capital
my.index <- c('firm','year')

# estimate it
my.pdm.random <- plm(data = Grunfeld, 
                     formula = my.formula, 
                     model = my.model,
                     index = my.index)

# print result
print(summary(my.pdm.random))

#' 
#' 
#' As expected, the coefficients are significant a
#' 
#' As a last example of using R in panel models wi
#' 
#' Package `systemfit` offers a function with the 
#' 
## ------------------------------------------------------------------------
library(systemfit)

# set pdataframe
p.Grunfeld <- pdata.frame(Grunfeld, c( "firm", "year" ))

# estimate sur
my.SUR <- systemfit(formula = inv ~value + capital,
                    method =  "SUR",
                    data = p.Grunfeld)
print(my.SUR)

#' 
#' The output object `my.SUR` contains the estimat
#' 
#' 
#' ## Arima Models
#' 
#' Using time series models is common in financial
#' 
#' A simple example of an Arima model is defined b
#' 
#' 
#' 
#' In this example, we have an ARIMA(AR = 1, D = 0
#' 
#' 
#' ### Simulating Arima Models
#' 
#' First, let's simulate an Arima model using func
#' 
## ---- tidy=FALSE---------------------------------------------------------
set.seed(1)

# set number of observations
my.n <- 5000

# set model's parameters
my.model <- list(ar = 0.5, ma = -0.1)
my.sd <- 1

# simulate model
my.ts <- arima.sim(n = my.n, 
                   model = my.model , 
                   sd = my.sd)

#' 
#' We can look at the result of the simulation by 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(ggplot2)

# set df
temp.df <- data.frame(y = unclass(my.ts), 
                      date = Sys.Date() + 1:my.n)

p <- ggplot(temp.df, aes(x = date, y = y))
p <- p + geom_line(size=0.5)

print(p)

#' 
#' The graph shows a time series with an average c
#' 
#' 
#' ### Estimating Arima Models {#arima-estimating}
#' 
#' To estimate a Arima model, we use function `ari
#' 
## ------------------------------------------------------------------------
# estimate arima model
my.arima <- arima(my.ts, order = c(1,0,1))

# print result
print(coef(my.arima))

#' 
#' As expected, the estimated parameters are close
#' 
## ------------------------------------------------------------------------
print(summary(my.arima))

#' 
#' We have the adjustment criteria in `aic`, resid
#' 
#' The identification of the Arima model,  definin
#' 
#' In the next example, we use function `auto.arim
#' 
## ------------------------------------------------------------------------
# read file
my.f <- 'data/SP500.csv'
df.SP500 <- read.csv(my.f)

calc.ret <- function(P) {
  # calculates arithmetic returns from a vector of prices
  #
  # Args:
  #   P - vector of prices (numeric)
  #
  # Returns:
  #   A vector of returns
  
  my.length <- length(P)
  ret <- c(NA, P[2:my.length]/P[1:(my.length - 1)] - 1)
  return(ret)
}

# set return column
df.SP500$ret <- calc.ret(df.SP500$price)

#' 
#' Before estimating the model, we need to check t
#' 
## ---- message=FALSE------------------------------------------------------
library(tseries)
print(adf.test(na.omit(df.SP500$ret)))

#' 
#' The result of the test shows a small p-value th
#' 
## ---- message=FALSE------------------------------------------------------
print(adf.test(df.SP500$price))

#' 
#' This time, we easily fail to reject the null hy
#' 
#' Function `forecast::auto.arima` estimates na Ar
#' 
## ------------------------------------------------------------------------
library(forecast)

# estimate arima model with automatic identification
my.autoarima <- auto.arima(x = df.SP500$ret)

# print result
print(my.autoarima)

#' 
#' The result tells us the best model for the retu
#' 
#' 
#' ### Forecasting Arima Models
#' 
#' We can obtain the forecasts of an Arima model w
#' 
## ------------------------------------------------------------------------
# forecast model
print(forecast(my.autoarima, h = 5))

#' 
#' ## Garch Models
#' 
#' Garch models relate to the seminal work of @eng
#' 
#' A GARCH model is modular. In its simplest forma
#' 
#' 
#' 
#' The `r if (my.engine!='epub3') {'$y_t$'} else {
#' 
#' 
#' ### Simulating Garch Models
#' 
#' R has no native function to simulate and estima
#' 
#' In `fGarch`, we simulate a model using function
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------
library(fGarch)

# set list with model spec
my.model = list(omega=0.001, 
                alpha=0.15, 
                beta=0.8, 
                mu=0.02, 
                ar = 0.1)

# set garch spec				
spec = garchSpec(model = my.model)

# print it
print(spec)

#' 
#' The previous code defines a Garch model equival
#' 
#' 
#' 
#' 
#' To simulate _1000_ observations of this model, 
#' 
## ------------------------------------------------------------------------
set.seed(20)
# simulate garch model
sim.garch = garchSim(spec, n = 1000)

#' 
#' We can visualize the artificial time series gen
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
# set df for ggplot
temp.df <- data.frame(sim.ret = sim.garch$garch, 
                      idx=seq_along(sim.garch$garch))

library(ggplot2)
p <- ggplot(temp.df, aes(x=idx, y=sim.ret))
p <- p + geom_line()
print(p)

#' 
#' The behaviour of the simulated series is simila
#' 
#' 
#' ### Estimating Garch Models {#estimating-garch}
#' 
#' The estimation of the parameters from a GARCH m
#' 
#' In the following example we estimate a Garch mo
#' 
## ---- tidy=FALSE---------------------------------------------------------
# estimate garch model
my.garchfit <- garchFit(data = sim.garch, 
                        formula = ~ arma(1,0) + garch(1,1), 
                        trace = FALSE)

#' 
#' To learn more about the estimated model, we can
#' 
## ------------------------------------------------------------------------
print(my.garchfit)

#' 
#' The resulting parameters from the estimation ar
#' 
#' Now, as an example with real data, let's estima
#' 
#' 
## ------------------------------------------------------------------------
library(FinTS)

# test for Arch effects
my.arch.test <- ArchTest(x = df.SP500$ret, lags = 5)

# print result
print(my.arch.test)

#' 
#' The evidence is strong for Arch effects in SP50
#' 
## ------------------------------------------------------------------------
# set object for estimation
df.est <- as.timeSeries(na.omit(df.SP500))

# estimate garch model for SP500
my.garchfit.sp500 <- garchFit(data = df.est , 
                              formula = ret ~ arma(1,0) + garch(1,1), 
                              trace = FALSE)

print(my.garchfit.sp500)


#' 
#' As expected, all Garch coefficients are signifi
#' 
#' 
#' ### Forecasting Garch Models
#' 
#' Forecasting Garch models involves two elements:
#' 
#' In package `fGarch`, both forecasts are calcula
#' 
#' 
## ------------------------------------------------------------------------
# static forecast for garch
my.garch.forecast <- predict(my.garchfit.sp500, n.ahead = 3)

# print df
print(my.garch.forecast)

#' 
#' The first column of the previous result is the 
#' 
#' 
#' ## Regime Switching Models 
#' 
#' Markov regime switching models are a specificat
#' 
#' As a way to motivate the model, consider the fo
#' 
#' 
#' 
#' where `r if (my.engine!='epub3') {'$S_t=1..k$'}
#' 
#' Now, let's assume the previous model has two st
#' 
#' 
#' 
#' where:
#' 
#' 
#' 
#' This representation implies two processes for t
#' 
#' As an example in finance, the dependent variabl
#' 
#' The different volatilities represent the higher
#' 
#' The changes of the states in the model can be s
#' 
#' A special regime switching model is markov swit
#' 
#' 
#' 
#' 
#' In the previous matrix, row _i_, column _j_ con
#' 
#' 
#' ### Simulating Regime Switching Models
#' 
#' In R, two packages are available for handling u
#' 
## ---- eval=FALSE---------------------------------------------------------
## install.packages("fMarkovSwitching",
##                  repos="http://R-Forge.R-project.org")

#' 
#' Once it is installed, let's look at its functio
#' 
## ------------------------------------------------------------------------
library(fMarkovSwitching)
print(ls('package:fMarkovSwitching'))

#' 
#' The package includes functions for simulating, 
#' 
#' 
#' 
#' 
#' The transition matrix will be given by:
#' 
#' 
#' 
#' This model has two states with different volati
#' 
## ------------------------------------------------------------------------
set.seed(10)
library(fMarkovSwitching)

# number of obs
nr <- 500 

# distribution of residuals
distrib <- "Normal"	

# number of states
k <- 2 	

# set transition matrix
P <- matrix(c(.9 ,.2,
              .1 ,.8), 
            nrow = 2, 
            byrow = T)

# set switching flag		   
S <- c(0,1)

# set parameters of model (see manual for details)
nS_param <- matrix(0)    
S_param <- matrix(0,sum(S),k)
S_param[,1] <-  .5         
S_param[,2] <- -.5

# set variance of model
sigma <- matrix(0,1,k)
sigma[1,1] <- sqrt(0.25)  # state 1
sigma[1,2] <- 1           # state 2

# build list
Coeff <- list(P = P               ,
              S = S               ,
              nS_param = nS_param ,
              S_param = S_param   ,
              sigma = sigma       )

# simulate model
my.ms.simul <- MS_Regress_Simul(nr,Coeff,k,distrib)

#' 
#' In the simulation function, argument `nS_param`
#' 
#' Once the model is simulated and available, let'
#' 
## ---- fig.height=my.fig.height, fig.width=my.fig.width-------------------
library(ggplot2)
df.to.plot <- data.frame(y = my.ms.simul@dep, 
                         x = Sys.Date()+1:my.ms.simul@nr,
                         states = my.ms.simul@trueStates[,1])

p <- ggplot(data = df.to.plot, aes(y=y, x=seq_along(y)))
p <- p + geom_line()
p <- p + labs(x='Time', y = 'Simulated time series')
print(p)

#' 
#' We can also look at the simulated states:
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)
df.to.plot <- data.frame(y = my.ms.simul@dep, 
                         x = Sys.Date()+1:my.ms.simul@nr,
                         states = my.ms.simul@trueStates[,1])

p <- ggplot(data = df.to.plot, aes(y=states, x=x))
p <- p + geom_line()
p <- p + labs(y='Probability of state 1')
print(p)

#' 
#' As expected, the model is switching from one st
#' 
#' 
#' ### Estimating Regime Switching Models
#' 
#' We can estimate a univariate markov switching m
#' 
## ---- message=FALSE, results='hide', cache=TRUE--------------------------
# set dep and indep 
dep <- my.ms.simul @dep
indep <- my.ms.simul@indep

# set switching parameters and distribution
S <- c(0,1)	
k <- 2		
distIn <- "Normal" 

# estimate the model
my.MS.model <- MS_Regress_Fit(dep,indep,S,k)	# fitting the model

#' 
#' Argument `dep` and `indep` sets the variables i
#' 
## ------------------------------------------------------------------------
# print estimation output
print(my.MS.model)

#' 
#' The estimated coefficients are close to the one
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=6, fig.width=7-------------------
plot(my.MS.model)	# plotting output

#' 
#' As an example with real data, let's estimate th
#' 
## ---- message=FALSE,results='hide', cache=TRUE---------------------------
# read file
my.f <- 'data/SP500.csv'
df.SP500 <- read.csv(my.f, 
                     colClasses = c('Date','numeric'))

# set calc.ret
calc.ret <- function(P) {
  return(c(NA, P[2:length(P)]/P[1:(length(P) - 1)] - 1))
}

# set return column
df.SP500$ret <- calc.ret(df.SP500$price)

# set input objects to MS_Regress_Fit
ret <- na.omit(df.SP500$ret)
dep <- matrix(ret, nrow = length(ret))
indep <- matrix(rep(1, length(dep)),nrow = length(dep))

S <- c(1)	# where to switch (in this case in the only indep)
k <- 2		# number of states
distIn <- "Normal" #distribution assumption

my.SP500.MS.model <- MS_Regress_Fit(dep,indep,S,k)	# fitting the model

#' 
#' And now, we check the result.
#' 
## ------------------------------------------------------------------------
# printing output
print(my.SP500.MS.model)	

#' 
#' 
#' 
#' The model identified two volatility regimes fro
#' 
#' A common figure in the analysis of markov switc
#' 
#' 
## ---- fig.height=my.fig.height, fig.width=my.fig.width-------------------
library(dplyr)

# get smooth probs of states
smooth.prob = as.numeric(my.SP500.MS.model@smoothProb[,1])

# build df to plot
df.to.plot <- data.frame(smooth.prob = smooth.prob, 
                         ref.date = df.SP500$date[2:nrow(df.SP500)],
                         price = df.SP500$price[2:nrow(df.SP500)])

# create factor from probs
df.to.plot$States <- ifelse(df.to.plot$smooth.prob > 0.5,
                            'State 1','State 2')

# plot with ggplot
p <- ggplot(df.to.plot,
            aes(y=price, x =ref.date, color=States)) +
  geom_point()

# plot it!
print(p)

#' 
#' The figure shows how the price increases in sta
#' 
#' 
#' ### Forecasting Regime Switching Models
#' 
#' Package `MS_Regress` provides function `MS_Regr
#' 
## ------------------------------------------------------------------------
# make static forecast of regime switching model
newIndep <- 1

my.for <- MS_Regress_For(my.SP500.MS.model, newIndep)

# print output
print(my.for)

#' 
#' The model predicts, the day after the last date
#' 
#' 
#' ## Dealing with Several Models
#' 
#' In the practice of research, it is likely we wi
#' 
#' In chapter \@ref(programming), we learned we ca
#' 
## ------------------------------------------------------------------------
set.seed(10)

# set number of stocks
n.stocks <- 4

# load data from .RData
load('data/SP500-Stocks-WithRet.RData')

# select tickers
my.tickers <- sample(unique(my.df$ticker), n.stocks)

# set my.df
my.df.stocks <- my.df[my.df$ticker %in% my.tickers, ]

# renew factors in ticker
my.df.stocks$ticker <- as.factor(as.character(my.df.stocks$ticker))

#' 
#' Now, what we want to do with this data is separ
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.l <- tapply(X = my.df.stocks$ret, 
               INDEX = my.df.stocks$ticker, 
               FUN = arima, 
               order = c(1,0,0))


#' 
#' Each model is available in `my.l`. To retrieve 
#' 
## ------------------------------------------------------------------------
print(sapply(X = my.l, FUN = coef))

#' 
#' A limitation is, by using `tapply`, we are rest
#' 
#' For an example of estimating several models wit
#' 
## ------------------------------------------------------------------------
# load SP500 data
df.sp500 <- read.csv(file = 'data/SP500.csv', 
                     colClasses = c('Date','numeric'))

# calculate return
df.sp500$ret <- calc.ret(df.sp500$price)


# find location of dates in df.sp500
idx <- match(my.df$ref.date, df.sp500$date)

# create column in my.df with sp500 returns
my.df$ret.sp500 <- df.sp500$ret[idx]

#' 
#' The next step is to create a function that will
#' 
## ------------------------------------------------------------------------
estimate.beta <- function(df) {
  # Function to estimate beta from dataframe of stocks returns
  #
  # Args:
  #   df - Dataframe with columns ret and ret.sp500
  #
  # Returns:
  #   The value of beta
  
  my.model <- lm(data = df, formula = ret ~ ret.sp500)
  
  return(coef(my.model)[2])
}

#' 
#' Now, we can use the previous function with `by`
#' 
## ---- tidy=FALSE---------------------------------------------------------
# calculate beta for each stock
my.betas <- by(data = my.df, 
               INDICES = my.df$ticker, 
               FUN = estimate.beta)

#' 
#' The values of the different `betas` are availab
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)

df.to.plot <- data.frame(betas = as.numeric(my.betas)) 

p <- ggplot(df.to.plot, aes(x=betas)) +
  geom_histogram()

print(p)


#' 
#' 
#' For the SP500 data, we find no negative value o
#' 
#' Another way of storing and managing several mod
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(dplyr)

my.tab <- my.df %>%
  group_by(ticker) %>%
  do(my.model = arima(x = .$ret, order = c(1,0,0)))

print(head(my.tab))

#' 
#' We have a list-column, called `my.model`, stori
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.model.tab <- my.df %>%
  group_by(ticker) %>%
  do(my.model = arima(x = .$ret, order = c(1,0,0))) %>%
  mutate(alpha = coef(my.model)[2],
         ar1 = coef(my.model)[1])

print(head(my.model.tab))

#' 
#' Another trick in handling models with `dplyr` i
#' 
## ---- message=FALSE, tidy=FALSE------------------------------------------
library(broom)

# get coefs with tidy
my.coef.tab <- my.model.tab %>% 
  tidy(my.model)

# print result
print(head(my.coef.tab))

#' 
#' Notice how function `tidy` included the estimat
#' 
## ---- tidy=FALSE---------------------------------------------------------
# get info on models
my.info.models <- my.model.tab %>% 
  glance(my.model)

print(head(my.info.models))

#' 
#' It includes information about coefficients and 
#' 
#' 
#' ## Reporting Models with `texreg` {#reporting-m
#' 
#' After creating many models, the next step is to
#' 
#' As an example, let's use package `texreg` to re
#' 
#' 
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------
library(texreg)
library(dplyr)

set.seed(20)

# get tickers
my.tickers <- sample(unique(my.df$ticker), 4)
df.stocks <- my.df[my.df$ticker %in% my.tickers, ]

# estimate betas
beta.tab <- df.stocks %>%
  group_by(ticker) %>%
  do(beta.model = lm(data=., ret ~ ret.sp500))

# report result
est.table <- screenreg(l = beta.tab$beta.model, 
                       custom.model.names = beta.tab$ticker, 
                       custom.coef.names = c('Alpha', 'Beta'),
                       digits = 2)

# print it
print(est.table)

#' 
#' In the previous code, we use a list of models f