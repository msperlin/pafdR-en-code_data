#' # Financial Models in R
#' 
#' 
#' Finance has inherited many methodological proce
#' 
#' The variety of models used in finance is huge, 
#' 
#' ## Linear models (OLS)
#' 
#' A linear model with _N_ explanatory variables c
#' 
#' 
#' 
#' WRITE MORE HERE!
#' 
#' ### Simulating and estimating a linear model (O
#' 
#' 
#' In R, the main function for the estimation of l
#' 
#' 
#' 
#' To create `r if (my.engine!='epub3') {'$y_t$'} 
#' 
## ------------------------------------------------------------------------
# number of obs
N <- 1000 

# set x as normal random
x <- rnorm(N)

# set coefficients
my.alpha <- 0.5
my.beta <- 2

# build y
y <- my.alpha + my.beta*x + rnorm(N)

#' 
#' Using `ggplot`, we can create a scatter plot to
#' 
## ---- out.width='60%'----------------------------------------------------
library(ggplot2)

# set temp df
temp.df <- data.frame(x, y)

p <- ggplot(temp.df, aes(x = x, y = y))
p <- p + geom_point(size=0.5)

print(p)

#' 
#' Clearly, there is a positive linear correlation
#' 
#' To estimate the coefficients of the linear mode
#' 
## ------------------------------------------------------------------------
# set df
lm.df <- data.frame(x, y)

# estimate linear model
my.lm <- lm(data = lm.df, formula = y ~ x)
print(my.lm)

#' 
#' The output of function `lm` is an object simila
#' 
## ------------------------------------------------------------------------
# print names in model
print(names(my.lm))

#' 
#' As you can see, there is a slot called `coeffic
#' 
## ------------------------------------------------------------------------
print(my.lm$coefficients)

#' 
#' The estimated coefficients are very close to th
#' 
#' Experienced researchers have probably noted tha
#' 
## ------------------------------------------------------------------------
print(summary(my.lm))

#' 
#' 
#' Now we have a lot more information! It appears 
#' 
#' Several other additional information is availab
#' 
## ------------------------------------------------------------------------
my.summary <- summary(my.lm)
print(names(my.summary))

#' 
#' Each of these elements contain information that
#' 
#' 
#' ## Arima Models
#' 
#' Arima is a special type of model that uses the 
#' 
#' A simple example of an Arima model type is defi
#' 
#' 
#' 
#' In this example, we have a ARIMA (1, 0, 1) mode
#' 
#' 
#' ### Simulating and estimating models of type Ar
#' 
#' First, lets simulate an Arima model using funct
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set number of observations
my.n <- 1000

# set model's parameters
my.model <- list(ar = c(0.5), ma = c(-0.2))
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
#' The graph shows how the time series of `y` has 
#' 
#' To estimate a Arima model, we use function `ari
#' 
## ------------------------------------------------------------------------
# estimate arima model
my.arima <- arima(my.ts, order = c(1,0,1))

# print result
print(my.arima)

#' 
#' As expected, the estimated parameters are very 
#' 
## ------------------------------------------------------------------------
print(summary(my.arima))

#' 
#' Again, we have lots of information. We could fu
#' 
#' The identification of models Arima type, that i
#' 
#' In the next example, we use function `auto.arim
#' 
## ------------------------------------------------------------------------
# read file
my.f <- 'data/SP500.csv'
df.SP500 <- read.csv(my.f)

# set return column
df.SP500$ret <- calc.ret(df.SP500$price)

#' 
#' Now, we estimate the model with function `auto.
#' 
## ------------------------------------------------------------------------
library(forecast)

# estimate arima model with automatic identification
my.autoarima <- auto.arima(x = df.SP500$ret)

# print result
print(my.autoarima)

#' 
#' The result tells us that the best model for the
#' 
#' 
#' ### Forecasting with Arima models
#' 
#' We can obtain the forecasts of an Arima model w
#' 
## ------------------------------------------------------------------------
# forecast model
print(forecast(my.autoarima, h = 5))

#' 
#' ## Garch models
#' 
#' Garch models became one of the specifications i
#' 
#' A GARCH model is modular. In its simplest forma
#' 
#' 
#' 
#' Explaining it further, the `r if (my.engine!='e
#' 
#' 
#' ### Simulating Garch models
#' 
#' R does not have a native function to simulate a
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
#' The previous code defines a Garch model that is
#' 
#' 
#' 
#' 
#' 
#' To simulate _1000_ observations of this model, 
#' 
## ------------------------------------------------------------------------
# simulate garch model
sim.garch = garchSim(spec, n = 1000)

#' 
#' We can visualize the artificial time series gen
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set df for ggplot
temp.df <- data.frame(sim.ret = sim.garch$garch, 
                      idx=seq_along(sim.garch$garch))

library(ggplot2)
p <- ggplot(temp.df, aes(x=idx, y=sim.ret))
p <- p + geom_line()
print(p)

#' 
#' As can be seen, the behaviour of this simulated
#' 
#' 
#' ### Estimating Garch models
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
#' Function `summary` also works for Garch models.
#' 
#' 
#' ### Forecasting Garch models
#' 
#' Forecasting Garch models involves two elements:
#' 
#' In package `fGarch`, both forecasts are calcula
#' 
#' 
## ------------------------------------------------------------------------
predict(my.garchfit, n.ahead = 3)

#' 
#' The first column of the previous result is the 
#' 
#' ## Dealing with several models
#' 
#' In the previous sections we learned to estimate
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
my.df <- my.df[my.df$ticker %in% my.tickers, ]

# renew factors in ticker
my.df$ticker <- as.factor(as.character(my.df$ticker))

#' 
#' Now, what we want to do with this data is to se
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(forecast)

my.l <- tapply(X = my.df$ret, 
               INDEX = my.df$ticker, 
               FUN = arima, 
               order = c(1,0,0))


#' Each of the models is available in `my.l`. In o
#' 
## ------------------------------------------------------------------------
print(sapply(X = my.l, FUN = coef))

#' 
#' One thing to notice here that, by using `tapply
#' 
## ---- tidy=FALSE---------------------------------------------------------
estimate.model <- function(df) {
  require(forecast)
  
  x <- df$ret
  my.model <- arima(x = x, 
                    order = c(2,0,2))
  
  return(my.model)
}


#' 
#' Now we can use the previous function with `by`.
#' 
## ---- tidy=FALSE---------------------------------------------------------
# estimate a ARIMA model to each stock return
my.l <- by(data = my.df, 
           INDICES = my.df$ticker, 
           FUN = estimate.model)

# print coefficients
print(sapply(X = my.l, FUN = coef))


#' 
#' As you can see, the result is simillar to the p
#' 
#' Another way of resolving the problem of several
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(dplyr)

my.tab <- my.df %>%
  group_by(ticker) %>%
  do(my.model = arima(x = .$ret, order = c(1,0,0)))

print(my.tab)

#' As we can see, we have a list-column called `my
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.model.tab <- my.df %>%
  group_by(ticker) %>%
  do(my.model = arima(x = .$ret, order = c(1,0,0))) %>%
  mutate(alpha = coef(my.model)[2],
         ar1 = coef(my.model)[1])

print(my.model.tab)

#' 
#' Another trick in handling models with `dplyr` i
#' 
## ---- message=FALSE, tidy=FALSE------------------------------------------
library(broom)

my.coef.tab <- my.model.tab %>% 
  tidy(my.model)

print(my.coef.tab)

#' 
#' Notice how it also included the estimated error
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.info.models <- my.model.tab %>% 
  glance(my.model)

print(my.info.models)

#' 
#' It includes information about coefficients and 
#' 
#' 
#' ## Reporting models with `texreg`
#' 
#' After creating many models, the next step is to
#' 
#' As an example, let's use package `texreg` for r
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------
library(texreg)

est.table <- screenreg(l = my.model.tab$my.model, 
                       custom.model.names = as.character(my.tickers), 
                       custom.coef.names = c('Alpha', 'Beta'),
                       digits = 3)

print(est.table)

#' 
#' In the previous code, we use a list of models f
#' 
#' Package `texreg` offers many more options to th
#' 
## ---- results='asis', tidy=FALSE, echo=FALSE-----------------------------
est.table <- texreg(l = my.model.tab$my.model, 
                       custom.model.names = as.character(my.tickers), 
                       custom.coef.names = c('Alpha', 'Beta'),
                       digits = 3)

print(est.table)

#' 