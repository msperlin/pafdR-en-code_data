#' # Estimation models with R
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
#' ### Simulating and estimating models Garch
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
#' ### Predicting models Garch
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
#' ## Dealing with several models with `dplyr`
#' 
#' WRITE MORE HERE
#' 
## ------------------------------------------------------------------------
library(readr)
library(broom)
library(dplyr)

set.seed(100)

load('data/SP500-Stocks-WithRet.RData')

my.tickers <- sample(unique(my.df$ticker), 5)

my.df <- my.df[my.df$ticker %in% my.tickers, ]

my.f <- 'data/SP500.csv'
df.SP500 <- read.csv(my.f, colClasses = c('Date', 'numeric'))

# use calc.ret function from chapter Programming
df.SP500$retSP500 <- calc.ret(df.SP500$price)

df.SP500 <- df.SP500[ , c('date','retSP500')]

my.df <- merge(x = my.df, y= df.SP500, by.x = 'ref.date', by.y = 'date')

models <- my.df %>%
  group_by(ticker) %>%
  do(my.model = lm(data=., formula = ret ~ retSP500  ))


my.tab <- models %>%
  tidy(my.model)

my.tab

my.tab <- models %>% 
  glance(my.model)


my.tab

library(texreg)

screenreg(l = as.list(models$my.model), 
          custom.model.names = as.character(my.tickers), 
          custom.coef.names = c('Alpha', 'Beta'))


#' 
#' 
#' ## Reporting model results