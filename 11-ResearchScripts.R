#' # Writing Research Scripts {#research-scripts}
#' 
#' 
#' In previous chapters, we learned how to use R f
#' 
#' 
#' ## Structure of a Research Script
#' 
#' Doing research with R will generally involve a 
#' 
#' 1. **Importation of data**: At this stage, the 
#' 
#' 2. **Cleaning and structuring the data**: The d
#' 
#' 3. **Modelling and hypothesis testing**: After 
#' 
#' 4. **Reporting the results**: The final stage o
#' 
#' Each of the mentioned steps can be structured i
#' 
#' A practical example would be the analysis of a 
#' 
#' If you are working with multiple files, one sug
#' 
#' 
#' ## Folder Structure
#' 
#' A proper, thought out, folder structure also be
#' 
#' A suggestion for an effective folder structure 
#' 
#' 	/My Research about capital markets/
#' 		/data/
#' 			datafile1.csv
#' 			datafile2.csv
#' 			datafile2.csv
#' 		/fig/
#' 			MyImpressiveFigure.png
#' 		/table/
#' 			Table_with_publishable_results.tex
#' 			DescriptiveTable.tex
#' 		/R-Fcts/
#' 			estimate_model.R
#' 			get_results.R
#' 			read_my_files.R
#' 		0-run-it-all.R
#' 		1-import-and-clean-data.R
#' 		2-run-research.R
#' 
#' The research code should also be self-contained
#' 
#' The benefits of this directory format are clear
#' 
#' An example of the contents of file `0-run-it-al
#' 
## ----eval=FALSE, tidy=FALSE----------------------------------------------
## # Example script for executing a research
## #
## # Output:
## #   Figure in folder fig, tables in folder table
## #
## # Author: Mr R Enthusiast
## # Date: 01/01/2017
## 
## 
## #install.packages(c('BatchGetSymbols', 'plm'))
## 
## # clean up workspace
## rm(list=ls())
## 
## # close all figure windows created with x11()
## graphics.off()
## 
## # change directory
## my.d <- 'My Dir here!'
## setwd(my.d)
## 
## # load all functions
## my.R.files <- list.files(path='R-Fcts',
## 						 pattern = '*.R',
## 						 full.names=TRUE)
## 						
## # source all function file
## sapply(my.R.files,source)
## 
## # run all steps of research
## source('1-import-and-clean-data.R')
## source('2-run-research.R')

#' 
#' Notice that to run the above code on another co
#' 
#' 
#' ## Examples of Research Scripts {#ExampleResear
#' 
#' Here we will present three elaborate examples o
#' 
#' The first example of research is the analysis o
#' 
#' 
## ----child = 'Scripts/ResearchScript-InvestPerformance.Rmd'--------------

#' ### The performance of international investment
#' 
#' 
#' One of the most popular subjects in Finance is 
#' 
#' In this example of research, we will analyze th
#' 
#' 
#' #### The Data
#' 
#' The first step of the study is to identify the 
#' 
#' - The index must be composed of stocks. No vola
#' - Data about about the local exchange rate to d
#' - There must be at least 10 year of data about 
#' 
#' 
#' After verifying these conditions for all assets
#' 
## ------------------------------------------------------------------------
# load indices data
# last update: 2017-04-04
# data manually built with:
#   Yahoo Finance: https://finance.yahoo.com/world-indices
#   Quandl: https://www.quandl.com/

my.f <- 'data/MktIndices_and_Symbols.csv'
df.indices <- read.csv(file = my.f, colClasses = 'character')

# print df
print(df.indices)

#' 
#' The data is composed of European, North America
#' 
#' The first part of our research script is to cle
#' 
## ---- eval=FALSE---------------------------------------------------------
## # clean workspace
## rm(list=ls())
## 
## # change dir
## my.d <- dirname(rstudioapi::getActiveDocumentContext()$path)
## setwd(my.d)
## 
## # close all graphics
## graphics.off()

#' 
#' Let's also set the options of the research scri
#' 
## ------------------------------------------------------------------------
# set dates
first.date <- as.Date('2000-01-01')
last.date <- as.Date('2016-12-31')

# set api key (will not work for you, do change it!)
my.api.key <- 'Esv1Ac7zuZzJSCGxynyV' 
my.f <- 'data/MktIndices_and_Symbols.csv'


#' 
#' Given the structure of the raw data about inter
#' 
#' The easiest way to organize our code is to firs
#' 
#' A particularity of the exchange rate data from 
#' 
#' Another control that must be made in the code i
#' 
#' 
## ------------------------------------------------------------------------
get_and_clean_data <- function(ticker, 
                               quandl.symbol, 
                               first.date,
                               last.date) {
  # gets price data from yahoo and exchange 
  # data from Quandl and adjusts it to dollar. 
  # 
  # Args:
  #   ticker - a ticker symbol from Yahoo finance
  #   quandl.symbol - the symbol of the exchange 
  #                   rate in quandl
  #   first.date - first date of data
  #   last.date - last date of data
  #
  # Returns:
  #   A dataframe with dollar adjusted values of asset price
  
  require(Quandl)
  require(BatchGetSymbols)
  require(stringr)
  
  
  cat(paste0('\nGetting data for ',ticker))
  cat(paste0('\n\tDownloading price data from Yahoo Finance'))
  
  # get data from yahoo finance
  df.ticker <- BatchGetSymbols(tickers = ticker,
                               first.date = first.date,
                               last.date = last.date)$df.tickers
  
  # remove uninteresting info and rename cols
  cols.to.keep <- c("price.adjusted",
                    "ref.date",
                    "ticker" )
  
  df.ticker <- df.ticker[, cols.to.keep]
  colnames(df.ticker) <- c("price",
                           "ref.date",
                           "ticker" )
  
  # get data from quandl
  cat(paste0('\n\tDownloading FX data from Quandl'))
  
  # case of SP500 (no need for exchange data)
  
  if (ticker == '^GSPC') {
    df.ticker$forex <- 1
    df.ticker$price.USD <- df.ticker$price/df.ticker$forex 
    df.ticker$quandl.symbol <- quandl.symbol
    return(df.ticker)
  }
  
  # register api key
  Quandl.api_key(my.api.key)
  
  df.currency <- Quandl(code = quandl.symbol,
                        type = 'raw', 
                        start_date = first.date,
                        end_date = last.date)
  
  # fix names
  df.currency$quandl.symbol <- quandl.symbol
  colnames(df.currency) <- c('ref.date','forex','quandl.symbol')
  
  # merge datasets
  df.ticker <- merge(x = df.ticker, 
                     y = df.currency, 
                     by = 'ref.date')
  
  # calculated USD value of index
  if (str_detect(quandl.symbol,'US')) {
    df.ticker$price.USD <- df.ticker$price*df.ticker$forex 
  } else {
    df.ticker$price.USD <- df.ticker$price/df.ticker$forex
  }
  
  return(df.ticker)
  
}

#' 
#' Now that we have our function to download data 
#' 
#' 
## ---- eval=FALSE, results='hide', message=FALSE--------------------------
## n.rows <- nrow(df.indices)
## 
## my.df <- data.frame()
## for (i in seq(1,n.rows)) {
##   ticker.now <- df.indices$ticker[i]
##   quandl.code.now <- df.indices$quandl.symbol[i]
## 
##   df.temp <- get_and_clean_data(ticker = ticker.now,
##                                 quandl.symbol = quandl.code.now,
##                                 first.date=first.date,
##                                 last.date=last.date)
## 
##   my.df <- rbind(my.df, df.temp)
## }
## 

#' 
#' 
#' For every iteration of the loop, we incremented
#' 
#' 
#' 
#' As expected, we have a new column called `price
#' 
#' 
#' #### Calculating annual returns
#' 
#' The next step in the research is to calculate t
#' 
#' The imported data available in `my.df` is in th
#' 
## ----tidy=FALSE----------------------------------------------------------
library(dplyr)

my.ret <- my.df %>% 
  mutate(year = format(ref.date,'%Y')) %>%
  group_by(ticker, year) %>%
  summarise(last.price = price.USD[length(price.USD)]) %>%
  mutate(ret = c(0, last.price[2:length(last.price)]/
                   last.price[1:(length(last.price)-1)]-1))

#' 
#' A small difference here from the previous code 
#' 
#' Let's have a look in the yearly returns of the 
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
library(ggplot2)
p <- ggplot(my.ret, aes(x=as.numeric(year), 
                        y=ret, 
                        color=ticker))
p <- p + geom_line(size=1.5)
p <- p + labs(x='Year', y = 'Annual Return')
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.title=element_blank())
print(p)

#' 
#' The yearly returns of the different investments
#' 
## ------------------------------------------------------------------------
library(tidyr)

# turn long df to wide and remove year col
ret.wide <- my.ret %>%
  select(year, ticker,ret) %>%
  spread(key = ticker, value = ret, drop = TRUE) %>%
  select(-year)

# turn to matrix
ret.mat <- as.matrix(ret.wide)

# print summary of cor matrix
summary(cor(ret.mat))

#' 
#' As expected, the matrix of returns has a strong
#' 
#' Another way of analyzing the appreciation of va
#' 
## ---- tidy=FALSE---------------------------------------------------------
# calculate accumulated returns
my.l <- tapply(X = my.ret$ret, 
               INDEX = my.ret$ticker,  
               FUN = function(x) cumprod(c(x+1)))

# sorts my.l by ticker and add new column in my.ret
my.l <- my.l[unique(my.ret$ticker)] 
my.ret$acum.ret <- unlist(my.l)

#' 
#' Previous code calculates the cumulative return 
#' 
#' Now, let's plot the result.
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
p <- ggplot(my.ret, aes(x = as.numeric(year), 
                        y = acum.ret, 
                        color = ticker))
p <- p + geom_line(size=1)
p <- p + labs(x = 'Year', 
              y = 'Accumulated Return')
p <- p + theme(legend.position = "bottom",
               legend.title = element_blank())
print(p)

#' 
#' 
#' 
#' It is interesting to see how an investment in `
#' 
#' There are many ways we can quantify risk. In a 
#' 
#' The following code will use object `my.ret` to 
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.tab <- my.ret %>% 
  group_by(ticker) %>%
  summarise(mean.ret = mean(ret), 
            sd.ret = sd(ret),
            sharpe = mean.ret/sd.ret) %>%
  arrange(-sharpe)

print(my.tab)

#' 
#' Object `my.tab` shows the descending values of 
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
p <- ggplot(my.tab, aes(x=sd.ret, y=mean.ret))
p <- p + geom_point(size=3)
p <- p + annotate('text', x = my.tab$sd.ret-0.007, 
                  y = my.tab$mean.ret+0.01, 
                  label = my.tab$ticker)
p <- p + labs(x='Risk', y='Expected Return')
print(p)

#' 
#' 
#' The graph confirms the results about the high p
#' 
#' Now, let's build a portfolio using the previous
#' 
## ------------------------------------------------------------------------
library(matrixcalc)

# check if ret matrix is positive definite
print(is.positive.definite(cov(ret.mat)))

# check for na
print(any(is.na(ret.mat)))

#' 
#' As we can see, the return matrix is positive de
#' 
## ------------------------------------------------------------------------
library(fPortfolio)

# convert to timeSeries
ret.mat <- as.timeSeries(ret.mat)

# get port composition for max sharpe ratio
eff.port <- tangencyPortfolio(ret.mat)

# print result
print(eff.port)

#' 
#' Not surprisingly, the optimized portfolio inves
#' 
## ------------------------------------------------------------------------
# get weights of efficient port
my.w <- eff.port@portfolio@portfolio$weights

# change to vector 
my.w <- matrix(my.w, nrow = length(my.w))

# calculate vector with portfolio returns
my.port <- as.matrix(ret.mat)%*%my.w

# get sharpe ratio
my.sharpe <- mean(my.port)/sd(my.port)

# print it
print(my.sharpe)

#' 
#' The value of the resulting sharpe ratio is slig
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
# get eff frontier
my.pf <- portfolioFrontier(data = ret.mat)

# plot it
tailoredFrontierPlot(object = my.pf, 
                     risk = 'Sigma', 
                     twoAssets = FALSE, 
                     sharpeRatio = FALSE)

#' 
#' The figure shows the investment opportunities o
#' 
#' The efficient frontier clearly shows how an inv
#' 
#' In this study we analyzed the historical perfor
#' 
#' The written code in this research is reproducib

#' 
## ----child = 'Scripts/ResearchScript-prophet.Rmd'------------------------

#' ### Can we predict stock's returns with Prophet
#' 
#' 
#' Facebook recently released an API package allow
#' 
#' > "It's not your traditional ARIMA-style time s
#' >
#' > --- Facebook Core Data Science team, 2017
#' 
#' Given its open source format, the prophet algor
#' 
#' Before describing the code and results, it is n
#' 
#' The role of practitioners is also important to 
#' 
#' 
#' #### The Data
#' 
#' 
#' For this research exercise, we will use the dat
#' 
#' Before applying the model to the data, we need 
#' 
#' Let's try a simple example of estimating and fo
#' 
## ------------------------------------------------------------------------
library(prophet)

# get ret data from df
my.stock <- unique(my.df$ticker)[1]
temp.df <- my.df[my.df$ticker %in% my.stock , ]

# set df for estimation
df.est <- data.frame(y = temp.df$ret, 
                     ds = temp.df$ref.date)

# estimate and print model					 
my.prophet <- prophet(df = df.est)

# create forecasts 
df.pred <- predict(my.prophet,
                   make_future_dataframe(my.prophet,
                                         periods = 10,
                                         include_history = FALSE))

# print result										   
print(head(df.pred))

#' 
#' The usage is straightforward, we input a `dataf
#' 
#' Now that we understand how `prophet` works, the
#' 
## ---- message=FALSE------------------------------------------------------
est.model.and.forecast <- function(df.in){
  # Estimates a model using prophet and forecast it
  #
  # Args:
  #   df.in - A dataframe with columns ret and ref.date
  #
  # Returns:
  #   A dataframe with forecasts and errors 
  
  require(prophet)
  require(dplyr)
  
  my.ticker <- as.character(unique(df.in$ticker[1]))
  
  cat('\nProcessing ', my.ticker)
  
  # get total number of rows in df.in
  n.row <- nrow(df.in)
  
  # remove uninteresting columns
  df.in <- select(df.in, ref.date, ret)
  names(df.in) <- c('ds', 'y')
  
  # get half the sample for estimation
  idx <- floor(nrow(df.in)/2)
  
  df.est <- df.in[1:idx, ]
  df.for <- df.in[(idx + 1):nrow(df.in), ]
  
  # estimate a (silent) prophet model
  capture.output(
    m <- prophet(df = df.est)
  )
  
  # calculate number of forecasts needed to match data
  n.forecasts <- length(seq(from = min(df.for$ds),
                            to = max(df.for$ds), 
                            by = '1 day'))
  
  # make predictions
  df.pred <- predict(m,
                     make_future_dataframe(m,
                                           periods = n.forecasts,
                                           include_history = FALSE))
  
  # merge y and yhat
  df.for <- merge(df.for, df.pred, by = 'ds')
  df.for <- select(df.for, ds, y, yhat)
  
  # set ticker
  df.for$ticker <- my.ticker
  
  return(df.for)
}

#' 
#' 
#' With the previous function ready, we can use `b
#' 
## ---- message=FALSE,eval=FALSE-------------------------------------------
## out.l <- by(data = my.df,
##             INDICES = my.df$ticker,
##             FUN = est.model.and.forecast)
## 
## # merge results
## my.result <- do.call(rbind, out.l)

#' 
#' 
#' After estimating the models and creating the fo
#' 
## ------------------------------------------------------------------------
print(str(my.result))

#' 
#' In this object we find the forecasts (`yhat`), 
#' 
#' 
#' ####  The Encopassing Test
#' 
#' A simple and powerful test for verifying the ac
#' 
#' 
#' 
#' 
#' If the predictive model provides good forecasts
#' 
#' First, let's find the result of the encompassin
#' 
## ------------------------------------------------------------------------
# do encompassing test for all data
lm.model <- lm(formula = y ~ yhat, 
               data = my.result)

# print result
summary(lm.model)

#' 
#' The result is far from good! the value of the i
#' 
## ---- message=FALSE------------------------------------------------------
library(car)

# set test matrix
my.rhs <- matrix(c(0,   # alpha test value
                   1))  # beta test value

# hypothesis matrix 
hyp.mat <- matrix(c(1,0,
                    0,1),nrow = 2)

# do wald test
my.waldtest <- linearHypothesis(lm.model, 
                                hypothesis.matrix = hyp.mat, 
                                rhs = my.rhs)

# print result
print(my.waldtest)

#' 
#' The results show that the joint hypothesis of a
#' 
#' 
## ------------------------------------------------------------------------
library(dplyr)

do.wald.test <- function(lm.model) {
  # Tests the joint hypothesis that alpha equals 0 and beta equals 1
  #
  # Args:
  #   lm.model - a model estimated with lm()
  #
  # Returns:
  #   The pvalue of the test
  
  require(car)
  wald.test<-linearHypothesis(lm.model, 
                              hypothesis.matrix = matrix(c(1,0,0,1),
                                                         nrow = 2), 
                              rhs = matrix(c(0,1)))
  
  p.value <- wald.test$`Pr(>F)`[2]
  
  return(p.value)
  
}

# do wald test for each stock
my.tab <- my.result %>%
  group_by(ticker) %>%
  do(model = lm(formula = y ~yhat, data = .)) %>%
  mutate(p.value = do.wald.test(model)) 

#' 
#' Now that we have the p-value of the Wald test f
#' 
## ------------------------------------------------------------------------
n.fail.to.reject <- sum(my.tab$p.value>0.1)
print(n.fail.to.reject)

#' 
#' So, in only `r n.fail.to.reject` stocks we have
#' 
#' 
#' #### Directional Forecasts and a Timing Strateg
#' 
#' When looking at performance in a trading applic
#' 
#' In order to test if `prophet` is able to provid
#' 
#' - buy in end of day _t_ if return forecast in _
#' - short-sell in the end of day _t_ when return 
#' 
#' While the values of the proportion of correct s
#' 
#' The following R code will execute these calcula
#' 
## ------------------------------------------------------------------------
library(dplyr)

# check directional performance and trading strategy
my.tab <- my.result %>%
  group_by(ticker) %>%
  summarise(n.correct.dir = sum(sign(y)==sign(yhat))/n(),
            ret.strat = prod(1+sign(yhat)*y)-1,
            ret.naive = prod(1 + y)-1,
            ret.excess = ret.strat - ret.naive)

#' 
#' We can now analyze the results for all stocks u
#' 
## ---- fig.show = 'hold',fig.height=my.fig.height, fig.width=my.fig.width----
library(ggplot2)

p <- ggplot(my.tab, aes(x=n.correct.dir))
p <- p + geom_histogram()
p <- p + geom_vline(aes(xintercept =  0.5),size=1)
print(p)

p <- ggplot(my.tab, aes(x=ret.excess))
p <- p + geom_histogram()
p <- p + geom_vline(aes(xintercept =  0.0),size=1)
print(p)

#' 
#' 
#' 
#' The visual results shows that the forecasting m
#' 
#' Let's confirm this result using a formal test. 
#' 
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
my.DAC.fct <- function(yhat, y, type.test) {
  # Tests for directional accuracy (PT) and excess profitability (AG)
  # Null hypothesis: PT - No directional accuracy
  #                  AG - No excess profitability
  # Args:
  #   yhat - vector of forecasted values
  #   y - vector of real values
  #   type.test - The option for test (PT or AG)
  #
  # Returns:
  #   The p-value from the test
  
  require(rugarch)
  
  test.out <- DACTest(forecast = yhat, 
                      actual = y, 
                      test = type.test)
  
  return(test.out$p.value)
  
}

# use test for each stock
my.tab <- my.result %>%
  group_by(ticker) %>%
  summarise(p.value.PT = my.DAC.fct(yhat, y, 'PT'),
            p.value.AG = my.DAC.fct(yhat, y, 'AG'))

# plot histogram (PT)
p <- ggplot(my.tab, aes(x=p.value.PT)) + 
    geom_histogram() + 
	geom_vline(aes(xintercept =  0.1),size=1)

print(p)	

# plot histogram (AG)
p <- ggplot(my.tab, aes(x=p.value.AG)) + 
    geom_histogram() + 
	geom_vline(aes(xintercept =  0.1),size=1)

print(p)	

#' 
#' 
#' 
#' The results in `my.tab` show that only in `r n.
#' 
#' The main results of the study are clear: `proph
#' 
#' In line with the previous example of research s

#' 
## ----child = 'Scripts/ResearchScript-GetHFData.Rmd'----------------------

#' ### An Analysis of High Frequency trade Data {#
#' 
#' 
#' 
#' In this final example of research script, we wi
#' 
#' 
#' #### Liquidity and the Time of the Day
#' 
#' In order to illustrate the usage of aggregated 
#' 
#' The data used in this empirical study is relate
#' 
#' The first step is to select the liquid assets t
#' 
## ----eval=get.new.data---------------------------------------------------
library(GetHFData)

# set type of market (equity, options,BMF)
type.market <- 'equity'

# get available files from ftp
df.ftp <- ghfd_get_ftp_contents(type.market = type.market)

# get last available date
last.date <- max(df.ftp$dates)

# get 6 most traded
df.tickers <- ghfd_get_available_tickers_from_ftp(my.date = last.date, 
                                                  type.market)


#' 
#' 
#' 
#' The last available date is `r last.date`. Funct
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
library(ggplot2)

# select tickers
temp.df <- df.tickers[1:25, ]

p <- ggplot(temp.df, aes(x = reorder(tickers, -n.trades), y = n.trades))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p <- p + labs(x = 'Tickers', y = 'Number of trades')
print(p)

#' 
#' We can see that the six most traded assets in `
#' 
#' From the programming side, object `df.tickers` 
#' 
## ------------------------------------------------------------------------
n.assets <- 6
my.assets <- df.tickers$tickers[1:n.assets]

print(my.assets)

#' 
#' We continue the empirical example using package
#' 
## ------------------------------------------------------------------------
# intraday time thresholds
first.time <- '10:30:00'
last.time <- '16:30:00'

# type of market
type.market <- 'equity'

# dates of study
last.date <- max(df.ftp$dates)
first.date <- last.date-30

# type of output and aggregation
type.output <- 'agg'
agg.diff <- '15 min'

#' 
#' After setting the inputs, we now use function `
#' 
## ---- eval=get.new.data--------------------------------------------------
df.out.agg <- ghfd_get_HF_data(my.assets = my.assets,
                               type.market = type.market,
                               first.date = first.date,
                               last.date = last.date,
                               first.time = first.time,
                               last.time = last.time,
                               type.output = type.output,
                               agg.diff = agg.diff)

#' 
#' 
#' The previous code will take some time to finish
#' 
## ------------------------------------------------------------------------
print(head(df.out.agg))

#' 
#' As described earlier, the object returned from 
#' 
#' Once the data is available, we proceed to the a
#' 
## ----fig.height=my.fig.height, fig.width=my.fig.width--------------------
p <- ggplot(df.out.agg, aes(x =  Tradetime, y = n.trades))
p <- p + geom_boxplot() + coord_cartesian(ylim = c(0, 2500))
p <- p  + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p <- p + facet_wrap(~InstrumentSymbol)
p <- p + labs(y='Number of Trades', x = 'Time of Day')
print(p)

#' 
#' Previous figure shows the number of trades as a
#' 
#' This result is supported by previous findings i
#' 
#' 
#' #### Calculating Realized Volatility from Tick 
#' 
#' One of the main innovations in the research fie
#' 
#' In this section we will present a simple exampl
#' 
#' The first step in this empirical section is to 
#' 
## ---- eval=get.new.data--------------------------------------------------
# set raw df, tick by tick
type.output <- 'raw'

# get data
df.out.raw <- ghfd_get_HF_data(my.assets = my.assets,
                               type.market = type.market,
                               first.date = first.date,
                               last.date = last.date,
                               first.time = first.time,
                               last.time = last.time,
                               type.output = type.output)

#' 
#' 
#' In this example of calculating realized volatil
#' 
#' > "The medRV belongs to the class of realized v
#' >
#' > --- Help file for `highfrequency::medRV`, 201
#' 
#' Further inspection in the usage of `medRV` show
#' 
#' The wrapper function works with the following s
#' 
## ------------------------------------------------------------------------
my.RV.fct <- function(TradePrice, TradeDateTime){
  # Calculates realized volatility from vetor of prices and trade times
  #
  # Args:
  #   TradePrice - a trade price vector
  #   TradeDateTime - a date-time vector with trade times
  #
  # Returns:
  #   A single value of realized volatility
  
  require(highfrequency)
  
  temp.x <- xts(TradePrice, order.by = TradeDateTime)
  RV <- medRV(temp.x, makeReturns = T)
  
  return(as.numeric(RV))
}

#' 
#' Once the function is available, we use it toget
#' 
## ------------------------------------------------------------------------
library(dplyr)

RV.tab <- df.out.raw %>% 
  group_by(InstrumentSymbol, SessionDate) %>%
  summarise(RV = my.RV.fct(TradePrice, TradeDateTime))

#' 
#' The result is a `dataframe` with three columns,
#' 
## ------------------------------------------------------------------------
print(head(RV.tab))

#' 
#' Once the processed data is ready, we illustrate
#' 
## ----RV-plot, fig.height=my.fig.height, fig.width=my.fig.width-----------
p <- ggplot(RV.tab, aes(x=SessionDate, y=RV))
p <- p + geom_line(size=1)
p <- p + facet_wrap(~InstrumentSymbol)
p <- p + labs(x='Date', y='Realized Volatility')
print(p)

#' 
#' As expected, the realized volatility presents t

#' 
#' 
#' 