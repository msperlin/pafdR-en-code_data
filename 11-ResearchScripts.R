#' # Writing research scripts
#' 
#' 
#' In previous chapters, we learned how to create 
#' 
#' ## The structure of a research script
#' 
#' Doing research with R will involve similar set 
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
#' If you are working with multiple files, one sug
#' 
#' 
#' ## The directory structure
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
#' 			Table_with_publishable_results.xlsx
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
#' The benefits of this type of work directory for
#' 
#' An example of the contents of file `0-run-it-al
#' 
## ----eval=FALSE, tidy=FALSE----------------------------------------------
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
## my.R.files <- list.files(path='',
## 						 pattern = '*.R',
## 						 full.names=TRUE)
## 						
## # source all main scripts
## sapply(my.R.files,source)

#' 
#' Notice that to run the above code on another co
#' 
#' 
#' ## Examples of research scripts
#' 
#' We continue this chapter with the presentation 
#' 
#' 
#' Again, we emphasize that the _scripts_ of each 
#' 
#' 
#' ### Analyzing the performance of international 
#' 
#' One of the most popular topics in Finance is th
#' 
#' The main interest in research proposal here is 
#' 
#' 
#' investments that are part of two types of marke
#' 
#' * IBOV - broad index of the Brazilian stock mar
#' 
#' * PETR4 - Preferred shares of Petrobras company
#' 
#' * NTN-B Principal 150824 - Title of federal deb
#' 
#' * CDI - interest rate in the banking market, ge
#' 
#' From these data, the performance analysis will 
#' 
#' We can arrange the stages of research as:
#' 
#' 1. Download the price data or daily of each ins
#' 2. Add these data in one _dataframe_.
#' 3. Process _dataframe_ with the prices in order
#' 
#' This research _script_ will need to install the
#' 
## ---- eval=FALSE, tidy=FALSE---------------------------------------------
## install.packages(c('BatchGetSymbols',
## 				   'GetTDData',
## 				   'dplyr',
## 				   'Quandl',
## 				   'ggplot2'))

#' 
#' Remember also that access to `Quandl` packet da
#' 
#' 
#' #### Importing data from the Brazilian stock ma
#' 
#' The Bovespa data can be directly downloaded fro
#' 
## ---- message=FALSE, tidy=FALSE------------------------------------------
library(BatchGetSymbols)

my.f <- 'data/InternationalStockIndices.csv'
df.indices <- read.csv(my.f)

my.tickers <- df.indices$ticker

first.date = as.Date('2008-01-01')
last.date = as.Date('2016-12-31')

my.l <- BatchGetSymbols(tickers = my.tickers,
                        first.date = first.date,
                        last.date = last.date)


#' 
#' After importing, you should check that everythi
#' 
## ------------------------------------------------------------------------
print(my.l$df.control)

#' 
#' As can be seen, the data was downloaded correct
#' 
## ------------------------------------------------------------------------
print(head(my.l$df.tickers))

#' 
#' Note that the _dataframe_ has several informati
#' 
## ------------------------------------------------------------------------
df.stocks <- my.l$df.tickers[, c('price.adjusted','ref.date','ticker')]
colnames(df.stocks) <- c('price','ref.date','ticker')

#' 
#' After this last step, we have in hand a _datafr
#' 
## ------------------------------------------------------------------------
print(str(df.stocks))

#' 
#' An important observation in this stage is that 
#' 
#' 
#' #### CDI Importing data
#' 
#' The last step in the import data is to seek inf
#' 
#' 
## ---- eval=FALSE---------------------------------------------------------
## library (Quandl)
## 
## my.key <- 'YOUR_API_HERE'

#' 
#' Note that you should use your own key, replacin
#' 
## ---- tidy=FALSE---------------------------------------------------------
Quandl.api_key(my.key)

quandl.codes <- c('BCB/4389')

df.CDI <- Quandl(quandl.codes,
                 type='raw', 
                 start_date = first.date,
				 end_date = last.date)


#' 
#' Again, we see the imported information:
#' 
## ------------------------------------------------------------------------
print(str(df.CDI))

#' 
#' 
#' #### Aggregating data
#' 
#' Since, for each type of data, the columns were 
#' 
## ------------------------------------------------------------------------
my.df <- rbind(df.stocks)

#' 
#' Checks the result with the `str`:
#' 
## ------------------------------------------------------------------------
print(str(my.df))

#' 
#' That last part ends the cleaning step and struc
#' 
#' 
#' #### Calculating annual returns
#' 
#' The next step in the research is to calculate t
#' 
#' It is observed that the imported data are daily
#' 
#' The first step is to create a new column in `my
#' 
## ------------------------------------------------------------------------
my.df$year <- format(my.df$ref.date,'%Y')

print(head(my.df$year))

#' 
#' This new column is then used to calculate the a
#' 
## ----tidy=FALSE----------------------------------------------------------
library(dplyr)

my.ret <- my.df %>% 
  group_by(ticker, year) %>%
  summarise(ret=price[length(price)]/price[1] -1) 

print(head(my.ret))

#' 
#' Note that the resulting _dataframe_ is exactly 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(ggplot2)
p <- ggplot(my.ret, aes(x=as.numeric(year), 
                        y=ret, 
                        color=ticker))
p <- p + geom_line(size=1.5)
p <- p + labs(x='Ano', y = 'Retorno Anual')
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.title=element_blank())
p <- p + scale_colour_grey( start = 0.2, end = 0.8)
print(p)

#' 
#' As can be seen in the graph, investment in the 
#' 
#' To check the total return that an investor woul
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.l <- tapply(X = my.ret$ret, 
               INDEX = my.ret$ticker,  
               FUN = function(x) cumprod(c(x+1)))

my.l <- my.l[unique(my.ret$ticker)] 
my.ret$acum.ret <- unlist(my.l)

#' 
#' The preceding code calculates the cumulative re
#' 
#' The result of this calculation is presented bel
#' 
## ---- tidy=FALSE---------------------------------------------------------
p <- ggplot(my.ret, aes(x=as.numeric(year), 
                        y=acum.ret, 
                        color=ticker))
p <- p + geom_line(size=2)
p <- p + labs(x='Ano', y = 'Retorno Anual Acumulado')
p <- p + theme(legend.position="bottom",legend.title=element_blank())
p <- p + scale_colour_grey( start = 0.2, end = 0.8)
print(p)

#' 
#' As can be seen in the previous figure, investme
#' 
#' Seeking a more complete analysis of performance
#' 
#' The following code will accomplish three things
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.tab <- my.ret %>% group_by(ticker) %>%
  summarise(mean.ret = mean(ret), 
            sd.ret = sd(ret),
            sharpe = mean.ret/sd.ret)

print(my.tab)

#' 
#' This result is easier to analyze the so-called 
#' 
## ---- tidy=FALSE---------------------------------------------------------
p <- ggplot(my.tab, aes(x=sd.ret, y=mean.ret))
p <- p + geom_point(size=3)
p <- p + annotate('text', x = my.tab$sd.ret-0.007, 
                  y = my.tab$mean.ret+0.01, 
                  label = my.tab$ticker)
p <- p + labs(x='Risk', y='Expected Return')
print(p)

#' 
#' The graphs and tables above confirms the result
#' 
#' In relation to the built _script_, it is emphas
#' 
#' 
#' ### What is the best model ARIMA for financial 
#' 
#' It is quite popular that it study the possibili
#' 
#' The second sample search _script_ will focus on
#' 
#' This research question is quite general and cou
#' 
## ---- eval=FALSE---------------------------------------------------------
## install.packages(c('BatchGetSymbols', 'forecast', 'dplyr'))

#' 
#' 
#' #### Importando os dados
#' 
#' O primeiro passo ? buscar as aces que comp?em o
#' 
## ---- eval=FALSE, tidy=FALSE---------------------------------------------
## library(BatchGetSymbols)
## 
## last.date <- Sys.Date()
## first.date <- last.date - 5*365
## 
## df.sp500 <- GetSP500Stocks()
## tickers <- df.sp500$tickers
## 
## l.out <- BatchGetSymbols(tickers = tickers,
## 						 first.date = first.date,
## 						 last.date = last.date)
## 
## df.sp500 <- l.out$df.tickers

#' 
#' 
#' 
#' I emphasize that the previous code data import 
#' 
#' After importing the data, we see the columns of
#' 
## ------------------------------------------------------------------------
print(head(df.sp500))

#' 
#' Note that, as in the previous example, several 
#' 
## ------------------------------------------------------------------------
df.sp500 <- df.sp500[, c('ticker','price.adjusted','ref.date')]

#' 
#' The next step is to better understand which pro
#' 
#' In research presented here, we are interested i
#' 
## ------------------------------------------------------------------------
my.fct <- function(p){
  require(forecast)

  # calculate returns from prices
  ret <- p[2:length(p)]/p[1:(length(p)-1)] -1

  # estimate model
  model.out <- auto.arima(ret, max.p = 3, max.q = 3, max.d = 2,ic = 'aic')

  # get order of model
  order <- arimaorder(model.out)

  # build output
  my.str <- paste0('AR=',order[1],' D=',order[2],' MA=',order[[3]])

  return(my.str)
}

#' 
#' In the previous function, the command is used `
#' 
#' In summary, the above feature operates as follo
#' 
## ---- cache=TRUE, tidy=FALSE---------------------------------------------
library(dplyr)

my.tab <- df.sp500 %>%
  group_by(ticker) %>%
  summarise(arima.out = my.fct(price.adjusted))

print(head(my.tab))

#' 
#' This result is a table showing, for each asset,
#' 
## ------------------------------------------------------------------------
my.result <- my.tab %>%
  group_by(arima.out) %>%
  summarise(my.count = n(),
            percent = n()/nrow(my.tab)) %>%
  arrange(-my.count)

print(my.result)

#' 
#' It appears in the table above, approximately 50
#' 
#' A note is important here. In this research, we 
#' 
#' 
#' ### The intraday liquidity of the Brazilian sto
#' 
#' The third and final example of research will ac
#'  
#' The data used in this study correspond to finan
#' 
#' The first step is to select the assets to perfo
#' 
#' 
## ----eval=FALSE, tidy=FALSE----------------------------------------------
## library(GetHFData)
## 
## n.assets <- 6
## my.date <- as.Date('2016-09-30')
## type.mkt <- 'equity'
## 
## df.tickers <- ghfd_get_available_tickers_from_ftp(my.date = my.date,
##                                                   type.market = type.mkt)

#' 
#' 
#' For the previous code, the `ghfd_get_available_
#' 
## ------------------------------------------------------------------------
print(head(df.tickers))

#' 
#' It is observed that the _dataframe_ `df.tickers
#' 
## ------------------------------------------------------------------------
my.assets <- df.tickers$tickers[1:n.assets]
print(my.assets)


#' 
#' We continue the empirical example using `GetHFD
#' 
#' For periods of time, we used the first time as 
#' 
#' 
## ---- eval=FALSE, tidy=FALSE---------------------------------------------
## my.folder<-'PATH TO YOUR FOLDER HERE'
## setwd(my.folder)
## 
## first.time <- '10:30:00'
## last.time <- '16:30:00'
## 
## first.date <- as.Date('2016-09-12')
## last.date <- as.Date('2016-09-30')
## type.output <- 'agg'
## agg.diff <- '15 min'
## 
## my.assets <- c("ITSA4", "PETR4", "ITUB4", "BBDC4", "ABEV3", "BBSE3")
## type.mkt <- 'equity'
## 
## df.out <- ghfd_get_HF_data(my.assets = my.assets,
##                            type.market = type.mkt,
##                            first.date = first.date,
##                            last.date = last.date,
##                            first.time = first.time,
##                            last.time = last.time,
##                            type.output = type.output,
##                            agg.diff = agg.diff)

#' 
#' It is noteworthy that the above code will take 
#' 
#' 
## ------------------------------------------------------------------------
print(head(df.out))


#' 
#' As described in the operating manual of `ghfd_g
#' 
#' Once the data is available in the R environment
#' 
#' 
## ------------------------------------------------------------------------
p <- ggplot(df.out, aes(x =  Tradetime, y = n.trades))
p <- p + geom_boxplot() + coord_cartesian(ylim = c(0, 3000))
p <- p  + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p <- p + facet_wrap(~InstrumentSymbol)
p <- p + labs(y='Number of Trades', x = 'Time of Day')
print(p)

#' 
#' The figure above shows the number of transactio
#' 
#' The meeting _U_ format for intraday liquidity i