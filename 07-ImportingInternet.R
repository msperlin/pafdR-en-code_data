#' # Importing Financial Data from the Internet {#
#' 
#' One of the great advantages of using R for data
#' 
#' ## CRAN Packages
#' 
#' In most cases, the importation of financial dat
#' 
#' 
#' ### Package `quantmod` 
#' 
#' To import daily trade data of stocks, one of th
#' 
#' In the following example, we will get data for 
#' 
## ---- message=FALSE------------------------------------------------------
library(quantmod)

# get data for FTSE
my.df <- getSymbols(Symbols = '^FTSE', auto.assign = FALSE)

# print last rows
print(tail(my.df))

#' 
#' In the call to `getSymbols`, we used argument `
#' 
#' Some attention is required when downloading tra
#' 
#' While downloading data for one asset can be use
#' 
## ------------------------------------------------------------------------
# set tickers
my.tickers <- c('MSFT','GOOGL','JPM','GE')

# create new environment
my.env <- new.env()

# download fin data and save to my.env
getSymbols(Symbols = my.tickers, env = my.env)

# print objects in my.env
print(names(my.env))

# print contents of MSFT
print(tail(my.env$MSFT))

#' 
#' The previous code downloads data for all ticker
#' 
#' 
#' ### Package `BatchGetSymbols`
#' 
#' Another possibility for downloading financial d
#' 
#' Look at the following example, where we downloa
#' 
## ---- message=FALSE------------------------------------------------------
library(BatchGetSymbols)

# set tickers
my.tickers <- c('MSFT','GOOGL','JPM','GE')

# set dates
first.date <- Sys.Date()-30
last.date <- Sys.Date()

l.out <- BatchGetSymbols(tickers = my.tickers,
                         first.date = first.date,
                         last.date = last.date)


#' 
#' The output of `BatchGetSymbols` is a `list`, wh
#' 
## ------------------------------------------------------------------------
# print result of download process
print(l.out$df.control)

#' 
#' Object `df.control` shows all tickers were vali
#' 
#' As for the actual financial data, it is contain
#' 
## ------------------------------------------------------------------------
# print df.tickers
print(tail(l.out$df.tickers))

#' 
#' As expected, the information about prices and v
#' 
#' Another useful function of `BatchGetSymbols` is
#' 
## ---- message=FALSE, eval=FALSE------------------------------------------
## library(BatchGetSymbols)
## 
## # set tickers
## my.tickers <- GetSP500Stocks()$ticker
## 
## # set dates
## first.date <- Sys.Date()-30
## last.date <- Sys.Date()
## 
## l.out <- BatchGetSymbols(tickers = my.tickers,
##                          first.date = first.date,
##                          last.date = last.date)
## 

#' 
#' Be aware running the previous code takes time, 
#' 
#' 
#' ### Package `finreportr`
#' 
#' Package `finreportr` [@finreportr] is designed 
#' 
## ------------------------------------------------------------------------
library(finreportr)

# print available functions in finreportr
ls('package:finreportr')

#' 
#' We have `r length(ls('package:finreportr'))` fu
#' 
## ------------------------------------------------------------------------
my.ticker <- 'FB'

# NOT WORKING
#info <- CompanyInfo(my.ticker)
#print(info)

#' 
## ---- echo=FALSE---------------------------------------------------------
info <- list(company = 'Facebook', street.address = 'Menlo Park',
             city.state = 'California')

#' 
#' 
#' As we can see, the formal name of Facebook is `
#' 
## ------------------------------------------------------------------------
# set final year
my.year <- 2016

# get income for FB
my.income <- GetIncome(my.ticker, my.year)

# print result
print(head(my.income))

#' 
#' Let's see what types of financial information w
#' 
## ---- eval=TRUE----------------------------------------------------------
# get unique fields
unique.fields <- unique(my.income$Metric)

# cut size of string
unique.fields <- substr(unique.fields,1, 60)

# print result
print(unique.fields)

#' 
#' We have not only revenues and earnings per shar
#' 
#' Let's see how each Facebook investor was financ
#' 
## ----tidy=FALSE----------------------------------------------------------
# set col and date
my.col <- 'Earnings Per Share, Basic'

# print earnings per share
print(my.income[my.income$Metric == my.col, ])

#' 
#' From the data, we can see Facebook investors re
#' 
#' An interesting aspect of `finreportr` is it wor
#' 
#' 
#' ### Package `tidyquant`
#' 
## ----echo=FALSE, message=FALSE-------------------------------------------
library(tidyquant)

#' 
#' Package `tidyquant` provides functions related 
#' 
#' The package includes functions for obtaining fi
#' 
#' In its current version, `tidyquant` has `r leng
#' 
## ----tidy=FALSE----------------------------------------------------------
library(tidyquant)

# set stock and dates
my.ticker <- 'AAPL'
first.date <- '2017-01-01'
last.date <-  Sys.Date()

# get data with tq_get
my.df <- tq_get(my.ticker,
                get = "stock.prices", 
				from = first.date, 
				to = last.date)

print(tail(my.df))

#' 
#' As we can see, the price data is the same as us
#' 
## ------------------------------------------------------------------------
# get key financial rations of AAPL
df.key.ratios <- tq_get("AAPL",get = "key.ratios")

# print it
print(df.key.ratios)		   

#' 
#' Object `df.key.ratios` offers fundamental infor
#' 
## ------------------------------------------------------------------------
# get profitability table
df.profitability <- df.key.ratios$data[[2]]

# print it
print(tail(df.profitability))

#' 
#' A novel and noteworthy aspect of `tidyquant` is
#' 
## ------------------------------------------------------------------------
# get stocks in AMEX
print(head(tq_exchange('AMEX')))

#' 
#' We can also get information about components of
#' 
## ------------------------------------------------------------------------
# print available indices
print(tq_index_options())

#' 
#' Let's get information for `"DOWJONES"`.
#' 
## ------------------------------------------------------------------------
# get components of "DOWJONES"
print(tq_index("DOWJONES"))

#' 
#' These functions are useful because they give th
#' 
#' We only looked into a few functions from packag
#' 
#' 
#' ### Package `GetHFData`
#' 
#' Package `GetHFData` [@gethfdata] is designed to
#' 
#' Let's try a simple example by downloading trade
#' 
#' 
## ----message=FALSE-------------------------------------------------------
library(GetHFData)

# set tickers and type of market
my.ticker <- c('PETR4','VALE5')
my.type.market <- 'equity'

# get available dates from ftp
df.available.dates <- ghfd_get_ftp_contents(my.type.market)

# set last date
last.date <- max(df.available.dates$dates)

# get data!
my.df <- ghfd_get_HF_data(my.assets = my.ticker, 
                          type.market = 'equity',
                          first.date = last.date,
                          last.date = last.date,
                          first.time = '10:00:00',
                          last.time = '17:00:00',
                          type.output = 'agg',
                          agg.diff = '5 min')

# print results
print(head(my.df))

#' 
#' The output of `ghfd_get_HF_data` is a `datafram
#' 
#' 
## ---- eval=TRUE, tidy=FALSE, message=FALSE-------------------------------
library(GetHFData)

# set tickers and type of market
my.ticker <- c('PETR4','VALE5')
my.type.market <- 'equity'

# get available dates from ftp
df.available.dates <- ghfd_get_ftp_contents(my.type.market)

# set last date
last.date <- max(df.available.dates$dates)

# get data!
my.df <- ghfd_get_HF_data(my.assets = my.ticker, 
                          type.market = 'equity',
                          first.date = last.date,
                          last.date = last.date,
                          first.time = '10:00:00',
                          last.time = '17:00:00',
                          type.output = 'raw')

# print results
print(head(my.df))

#' 
#' 
#' In the chapter about research scripts, we will 
#' 
#' 
#' ### Package `ustyc`
#' 
#' Package `ustyc` allows the download of yield cu
#' 
#' Using package `ustyc` is very simple. All you n
#' 
## ------------------------------------------------------------------------
library(ustyc)

# get yield curve
my.yield.curve <- getYieldCurve(year = 2016)

#' 
#' The return object is a list, where the yield cu
#' 
## ------------------------------------------------------------------------
# print result
print(head(my.yield.curve$df))

#' 
#' In section \@ref(ggplot), we will learn how to 
#' 
#' 
#' ### Package `Quandl`  {#quandl}
#' 
#' Another major source of financial data is the _
#' 
#' The first step in using `Quandl` is to register
#' 
## ------------------------------------------------------------------------
# set api key to quandl
my.api.key <- 'Asv8Ac7zuZzJSCGxynfG'

#' 
#' This API key is unique to each user, and the on
#' 
## ------------------------------------------------------------------------
library(Quandl)

# search string in quandl
df.search <- Quandl.search('Gold in Euro', silent = TRUE)

#' 
#' In our case, the function returned a `dataframe
#' 
## ------------------------------------------------------------------------
# print columns from 
print(colnames(df.search))

#' 
#' The `description` column gives information abou
#' 
#' With the API key and the Quandl symbol, we use 
#' 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(Quandl)

# register api key
Quandl.api_key(my.api.key)

# set symbol and dates
my.symbol <- 'WGC/GOLD_DAILY_EUR'
first.date <- as.Date('2000-01-01')
last.date <- Sys.Date()

# get data!
my.df <- Quandl(code = my.symbol,
                type='raw', 
                start_date = first.date,
                end_date = last.date)

print(tail(my.df))

#' 
#' Notice how we used `type = 'raw'` in the inputs
#' 
#' 
#' ### Package `Rbitcoin`
#' 
#' Given the popularity of cripto-currencies, anot
#' 
## ---- message=FALSE------------------------------------------------------
library(Rbitcoin)

# set mkt, currency pair and type of action
my.mkt <- "kraken"
my.currency <- c("BTC","EUR")
my.action <- 'trades'

# import data
my.l <- market.api.process(market = my.mkt,
                           currency_pair = my.currency,
                           action = my.action)

# print it
print(my.l)

#' 
#' The output of `market.api.process` is a `list` 
#' 
## ---- message=FALSE------------------------------------------------------
print(tail(my.l$trades))

#' 
#' It includes price and time information for the 
#' 
#' 
#' ### Other Packages
#' 
#' In CRAN, you'll find many more packages for imp
#' 
#' 
#' ## Accessing Data from Web Pages (_webscraping_
#' 
#' The previous packages are useful, as they make 
#' 
#' The process of extracting information from web 
#' 
#' 
#' ### Scraping the Components of the SP500 Index 
#' 
#' In its website, Wikipedia offers a [section](ht
#' 
## ----SP500-wikipedia, echo = FALSE, out.width = '75%', fig.cap = 'Mirror of Wikipedia page on SP500 components'----
knitr::include_graphics('figs/SP500-Wikipedia.png')

#' 
#' The information in this web page is constantly 
#' 
#' The first step in webscraping is finding out wh
#' 
## ----SP500-Wikipedia-webscraping, echo = FALSE, out.width = '75%', fig.cap = 'Finding xpath from website'----
knitr::include_graphics('figs/SP500-Wikipedia_webscraping.png')

#' 
#' In this case, the copied _xpath_ is:
#' 
## ---- eval=FALSE---------------------------------------------------------
## '//*[@id="mw-content-text"]/table[1]/thead/tr/th[2]'

#' 
#' This is the address of the header of the table.
#' 
#' Now that we have the location of what we want, 
#' 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(rvest)

# set url and xpath
my.url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
my.xpath <- '//*[@id="mw-content-text"]/div/table[1]'

# get nodes from html
out.nodes <- html_nodes(read_html(my.url),
                        xpath = my.xpath)

# get table from nodes (each element in 
# list is a table)
df.SP500Stocks <- html_table(out.nodes)

# isolate it and print it
df.SP500Stocks <- df.SP500Stocks[[1]]
print(head(df.SP500Stocks))

#' 
#' Object `df.SP500Stocks` contains a mirror of th
#' 
#' 
#' ### Scraping the Website of the Reserve Bank of
#' 
#' As another example of webscraping with R, let’s
#' 
## ----RBA-website, echo = FALSE, out.width = '75%', fig.cap = 'Website for the Reserve Bank of Australia'----
knitr::include_graphics('figs/website_RBA-webscrapping.png')

#' 
#' The website offers several information such as 
#' 
## ---- eval=FALSE---------------------------------------------------------
## my.xpath.inflation <- '//*[@id="content"]/section[1]/div/div[2]/p'
## my.xpath.int.rate <- '//*[@id="content"]/section[1]/div/div[1]/p'

#' 
#' A difference from the previous example is we ar
#' 
## ---- eval=FALSE---------------------------------------------------------
## library(rvest)
## 
## # set address of RBA
## my.url <- 'http://www.rba.gov.au/'
## 
## # read html
## html.code <- read_html(my.url)
## 
## # set xpaths
## my.xpath.inflation <- '//*[@id="content"]/section[1]/div/div[2]/p'
## my.xpath.int.rate <- '//*[@id="content"]/section[1]/div/div[1]/p'
## 
## # get inflation from html
## my.inflation <- html_text(html_nodes(html.code,
##                                      xpath = my.xpath.inflation ))
## 
## # get interest rate from html
## my.int.rate <- html_text(html_nodes(x = html.code,
##                                    xpath = my.xpath.int.rate ))
## 
## # print result
## cat("\nCurrent inflation in AUS:", my.inflation)
## cat("\nCurrent interest rate AUS:", my.int.rate)

#' 
#' 
#' 
#' The use of _Webscraping_ techniques becomes a s
#' 