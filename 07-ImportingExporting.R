#' # Importing and exporting data
#' 
#' In the previous chapter, we studied the main ty
#' 
#' ## About financial datasets
#' 
#' Before learning how to import data into R, it i
#' 
#' In most of the financial exchanges, when a comp
#' 
#' Prices in financial markets move according to s
#' 
#' It is important to point out that the data from
#' 
#' Another popular dataset in finance is financial
#' 
#' Now that you understands the most used types of
#' 
#' 
#' ## Import data from local files
#' 
#' The easiest way to import data into R is using 
#' 
#' In all cases presented here, we will assume tha
#' 
#' In this situation, using a folder for the scrip
#' 
## ---- eval=FALSE---------------------------------------------------------
## # set working directory
## my.d <- 'C:/My Research/'
## setwd(my.d)
## 
## # set file to be imported
## my.f <- 'data/my_data.csv'
## 

#' 
#' Notice how the code is self contained and porta
#' 
#' 
#' ### Importing data from a .csv file (_comma sep
#' 
#' 
#' Consider the data file called `r my.f`, located
#' 
#' The first lines, also called header line, of `r
#' 
#' To load the file `r my.f` in R, just use the `r
#' 
## ------------------------------------------------------------------------
# set file to read
my.f <- 'data/SP500.csv'

# read file
my.df.sp500 <- read.csv(my.f)

# print it
print(head(my.df.sp500))

#' 
#' The contents of the imported file are set as a 
#' 
## ------------------------------------------------------------------------
# print classes of all columns of my.df.sp500
print(sapply(my.df.sp500, class))

#' 
#' Note that the column of dates (_date_) was impo
#' 
#' The solution for the problem is simple: indicat
#' 
## ------------------------------------------------------------------------
# read csv file with correct col classes
my.df.sp500 <- read.csv(my.f,  colClasses = c('Date', 'numeric'))

# print column classes
print(sapply(my.df.sp500, class))

#' 
#' As we can see, the result is now correct. Anoth
#' 
## ------------------------------------------------------------------------
# load raw data
my.df.sp500 <- read.csv(my.f)

# convert columns to correct classes
my.df.sp500$date <- as.Date(my.df.sp500$date)
my.df.sp500$price <- as.numeric(my.df.sp500$price)

# print column classes
print(sapply(my.df.sp500, class))

#' 
#' Again, we got the desired result. As a rule of 
#' 
#' Going further, function `read.csv` has several 
#' 
#' Another possibility for importing csv files as 
#' 
## ------------------------------------------------------------------------
library(readr)

# read file with readr::read_csv
my.df.sp500 <- read_csv(my.f)

#' 
#' Notice how the previous code presented a messag
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(readr)

# set cols from import message
my.cols <- cols(date = col_date(format = ""),
				price = col_double() ) 

# read file with readr::read_csv
my.df.sp500 <- read_csv(my.f, col_types = my.cols)

#' 
#' Now, lets check the classes of the column:
#' 
## ------------------------------------------------------------------------
# print column classes
print(sapply(my.df.sp500, class))

#' 
#' As expected, it looks good. Both columns have t
#' 
#' 
#' ### Importing data from an _Excel_ file 
#' 
#' In many situations, the data to be analysed is 
#' 
#' R does not have a native function for importing
#' 
#' Although the previous cited packages have simil
#' 
#' In this section, we will give priority to packa
#' 
## ----eval=FALSE----------------------------------------------------------
## install.packages('readxl')

#' 
#' Now, consider a file called `SP500-Excel.xlsx` 
#' 
## ------------------------------------------------------------------------
library(readxl)

# set excel file
my.f <- 'data/SP500-Excel.xlsx'

# read excel file 
my.df <- read_excel(my.f, sheet = 'sp500-prices')

# print with str
print(str(my.df))

#' 
#' As we can see, one of the benefits of using Exc
#' 
#' The downside of using Excel files for storing d
#' 
#' 
#' ### Importing data from a .RData file
#' 
#' R has a native format to save objects from your
#' 
#' To create a new _.RData_ file, use the `save` f
#' 
## ------------------------------------------------------------------------
# set a object
my.x <- 1:100

# set name of RData file
my.file <- 'data/temp.RData'

# save it
save(list = c('my.x'), file = my.file)

#' 
#' We can verify the existence of the file with th
#' 
## ------------------------------------------------------------------------
# print contents of data folder
print(list.files('data'))

#' 
#' As expected, the `r my.file` file is available 
#' 
## ------------------------------------------------------------------------
# clear environment
rm(list=ls())

# load file
load(file = 'data/temp.RData')

# print all objects in environment
print(ls())

#' 
#' We can see that object `my.x` was recovered and
#' 
#' 
#' ### Importing data from SQLITE
#' 
#' The use of .csv or .RData files for storing dat
#' 
#' This brings us to the topic of _database softwa
#' 
#' Before moving to the examples, it is worth to u
#' 
#' As an example, let's first create an SQLITE dat
#' 
## ----echo=FALSE, message=FALSE-------------------------------------------
f.sqlite <- 'data/MySQLiteDatabase.SQLITE'
if (file.exists(f.sqlite)) file.remove(f.sqlite)


#' 
## ---- tidy=FALSE---------------------------------------------------------
library(RSQLite)

# set number of rows in df
N = 10^6 

# create simulated dataframe
my.large.df.1 <- data.frame(x=runif(N), 
                            G= sample(c('A','B'),
                                         size = N,
                                         replace = TRUE))

my.large.df.2 <- data.frame(x=runif(N), 
                            G = sample(c('A','B'),
                                         size = N,
                                         replace = TRUE))

# set name of SQLITE file
f.sqlite <- 'data/MySQLiteDatabase.SQLITE'

# open connection
my.con <- dbConnect(drv = SQLite(), f.sqlite)

# write df to sqlite
dbWriteTable(conn = my.con, name = 'MyTable1', value = my.large.df.1)
dbWriteTable(conn = my.con, name = 'MyTable2', value = my.large.df.2)

# disconnect
dbDisconnect(my.con)

#' From the logical output of `dbWriteTable` we ca
#' 
#' Now, let's use the previously created file to r
#' 
## ------------------------------------------------------------------------
# set name of SQLITE file
f.sqlite <- 'data/MySQLiteDatabase.SQLITE'

# open connection
my.con <- dbConnect(drv = SQLite(), f.sqlite)

# read table
my.df <- dbReadTable(conn = my.con, name = 'MyTable1')

# print with str
print(str(my.df))

#' 
#' It worked. The `dataframe` is exactly as expect
#' 
#' Another example of using SQLITE is with the act
#' 
## ------------------------------------------------------------------------
my.SQL <- "select * from myTable2 where G='A'"
my.df.A <- dbGetQuery(conn = my.con, statement = my.SQL)

dbDisconnect(my.con)

# print with str
print(str(my.df.A))

#' 
#' It also worked as expected. 
#' 
#' In this simple example we can see how easy it i
#' 
#' 
#' ### Importing data from a text file
#' 
#' In some cases, we are faced with data stored in
#' 
## ------------------------------------------------------------------------
# set file to read
my.f <- 'data/SP500.csv'

# read file line by line
my.txt <- readLines(my.f)

# print first five lines
print(my.txt[1:5])

#' 
#' In this case, we imported the entire content of
#' 
#' 
#' ### Other file formats
#' 
#' Using the import functions for files with exten
#' 
#' 
#' ## Importing data using the Internet
#' 
#' One of the great advantages of using R for data
#' 
#' In most cases, the importation of data from the
#' 
#' 
#' ### Package `quantmod`
#' 
#' To import financial data of stocks traded on st
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
#' Notice that the last date of the imported data 
#' 
#' In `my.df`, the only column with a not so obvio
#' 
#' It is noteworthy to point out that we used argu
#' 
#' Sometimes, Yahoo Finance has specific codes for
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
#' Have a look in the following example, where we 
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
#' As we can see, everything worked perfectly. The
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
##                         first.date = first.date,
##                         last.date = last.date)
## 

#' 
#' Be aware that running the previous code takes t
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
info <- CompanyInfo(my.ticker)
print(info)

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
#' Let's see how each Facebook investor was financ
#' 
## ----tidy=FALSE----------------------------------------------------------
library(dplyr)

# set col and date
my.col <- 'Earnings Per Share, Basic'
my.date <- '2016-12-31'
 
# print earnings per share
print(filter(my.income,  
             Metric == my.col))

#' 
## ----echo=FALSE----------------------------------------------------------
my.earnings <- filter(my.income,
                      endDate == my.date, 
					  Metric  == my.col)$Amount

#' 
#' From the data we can see that Facebook investor
#' 
#' An interesting aspect of `finreportr` is that i
#' 
#' 
#' ### Package `tidyquant`
#' 
## ----echo=FALSE, message=FALSE-------------------------------------------
library(tidyquant)

#' 
#' `tidyquant` is a package that offers lots of fu
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

my.df <- my.ticker %>% 
    tq_get(get = "stock.prices", 
	 from = first.date, 
	 to = last.date)

print(tail(my.df))

#' 
#' As we can see, the price data is the same as us
#' 
## ------------------------------------------------------------------------
df.key.ratios <- c("AAPL") %>%
           tq_get(get = "key.ratios")
		   
print(df.key.ratios)		   

#' 
#' Object `df.key.ratios` offers many fundamental 
#' 
## ------------------------------------------------------------------------
# get profitability table
df.profitability <- df.key.ratios$data[[2]]
		   
print(tail(df.profitability))

#' 
#' A novel and noteworthy aspect of `tidyquant` is
#' 
## ----echo=FALSE----------------------------------------------------------
# get stocks in AMEX
print(head(tq_exchange('AMEX')))

#' 		   
#' We can also get information about components of
#' 
## ----echo=FALSE----------------------------------------------------------
# get available indices
available.indices <- tq_index_options()
print(available.indices)

#' 
#' Let get information for `"DOWJONES"`.
#' 
## ----echo=FALSE----------------------------------------------------------
# get components of "DOWJONES"
print(tq_index("DOWJONES"))

#' 
#' These function are very useful because they giv
#' 
#' We only looked into a few function from package
#' 
#' 
#' ### Package `GetHFData`
#' 
#' Package `GetHFData` [@gethfdata] was written by
#' 
#' Let's try a simple example by downloading trade
#' 
## ---- echo=FALSE---------------------------------------------------------
file.remove(list.files('ftp files',full.names = TRUE))

#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------
library(GetHFData)

# set ticker for Petrobras
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
                          type.output = 'agg',
                          agg.diff = '5 min')

# print results
print(str(my.df, max.level=1))

#' 
#' As you can see, function `ghfd_get_HF_data` dow
#' 
#' 
#' ### Package `ustyc`
#' 
#' Package `ustyc` is designed to allow the downlo
#' 
#' Using package `ustyc` is very simple, all that 
#' 
## ------------------------------------------------------------------------
library(ustyc)

# get yield curve
my.yield.curve <- getYieldCurve(year = 2016)

#' 
#' Let's check the contents of the output with `st
#' 
## ---- tidy=FALSE---------------------------------------------------------
#print(str(my.yield.curve, max.level = 1))

#' 
#' The return object is a list, where the yield cu
#' 
## ------------------------------------------------------------------------
# print result
print(head(my.yield.curve$df))

#' 
#' 
#' ### Package `Quandl`
#' 
#' Another major source of financial data is the _
#' 
#' To use Quandl, the first step is to register a 
#' 
## ------------------------------------------------------------------------
# set api key to quandl
my.api.key <- 'Asv8Ac7zuZzJSCGxynfG'

#' 
#' This API key is unique to each user and the one
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
#' ### Other packages
#' 
#' Also worth highlighting two other packages that
#' 
#' WRITE MORE HERE
#' 
#' 
#' ### Accessing data from web pages (_webscraping
#' 
#' The previous packages are very useful because t
#' 
#' #### Scraping the website of Bank of England
#' 
#' This page shows the current prices and returns 
#' 
#' WRITE MORE HERE
#' 
## ----BOE-website, echo = FALSE, out.width = '100%', fig.cap = 'Bank of England website'----
knitr::include_graphics('figs/website_BOE-webscrapping.png')

#' 
#' The import of such data for R easy. The package
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(rvest)

my.url <- 'http://www.bankofengland.co.uk/Pages/home.aspx'

html.code <- read_html(my.url)

my.xpath <- paste0('//*[@id="ctl00_ctl21_g_347435ac_bd2d_',
                  '401a_a07a_99384607446d_ctl00_lstKeyFacts_ctrl3_Label2"]')
my.inflation <- html_text(html_nodes(x = html.code,xpath = my.xpath ))

my.xpath <- paste0('//*[@id="ctl00_ctl21_g_347435ac_bd2d_401a_a07a_',
                   '99384607446d_ctl00_lstKeyFacts_ctrl0_Label2"]')

my.intrate <- html_text(html_nodes(x = html.code,xpath = my.xpath ))

cat("\nCurrent inflation in the UK: ", my.inflation)
cat("\nCurrent interest rate in the UK: ", my.intrate)

#' 
#' 
#' 
#' The use of _Webscraping_ techniques becomes a s
#' 
#' 
#' ## Export to local file
#' 
#' A very common operation in the use of R is to e
#' 
#' Exporting data from R involves a decision regar
#' 
#' Thus, it is very easy to share data with other 
#' 
#' 
#' ### Exporting data to a _.csv_ file 
#' 
#' To write a _.csv_ file, simply use the `write.c
#' 
## ------------------------------------------------------------------------
# set the number of rows
N <- 100

# set dataframe
my.df <- data.frame(y = runif(N), z = rep('a',N))

# set file out
f.out <- 'data/temp.csv'

# write to files
write.csv(x = my.df, file = f.out)

#' 
#' In the previous example, we save the object  `m
#' 
#' 
## ------------------------------------------------------------------------
# read it
my.df.import <- read.csv(f.out)

# print first five rows
print(head(my.df.import))

#' 
#' Note that a column called `x` containing the na
#' 
## ------------------------------------------------------------------------
# set the number of rows
N <- 100

# set dataframe
my.df <- data.frame(y = runif(N), z = rep('a',N))

# set file out
f.out <- 'data/temp.csv'

# write to files (without rownames)
write.csv(x = my.df, file = f.out, row.names=FALSE)

# check result
my.df.import <- read.csv(f.out)
print(head(my.df.import))

#' 
#' As we can see, the row numbers are no longer sa
#' 
#' 
#' ### Exporting data to an Excel file
#' 
#' Exporting a `dataframe` to an Excel file is als
#' 
#' An example of usage is given next.
#' 
## ------------------------------------------------------------------------
library(xlsx)

# create dataframe
N <- 50
my.df <- data.frame(y = seq(1,N), z = rep('a',N))

# set excel file
f.out <- 'data/temp.xlsx'

# write to excel
write.xlsx(x = my.df, file = f.out, sheetName = "my df")

#' 
#' An important information here is that, if you w
#' 
## ------------------------------------------------------------------------
# create two dataframes
N <- 25
my.df.A <- data.frame(y = seq(1,N), z = rep('a',N))
my.df.B <- data.frame(z = rep('b',N))

# set file out
f.out <- 'data/temp.xlsx'

# write in different sheets
write.xlsx(x = my.df.A, file = f.out, sheetName = "my df A")
write.xlsx(x = my.df.B, file = f.out, sheetName = "my df B", append = TRUE )

#' 
#' 
#' ### Exporting data to a text file
#' 
#' In some situations, you may need to export some
#' 
## ------------------------------------------------------------------------
# set file
my.f <- 'data/temp.txt'

# set some string
my.str <- paste(letters[1:5], '\n', collapse = '')

# save string to file
cat(my.str, file = my.f, append = FALSE)

#' 
#' In the previous example, we created a text obje
#' 
## ------------------------------------------------------------------------
print(readLines(my.f))

#' 
#' As we can see, it worked as expected.
#' 
#' 
#' ## The format of the `dataframe` (_long_ and _w
#' 
#' After understanding the process of importing in
#' 
#' **In the wide format**, the rows are usually in
#' 	
#' 
#' | refdate    | STOCK1| STOCK2| STOCK3|
#' |------------|-------|-------|-------|
#' | 2015-01-01 | 10    | 3     | 6     |
#' | 2015-01-02 | 11    | 3.1   | 7     |
#' | 2015-01-03 | 10.5  | 3.2   | 7.5   |
#' | 2015-01-04 | 12    | 3.5   | 6     |
#' | ...        | ...   | ...   | ...   |
#' 
#' Note that the above table has three distinct in
#' 	
#' **In the long format**, each row of the `datafr
#' 
#' | refdate    | asset.code | Price |
#' |------------|------------|-------|
#' | 2015-01-01 | STOCK1     | 10    |
#' | 2015-01-01 | STOCK2     | 3     |
#' | 2015-01-01 | STOCK3     | 6     |
#' | 2015-01-02 | STOCK1     | 11    |
#' | 2015-01-02 | STOCK2     | 3.1   |
#' | 2015-01-02 | STOCK3     | 7     |
#' | 2015-01-03 | STOCK1     | 10.5  |
#' | 2015-01-03 | STOCK2     | 3.2   |
#' | 2015-01-03 | STOCK3     | 7.5   |
#' | ...        | ...        | ...   |
#' 
#' 	
#' At first glance, this argument may seem trivial
#' 
#' In Finance, the wide format is generally used i
#' 
#' 
#' ### Converting a `dataframe` structure (long an
#' 
#' The conversion from one format to the other is 
#' 
## ------------------------------------------------------------------------
library(tidyr)

# set dates and stock vectors
refdate <- as.Date('2015-01-01') + 0:3
STOCK1 <- c(10, 11, 10.5, 12)
STOCK2 <- c(3, 3.1, 3.2, 3.5)
STOCK3 <- c(6, 7, 7.5, 6)

# create wide dataframe
my.df.wide <- data.frame(refdate, STOCK1, STOCK2, STOCK3)

# convert wide to long
my.df.long <- gather(data = my.df.wide,key = 'ticker',value = 'price', - refdate)

# print result
print(my.df.long)

#' 
#' To perform the reverse conversion, _long_ to _w
#' 
## ---- tidy=FALSE---------------------------------------------------------
# convert from long to wide
my.df.wide.converted <- spread(data = my.df.long, 
                               key = 'ticker',
							   value = 'price')
							   
# print result
print(my.df.wide.converted)

#' 
#' In the case of more complex conversions, where 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(reshape2)

# use melt to change from wide to long
my.df.long <- melt(data = my.df.wide, 
				   id.vars = 'refdate', 
				   variable.name = 'ticker', 
				   value.name = 'price')

# print result				   
print(my.df.long)

#' 
## ------------------------------------------------------------------------
# use melt to change from long to wide
my.df.wide.converted <- dcast(data = my.df.long, 
                              formula = refdate ~ ticker, 
							  value.var = 'price')
print(my.df.wide.converted)

#' 
#' It is important to know these functions when wo