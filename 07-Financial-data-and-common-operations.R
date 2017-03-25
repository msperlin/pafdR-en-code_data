#' # Financial data and common operations {#Financ
#' 
#' 
#' Before learning how to import information into 
#' 
#' 
#' ## Data from financial markets
#' 
#' Without a doubt, the most popular dataset in fi
#' 
#' What differs one financial contract to the othe
#' 
#' The process of issuing stocks is elaborate. In 
#' 
#' Prices in financial markets move according to s
#' 
#' Sites like [Yahoo Finance](https://finance.yaho
#' 
#' It is important to point out that the data from
#' 
#' [^2]: Some companies, such as [kibot](http://ww
#' ), sell this data to the public.
#' 
#' From the point of view of data analysis, price 
#' 
#' 
#' #### Calculating returns
#' 
#' After importing price data, one of the most com
#' 
#' Returns are simply the percentage difference of
#' 
#' There are two types of returns, arithmetic and 
#' 
#' 
#' 
#' So, based on a series of prices, we can create 
#' 
#' In R, lets first simulate a price series with t
#' 
## ---- tidy = FALSE-------------------------------------------------------
set.seed(10)
# simulate artificial prices
N <- 5
P <- 10*(cumprod(1+rnorm(N, 
                         mean = 0, 
						 sd = 0.1)))

# print prices
print(P)

#' 
#' In practice, this data will be provided from an
#' 
## ------------------------------------------------------------------------
# calculate arit. return
arit.ret <- c(NA, P[2:length(P)]/
                  P[1:(length(P)-1)] -1)
				  
# print result
print(arit.ret)

#' 
#' Arithmetic returns can also be compounded, whic
#' 
#' 
#' 
#' In R, we can calculate it using function `cumpr
#' 
## ------------------------------------------------------------------------
# calculate accumulated arit. return
acum.arit.ret <- c(1, cumprod(1+na.omit(arit.ret)))
				  
# print result
print(acum.arit.ret)

#' 
#' Notice how it was necessary to omit all `NA` va
#' 
#' The second type of return is the logarithmic (o
#' 
#' 
#' 
#' 
#' As you can see, calculation wise, the biggest d
#' 
## ---- tidy=FALSE---------------------------------------------------------
log.ret <- c(NA, log(P[2:length(P)]/
                     P[1:(length(P)-1)]))

#' 
#' 
#' As for compounding log returns, the additive pr
#' 
#' 
#' 
#' As for implementing it in R, we can use functio
#' 
## ------------------------------------------------------------------------
# calculate accumulated log. return
acum.log.ret <- c(1, 1+cumsum(na.omit(arit.ret)))
				  
# print result
print(acum.log.ret)

#' 
#' At this point, it is important to acknowledge t
#' 
#' 
#' 
#' 
#' 
#' 
#' Let's try it out:
#' 
## ------------------------------------------------------------------------
# converting arit ret to log ret and vice-versa
arit.ret.from.log <- exp(log.ret)-1
log.ret.from.arit <- log(arit.ret+1)

# print result as df
print(data.frame(arit.ret, arit.ret.from.log,
                 log.ret, log.ret.from.arit))


#' 
#' 
#' As you can see, the returns from the conversion
#' 
#' 
#' #### Calculating return for a portfolio
#' 
#' Another common operation in using data from fin
#' 
#' 
#' 
#' Now, let's try an example where the investor ha
#' 
## ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(10)

# set number of time periods
N <- 5

# simulate prices
P.1 <- 10*(cumprod(1+rnorm(N,mean = 0, sd = 0.1)))
P.2 <- 20*(cumprod(1+rnorm(N,mean = 0, sd = 0.1)))
P.3 <- 30*(cumprod(1+rnorm(N,mean = 0, sd = 0.1)))

# gather info in df
my.df <- data.frame(ref.date = Sys.Date()+1:N,
                    prices = c(P.1,P.2,P.3),
                    ticker = paste('Stock ',c(rep('A',N),
                                              rep('B',N),
                                              rep('C',N))))

# print result  
print(my.df)

#' 
#' As mentioned before, using a matrix notation fa
#' 
## ------------------------------------------------------------------------
# set price matrix
my.price.mat <- matrix(my.df$prices, nrow = N)

# set row and col names
colnames(my.price.mat) <- unique(my.df$ticker)
rownames(my.price.mat) <- as.character(unique(my.df$ref.date))

# print result
print(my.price.mat)

#' Now, from the price matrix we can calculate a r
#' 
## ---- tidy=FALSE---------------------------------------------------------
# apply return formula for each column
my.ret.mat <- my.price.mat[2: nrow(my.price.mat)   , ]/
              my.price.mat[1:(nrow(my.price.mat)-1), ] -1 

# print it!
print(my.ret.mat)

#' 
#' Finally, we use the weight vector in a `matrix`
#' 
## ------------------------------------------------------------------------
# set weight of portfolio (evenly weighted)
w <- matrix(rep(1/3,3), nrow = 3)

# calculate return of portfolio over time
ret.port <- my.ret.mat %*% w

# print result
print(ret.port)

#' 
#' Vector `ret.port` show the return that an inves
#' 
#' ## Data from financial evaluation of projects
#' 
#' Another type of financial data is related to th
#' 
#' As for the data side, the cash-flow of projects
#' 
## ------------------------------------------------------------------------
# set project cashflow
CF <- c(-8000, 2500, 3000, 3000, 5000)
names(CF) <- paste('Year', 0:(length(CF)-1))

# print result
print(CF)

#' 
#' Notice how the first element is negative. It me
#' 
## ------------------------------------------------------------------------
library(FinCal)

# set discount rate
r <- 0.1

# calculate npv
my.NPV <- npv(r = r, cf = CF)

# calculate irr
my.IRR <- irr(cf = CF)

# calculate payback
my.payback <- min(which(cumsum(CF) > 0)) - 1

# print results
cat('NPV = ', my.NPV )
cat('IRR = ', my.IRR)
cat('Payback = ', my.payback, 'years')

#' In this simple example, the net present value o
#' 
#' Package `FinCal` offers many more functions for
#' 
#' 
#' ## Data from financial statements
#' 
#' Another popular dataset in finance relates to f
#' 
#' When looking at data for just one company in on
#' 
#' * RefTime - reference date or time period
#' * CompanyID - unique identifier for companies
#' * TypeOfFinStatement - the type of financial st
#' * TypeAccount - type/name of account (total ass
#' * Value - the value of the account
#' 
#' The previous format can hold a large number of 