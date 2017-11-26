#' # Financial Data and Common Operations {#Financ
#' 
#' 
#' Before learning how to import data into R, it i
#' 
#' 
#' ## Data from Financial Markets
#' 
#' Without a doubt, the most popular dataset in fi
#' 
#' What differs one financial contract to the othe
#' 
#' Issuing stocks is an elaborate process. For mos
#' Investors make money by either selling the stoc
#' 
#' Prices in financial markets move according to s
#' 
#' Certain events can also alter the price of a st
#' 
#' Another event that impacts price is the payment
#' 
#' In its raw form, price data is recorded wheneve
#' 
#' The data from Yahoo Finance or others is in the
#' 
#' [^2]: Some companies, such as [kibot](http://ww
#' ), sell this data to the public.
#' 
#' From the viewpoint of data analysis, price data
#' 
#' 
#' ### Calculating Returns for a Single Asset
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
#' In R, let's first simulate a price series with 
#' 
## ---- tidy = FALSE-------------------------------------------------------
set.seed(10)
# simulate artificial prices
nT <- 5
P <- 10*(cumprod(1+rnorm(nT, 
                         mean = 0, 
						 sd = 0.1)))

# print prices
print(P)

#' 
#' Now, we can calculate a return vector based on 
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
#' In this case, the investor starts with 100% of 
#' 
#' The second type of return is the logarithmic (o
#' 
#' 
#' 
#' 
#' The biggest difference between arithmetic and l
#' 
## ---- tidy=FALSE---------------------------------------------------------
# using indexing
log.ret <- c(NA, log(P[2:length(P)]/
                     P[1:(length(P)-1)]))

# using diff					 
log.ret <- c(NA, diff(log(P)))					 

#' 
#' As for compounding log returns, the additive pr
#' 
#' 
#' 
#' We can use function `cumsum` to calculate the c
#' 
## ------------------------------------------------------------------------
# calculate accumulated log. return
acum.log.ret <- c(1, 1+cumsum(na.omit(log.ret)))
				  
# print result
print(acum.log.ret)

#' 
#' The conversion from log to arithmetic return is
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
#' As you can see from the printed `dataframe`, th
#' 
#' 
#' ### Calculating Returns for a Portfolio
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
nT <- 5

# simulate prices
P.1 <- 10*(cumprod(1+rnorm(nT,mean = 0, sd = 0.1)))
P.2 <- 20*(cumprod(1+rnorm(nT,mean = 0, sd = 0.1)))
P.3 <- 30*(cumprod(1+rnorm(nT,mean = 0, sd = 0.1)))

# gather info in df
my.df <- data.frame(ref.date = Sys.Date()+1:nT,
                    prices = c(P.1,P.2,P.3),
                    ticker = paste('Stock ',c(rep('A',nT),
                                              rep('B',nT),
                                              rep('C',nT))))

# print result  
print(my.df)

#' 
#' As mentioned before, using a matrix notation fa
#' 
## ------------------------------------------------------------------------
# set price matrix
my.price.mat <- matrix(my.df$prices, nrow = nT)

# set row and col names
colnames(my.price.mat) <- unique(my.df$ticker)
rownames(my.price.mat) <- as.character(unique(my.df$ref.date))

# print result
print(my.price.mat)

#' 
#' Now, from the price matrix, we can calculate a 
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
#' Vector `ret.port` show the return an investor w
#' 
#' 
#' ## Data from the Financial Evaluation of Projec
#' 
#' In corporate finance, we can evaluate a project
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
#' Notice how the first element is negative. This 
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

#' 
#' In this simple example, the net present value o
#' 
#' Package `FinCal` offers many more functions for
#' 
#' 
#' ## Data from Financial Statements
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
#' The previous format is flexible and can hold a 