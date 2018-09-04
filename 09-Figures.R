#' # Creating and Saving Figures with `ggplot2` {#
#' 
#' 
#' Using graphical resources in technical reports 
#' 
#' In this book, we will not go deep into `ggplot2
#' 
#' For most examples given here, we will work with
#' 
#' First, let's load the data.
#' 
## ---- eval=TRUE----------------------------------------------------------
# set file and load data
my.f <- 'data/SP500-Stocks-WithRet.RData'
load(my.f)

# print first 5 rows
print(head(my.df))

#' 
#' ## Using Graphic Windows
#' 
#' Before studying the use of `ggplot2`, we need t
#' 
#' A more intelligent approach to managing figures
#' 
## ---- eval=FALSE---------------------------------------------------------
## x11()
## plot(1:10)

#' 
#' The visual result in RStudio should be similar 
#' 
#' 
#' Each call to `x11()` will create a new window. 
#' 
#' After creating so many windows, it is best to c
#' 
#' 
#' ## Creating Figures with Function `qplot`
#' 
#' Package `ggplot2` has an introductory function,
#' 
#' To build a time series plot with the prices of 
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width---------
library(ggplot2)

# filter stock data
temp.df <- my.df[my.df$ticker == 'MMM', ]

# plot its prices
qplot(data = temp.df, 
      x = ref.date, 
      y = price.adjusted, 
      geom = 'line')

#' 
#' In the previous example, the name of the axis c
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width---------
qplot(data = temp.df, 
      x = ref.date, 
      y = price.adjusted, 
      geom = 'line', 
      xlab = 'Dates', 
      ylab = 'Adjusted closing prices')

#' 
#' Notice how the horizontal axis of dates in the 
#' 
#' 
#' ## Creating Figures with Function `ggplot` {#gg
#' 
#' Using function `qplot` is recommended when you 
#' 
#' Before presenting examples using `ggplot`, let'
#' 
#' The distinction between the steps of creating a
#' 
#' Look at the syntax of the following example tha
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
p <- ggplot(data = temp.df, aes(x = ref.date, y = price.adjusted))
p <- p + geom_line()
p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
print(p)

#' 
#' In using `ggplot`, it is always necessary to pr
#' 
#' 
#' Once the data and axis are defined, we save it 
#' 
## ----eval=TRUE-----------------------------------------------------------
library(ggplot2)
library(stringr)

# get names of functions in ggplot2
fcts <- ls('package:ggplot2')

# select those that starts with geom_
idx <- str_sub(fcts, 1, 5) == 'geom_'
fcts <- fcts[idx]

# print result
print(fcts)

#' 
#' As you can see, there are plenty of options. Pa
#' 
#' Going back to our example, the third line of th
#' 
## ---- eval=FALSE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
## p <- ggplot(data = temp.df, aes(x = ref.date, y = price.adjusted))
## p <- p + geom_line()
## #p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
## print(p)

#' 
#' One of the great advantages of using `ggplot` i
#' 
## ----eval=TRUE-----------------------------------------------------------
# fix seed
set.seed(10)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

# find all rows that contain the stocks
idx <- my.df$ticker %in% my.tickers

# create temporary df
temp.df <- my.df[idx, ]

#' 
#' In this code, first, we set a random seed, so a
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
p <- ggplot(data = temp.df, aes(x = ref.date, 
                                y = price.adjusted, 
                                colour=ticker))
p <- p + geom_line()
p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
print(p)

#' 
#' A difference from the previous examples is that
#' 
#' Now, let's use what we learned so far to create
#' 
## ----eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(ustyc)
library(tidyr)
library(ggplot2)
library(stringr)

# get yield curve
my.df.yc <- getYieldCurve(year = 2016)$df

# set date.col
my.df.yc$ref.date <- as.Date(rownames(my.df.yc))

# change to long format and convert to factor
my.df.yc <- gather(data = my.df.yc,
                   key = 'maturity',
                   value = 'rate',
                   -ref.date)

names(my.df.yc) <- c('ref.date', 'maturity', 'rate')
my.df.yc$maturity <- as.factor(my.df.yc$maturity)

# keep only longer term yields (names with YEAR)
idx <- str_detect(my.df.yc$maturity, 'YEAR')
my.df.yc <- my.df.yc[idx, ]

# change name to year number with regex
# obs: regex ([0-9]+) extracts all numbers within a string
out <- str_extract_all(string = my.df.yc$maturity,
                       pattern = '([0-9]+)')
my.df.yc$maturity <- as.numeric(out)

# keep only last date of each
last.date <- max(my.df.yc$ref.date)
my.df.yc.last.date <- my.df.yc[my.df.yc$ref.date == last.date, ]

# plot it!
p <- ggplot(my.df.yc.last.date, aes(x=maturity, y=rate))
p <- p + geom_point(size=2)
p <- p + geom_line(size=1)
p <- p + labs(x = 'Maturity (years)', 
              y='Yield Rate',
              title = paste0('US Yield Curve (',last.date,')' ))

print(p)


#' 
#' As expected, the current yield curve is upward 
#' 
#' 
## ------------------------------------------------------------------------
# set number of periods 
n.periods <- 5

# set sequence of observations
my.seq <- floor(seq(1,nrow(my.df.yc), length.out = n.periods))

# get actual dates from sequence
my.dates <- my.df.yc$ref.date[my.seq]

# find rows for dates in df
idx <- my.df.yc$ref.date %in% my.dates
my.df.yc.periods <- my.df.yc[idx, ]

# plot it!
p <- ggplot(my.df.yc.periods, aes(x=maturity, 
                                  y=rate, 
                                  color= factor(ref.date)))
p <- p + geom_point(size=2)
p <- p + geom_line(size=1)
p <- p + labs(x = 'Maturity (years)', 
              y='Yield Rate',
              title = 'US Yield Curve')

print(p)


#' The US yield curve changed significantly in 201
#' 
#' As another example of geometric shape in `ggplo
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
library(dplyr)
# fix seed
set.seed(10)

# select 10 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 10)

# find all rows that contain the stocks
idx <- my.df$ticker %in% my.tickers

# create temporary df
temp.df <- my.df[idx, ]

plot.df <- temp.df %>%
  group_by(ticker) %>%
  summarise(sharpe.ratio = mean(ret)/sd(ret))


p <- ggplot(data = plot.df, 
            aes(x = reorder(ticker, -sharpe.ratio), 
                y = sharpe.ratio))
p <- p + geom_bar(stat = 'identity')
p <- p + labs(x = 'Tickers', y = 'Daily Sharpe Ratio')
print(p)

#' 
#' 
#' ### Using Themes
#' 
#' One way of customizing graphics in `ggplot2` is
#' 
## ----eval=TRUE-----------------------------------------------------------
library(ggplot2)
library(stringr)

# get all functions
fcts <- ls('package:ggplot2')

# find out those that start with theme_
idx <- str_sub(fcts, 1, 6) == 'theme_'
fcts <- fcts[idx]

# print result
print(fcts)

#' 
#' Let's try it with the theme of function `theme_
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
p <- ggplot(data = temp.df, aes(x = ref.date, 
                                y = price.adjusted, 
                                colour=ticker))
p <- p + geom_line()
p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
p <- p + theme_bw()

print(p)

#' 
#' As you can see, the new theme was a white backg
#' 
#' In the previous example, notice how the structu
#' 
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
p <- p + scale_colour_grey(start = 0.0, end = 0.6)
print(p)  

#' 
#' The lines of the plot are now in grey. The inpu
#' 
#' 
#' ### Creating Panels with `facet_wrap`
#' 
#' Another possibility in creating graphics for di
#' 
#' Facets are possible with function `facet_wrap`,
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
library(dplyr)
# fix seed
set.seed(20)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ref.date, y = price.adjusted)) + 
  geom_line() + 
  labs(x = 'Date', 
       y = 'Adjusted closing prices') + 
  facet_wrap(facets = ~ticker)

print(p)

#' 
#' Using panels is recommended when the data of th
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
# fix seed
set.seed(25)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ref.date, y = ret)) + 
  geom_line(size=1) + 
  labs(x = 'Date', 
       y = 'Returns') +   
  facet_wrap(facets = ~ticker)

print(p)

#' 
#' Notice how the vertical axis of the panels is f
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width---------
p <- p + facet_wrap(facets = ~ticker, scales = 'free')

print(p)

#' 
#' 
#' ## Using Pipelines for Data Analysis and Figure
#' 
#' Another great thing about `ggplot2` is you can 
#' 
## ---- eval=TRUE, tidy=FALSE,fig.height=my.fig.height, fig.width=my.fig.width----
library(dplyr)
library(ggplot2)

# calculated mean and sd of returns, plot result
p <- my.df %>%
  group_by(ticker) %>%
  summarise(mean.ret = mean(ret),
            std.ret = sd(ret)) %>%
  ggplot(aes(x = std.ret, y = mean.ret)) +
  geom_point() + 
  labs(x = 'Standard deviation of returns', 
       y = 'Average Returns')


print(p)

#' 
#' Notice how the previous code is self-contained,
#' 
#' We only scratched the surface of `ggplot2`. Man
#' 
#' 
#' ## Creating Statistical Graphics
#' 
#' Package `ggplot` has several options for creati
#' 
#' 
#' ### Creating Histograms
#' 
#' A histogram shows the empirical distribution of
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width---------
p <- ggplot(data = my.df, aes(x = ret))
p <- p + geom_histogram(bins = 25)

print(p)

#' 
#' Here, we only need to define the _x_ value, wit
#' 
#' We can also use groups and facets as we did for
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
# fix seed
set.seed(30)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ret)) + 
  geom_histogram(bins = 50) +
  facet_wrap(facets = ~ticker)

print(p)

#' 
#' A histogram with the empirical densities of the
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ret)) + 
  geom_density() + 
  facet_wrap(facets = ~ticker)

print(p)

#' 
#' The previous figure allows a clear visual compa
#' 
#' 
#' ### Creating _boxplot_ Figures
#' 
#' Figures of type _boxplot_ (or box and whisker d
#' 
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
# fix seed
set.seed(35)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ticker, y = price.adjusted)) + 
  geom_boxplot()

print(p)

#' 
#' As we can see from the previous figure, the sto
#' 
#' 
#' ### Creating _QQ_ Plots
#' 
#' QQ plots show a comparison between the distribu
#' 
#' Let's try an example with some simulated data.
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
# fix seed
set.seed(40)

N=1000
my.mean <- 10
my.sd <- 2

temp.df <- data.frame(y=rnorm(n = N, mean = my.mean, sd = my.sd))

p <- ggplot(data = temp.df, aes(sample = y)) 
#p <- p + labs(title = 'QQ plot for simulated data')
p <- p + geom_qq(distribution = qnorm, 
                 dparams = c(mean=my.mean, sd=my.sd))

print(p)

#' 
#' In the previous code, we simulate random normal
#' 
#' Now, let's try it for our dataset of stock's re
#' 
#' 
## ----eval=TRUE,fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
# fix seed
set.seed(45)

# select 4 stock randomly and filter from my.df
my.tickers <- sample(unique(my.df$ticker), 4)
temp.df <- filter(my.df, ticker %in% my.tickers)

# set function for normalization
norm.vec <- function(y){
  # Normalizes a vector by subtracting mean and dividing
  # by the standard deviation
  #
  # Args:
  #   y - numerical vector
  #
  # Returns:
  #   A normalized vector
  
  y.norm <- (y-mean(y, na.rm = TRUE))/sd(y, na.rm = TRUE)
  return(y.norm)
}

# apply function  
my.l <- tapply(X = temp.df$ret, 
               INDEX = factor(temp.df$ticker), 
               FUN = norm.vec)

# reorder list (tapply sorts alphabetically)
my.l <- my.l[as.character(unique(temp.df$ticker))]

# save new column norm.ret
temp.df$norm.ret <- unlist(my.l)

# plot it!
p <- ggplot(data = temp.df, aes(sample = norm.ret)) 
p <- p + geom_qq()
p <- p + facet_wrap(~ticker)

print(p)

#' 
#' 
#' As you can see, the result is not visually simi
#' 
#' 
#' ## Saving Graphics to a File
#' 
#' To save pictures created with `ggplot`, use fun
#' 
#' Consider the following example, where we create
#' 
## ----eval=TRUE, tidy=FALSE,fig.height=my.fig.height, fig.width=my.fig.width----
library(dplyr)
# fix seed
set.seed(40)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ref.date, y = price.adjusted, color = ticker)) + 
  geom_line() + 
  labs(x = 'Date', 
       y = 'Adjusted closing prices')

my.fig.file <- 'fig_ggplot/MyPrices.png'
ggsave(filename = my.fig.file, 
       plot=p,
       dpi = 600)

#' 
#' You can verify the creation of the file with fu
#' 
## ------------------------------------------------------------------------
print(list.files('fig_ggplot'))

#' 
#' As expected, the file is available in folder `f