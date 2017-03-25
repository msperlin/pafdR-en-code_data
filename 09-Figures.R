#' # Creating and Saving Figures {#Figures} 
#' 
#' 
#' The use of graphical resources in technical rep
#' 
#' R has built-in functions for creating figures, 
#' 
#' This deficiency was remedied by users. In 2005,
#' 
#' In this book, we will not go deep into `ggplot2
#' 
#' For most of the examples given here, we will wo
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
#' The file contains daily adjusted closing prices
#' 
#' 
#' ## Using graphic windows
#' 
#' Before studying the codes for the creation of f
#' 
#' A more intelligent approach to the creation of 
#' 
## ---- eval=FALSE---------------------------------------------------------
## x11()
## plot(1:10)

#' 
#' The result should be something close to Figure 
#' 
#' 
#' Each call to `x11()` will create a new window. 
#' 
#' After creating so many windows, it is best to c
#' 
#' 
#' ## Creating figures with function `qplot`
#' 
#' Package `ggplot2` has an introductory function 
#' 
#' For example, to build a time series plot with t
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width---------
library(ggplot2)

# filter stock data
temp.df <- my.df[my.df$ticker == 'MMM', ]

# plot its prices
qplot(data = temp.df, x = ref.date, y = price.adjusted, geom = 'line')

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
#' ## Creating figures with function `ggplot`
#' 
#' The use of function `qplot` is recommended for 
#' 
#' Before presenting examples in the use of `ggplo
#' 
#' This distinction between the steps of creating 
#' 
#' Have a look in the syntax of the following exam
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width----
p <- ggplot(data = temp.df, aes(x = ref.date, y = price.adjusted))
p <- p + geom_line()
p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
print(p)

#' 
#' In the use of `ggplot`, it is always necessary 
#' 
#' 
#' In the previous code, once the data and axis ar
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
#' In this code, first we set a random seed so tha
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

# get current year
current.year <- format(Sys.Date(),'%Y')

# get yield curve
my.df.yc <- getYieldCurve(year = current.year)$df

# set date.col
my.df.yc$ref.date <- as.Date(rownames(my.df.yc))

# change to long format and convert to factor
my.df.yc <- gather(data=my.df.yc, key =ref.date)
names(my.df.yc) <- c('ref.date', 'maturity', 'rate')
my.df.yc$maturity <- as.factor(my.df.yc$maturity)

# keep only longer term yields (names with YEAR)
idx <- str_detect(my.df.yc$maturity, 'YEAR')
my.df.yc <- my.df.yc[idx, ]

# change name to year number with 
# obs: regex ([0-9]+) extracts all numbers within a string
out <- str_extract_all(string = my.df.yc$maturity,
                       pattern = '([0-9]+)')
my.df.yc$maturity <- as.numeric(out)

# keep only last date of each
last.date <- max(my.df.yc$ref.date)
my.df.yc <- my.df.yc[my.df.yc$ref.date==last.date, ]

# plot it!
p <- ggplot(my.df.yc, aes(x=maturity, y=rate))
p <- p + geom_point(size=2)
p <- p + geom_line(size=1)
p <- p + labs(x = 'Maturity (years)', 
              y='Yield Rate',
              title = paste0('US Yield Curve (',last.date,')' ))

print(p)


#' 
#' 
#' ### Using themes
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
#' Let's give it a try with the theme of function 
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
p <- ggplot(data = temp.df, aes(x = ref.date, 
                                y = price.adjusted, 
								colour=ticker))
p <- p + geom_line()
p <- p + labs(x = 'Dates', y = 'Adjusted closing prices')
p <- p + theme_bw()
p <- p + scale_colour_grey(start = 0.0, end = 0.6)
print(p)  

#' 
#' The lines of the plot are now in a grey. The in
#' 
#' 
#' 
#' ### Creating panels with `facet_wrap`
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
#' The use of panels is recommended when the data 
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width, tidy=FALSE----
# fix seed
set.seed(25)

# select 4 stocks randomly
my.tickers <- sample(unique(my.df$ticker), 4)

p <- my.df %>%
  filter(ticker %in% my.tickers) %>%
  ggplot(aes(x = ref.date, y = ret)) + 
  geom_line() + 
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
#' ## Using pipelines for data analysis and figure
#' 
#' Another great thing about `ggplot2` is that you
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
#' We really once scratched the surface of `ggplot
#' 
#' 
#' ## Creating statistical graphics
#' 
#' Package `ggplot` has several options for creati
#' 
#' ### Creating histograms
#' 
#' A histogram show the empirical distribution of 
#' 
## ----eval=TRUE, fig.height=my.fig.height, fig.width=my.fig.width---------
p <- ggplot(data = my.df, aes(x = ret))
p <- p + geom_histogram(bins = 25)
  
print(p)

#' 
#' In this case, we only need to define the _x_ va
#' 
#' We can also use groups and facets in the same w
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
#' We can also create a histogram with the empiric
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
#' ### Creating _boxplot_ figures
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
#' As we can see clearly from the previous figure,
#' 
#' ### Creating _QQ_ plots
#' 
#' QQ plots shows a comparison between the distrib
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
  # Normalizes a vector
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
#' ## Saving pictures
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