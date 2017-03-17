## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(prompt = FALSE, cache = FALSE, tidy=TRUE)

#' 
#' # Programming and data analysis with R
#' 
#' In the previous chapters, we learned the ecosys
#' 
#' The concepts presented here are extremely valua
#' 
#' 
#' ## Creating Functions
#' 
#' As emphasized in earlier chapter, **the use of 
#' 
#' A function always has three parts: input, proce
#' 
## ----eval=FALSE----------------------------------------------------------
## my.fct <- function(arg1 = 1, arg2 = 'abc', ...){
## 
##   ...
## 
##   return(out)
## 
## }

#' 
#' And, after registering the function in the envi
#' 
## ----eval=FALSE----------------------------------------------------------
## out <- my.fct(arg1 = 2, arg2 = 'bcd')

#' 
#' The definition of a function is very similar to
#' 
#' Every function will return an object with the `
#' 
#' As for using the function, you'll need first to
#' 
#' Now, lets create a function that does something
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.fct <- function(x = c(1,1,1,1)){
  # Calculates the average of input x
  #
  # Args: 
  # 	x: a numerical vector
  #
  # Returns:
  #   The mean of x
  
  out <- sum(x)/length(x)
  
  return(out)
  
}

#' 
#' Notice how we set a comment section after the f
#' 
#' Functions should contain a comments section imm
#' 
#' After writing the function down, we need to exe
#' 
#' After executing the function definition, we can
#' 
## ------------------------------------------------------------------------
# testing function my.fct
my.mean <- my.fct(x = 1:100)

# print result
print(my.mean)

#' 
#' The result is `r my.mean`, as expected. 
#' 
#' If the function `my.fct` is called without any 
#' 
## ------------------------------------------------------------------------
# calling my.fct without input
my.mean <- my.fct()

# print result
print(my.mean)

#' 
#' Again, as expected, the returned value is corre
#' 
#' Although simple, the previous example can be fu
#' 
#' **By not testing the types of inputs in the fun
#' 
#' Correcting this problem is quite simple: just u
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.fct <- function(x = c(1,1,1,1)){
  # Calculates the average of input x
  #
  # Args: 
  # 	x: a numerical vector
  #
  # Returns:
  #   The mean of x
  
  if (!(class(x) %in% c('numeric','integer'))){
    stop('ERROR: x is not numeric or integer')
  }
  
  out <- sum(x)/length(x)
  
  return(out)
}

#' 
#' In the previous code, we use the `class` functi
#' 
## ---- error=TRUE---------------------------------------------------------
# using wrong inputs (ERROR)
my.fct(x = c('a','b'))

#' 
#' Going further in the development of our functio
#' 
## ------------------------------------------------------------------------
# sum with NA
print(sum(c(1, 2, 3, NA, 4)))

#' 
#' The problem with the `NA` is that this object i
#' 
#' In order to better handle `NA` values in functi
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.fct <- function(x = c(1,1,1,1)){
  # Calculates the average of input x
  #
  # Args: 
  # 	x: a numerical vector
  #
  # Returns:
  #   The mean of x
  
  if (!(class(x) %in% c('numeric','integer'))){
    stop('ERROR: x is not numeric or integer')
  }
  
  if (any(is.na(x))){
    warning('Warning: Found NA in x. Removing it.')
    x <- na.omit(x)
  }	
  
  out <- sum(x)/length(x)
  
  return(out)
}

#' 
#' For the previous code, we used function `warnin
#' 
## ----warning=TRUE--------------------------------------------------------
# set vector with NA
y <- c(1,2,3, NA,1)

# test function
print(my.fct(y))

#' 
#' As we can see, the function acknowledged the ex
#' 
#' Using comments and input testing is a good poli
#' 
#' Now, lets move to a more complete example of us
#' 
#' 
#' 
#' Notice that the calculation of returns must be 
#' 
#' To solve it, let's create a generic function th
#' 
#' First, let's register a function for calculatin
#' 
## ------------------------------------------------------------------------
calc.ret <- function(P) {
  # calculates aritmetic returns from a vector of prices
  #
  # Args:
  #   P - vector of prices (numeric)
  #
  # Returns:
  #   A vector of returns
  
  # ret = p_{t}/p_{t-1} - 1	
  my.length <- length(P)
  ret <- c(NA, P[2:my.length]/P[1:(my.length - 1)] - 1)
  return(ret)
}


#' 
#' Notice how we kept it simple. Since we will use
#' 
#' 
## ---- tidy = FALSE-------------------------------------------------------
df.calc.ret <- function(df.in, colname.price, colname.tickers){
  # Calculates an arithmetic return series and adds it to input
  #
  # Args:
  # 	df.in - a dataframe with columns for prices and tickers
  #	colname.price -  the name of the column in input df.in with prices
  #   colname.tickers - the name of the column with tickers
  #
  # Returns:
  # 	A copy of the input dataframe, but with a new column ret
  
  # error checking (classes)
  if ( class(df.in) != 'data.frame') {
    stop('ERROR: df.in should be a data.frame!')
  }
  
  if ( class(colname.price) != 'character') {
    stop('ERROR: colname.price should be a character object!')
  }
  
  if ( class(colname.tickers) != 'character') {
    stop('ERROR: colname.tickers should be a character object!')
  }
  
  # error checking (col.names)
  my.colnames <- colnames(df.in)
  
  if (any(!c(colname.price, colname.tickers) %in% my.colnames)) {
    stop('ERROR: column names dont match with names in df.in!')
  }
  
  # error checking (size of df)
  if ( nrow(df.in) < 2) {
    stop('ERROR: input df should have at least 2 rows!')
  }
  
  # do calc with tapply
  my.l <- tapply(X = df.in[[colname.price]], 
                 INDEX = df.in[[colname.tickers]], 
                 FUN = calc.ret)
  
  
  # restore order of tickers in df.in
  my.l <- my.l[unique(df.in[[colname.tickers]])]
  
  # set new col in df.in
  df.in$ret <- unlist(my.l)
  
  # return df
  return(df.in)
}

#' 
#' That's a lengthy code! But remember, you only n
#' 
#' Now, let's use the function with the data avail
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.f <- 'data/SP500-Stocks_long.csv'
my.df <- read.csv(my.f, colClasses = c('numeric', 'Date', 'factor'))

my.df <- df.calc.ret(my.df, 
                     colname.price = 'price.adjusted', 
                     colname.tickers = 'ticker')


#' 
#' Let's have a look at the result:
#' 
## ------------------------------------------------------------------------
#print(str(my.df, max.level = 1))

#' 
#' It looks great! The return vector is available 
#' 
## ------------------------------------------------------------------------
idx <- complete.cases(my.df)
my.df <- my.df[idx, ]

#' 
#' For last, we save the resulting dataset as a .R
#' 
## ------------------------------------------------------------------------
save(list = 'my.df', file = 'data/SP500-Stocks-WithRet.RData')

#' 
#' 
#' ## Using loops (_for_ command)
#' 
#' _Loops_ are the most basic command in any progr
#' 
#' The great thing about _loops_ is that the lengt
#' 
#' The definition of a _loop_ in R follows the fol
#' 
## ----eval=FALSE----------------------------------------------------------
## for (i in i.vec){
##   ...
## }

#' 
#' In the previous code, command `for` indicates t
#' 
## ------------------------------------------------------------------------
# set seq
my.seq <- seq(-5,5)

# do loop
for (i in my.seq){
  cat(paste('\nThe value of i is',i))
}

#' 
#' In the previous example, we created a sequence 
#' 
#' The iterated sequence in the _loop_ is not excl
#' 
## ------------------------------------------------------------------------
# set char vec
my.char.vec <- letters[1:5]

# loop it!
for (i.char in my.char.vec){
  cat(paste('\nThe value of i.char is', i.char))
}

#' 
#' The same goes for `lists`:
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set list
my.l <- list(x = 1:5, 
             y = c('abc','dfg'), 
             z = factor('A','B','C','D'))

# loop list
for (i.l in my.l){
  
  cat(paste0('\nThe class of i.l is ', class(i.l), '. '))
  cat(paste0('The number of elements is', length(i.l), '.'))
  
}

#' 
#' In the definition of _loops_, the iterator does
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set vec and iterators
my.vec <- seq(1:5)
my.x <- 5
my.z <- 10

for (i in my.vec){
  # iterate "mannualy"
  my.x <- my.x + 1
  my.z <- my.z + 2
  
  cat('\nValue of i = ', i, 
      ' | Value of my.x = ', my.x, 
      ' | Value of my.z = ', my.z)
}

#' 
#' The use of nested _loops_, that is, a _loop_ in
#' 
## ------------------------------------------------------------------------
# set matrix
my.mat <- matrix(1:9, nrow = 3)

# loop all values of matrix
for (i in seq(1,nrow(my.mat))){
  for (j in seq(1,ncol(my.mat))){
    cat(paste0('\nElement [', i, ', ', j, '] = ', my.mat[i,j]))
  }
}

#' 
#' Let's do a more complex example using data file
#' 
## ------------------------------------------------------------------------
# set number of files to create
n.files <- 10

# set first part of file names
pattern.name <- 'myfiles_'

# set dir
out.dir <- 'many_datafiles/'

# test if out.dir exists, if not, create it
if (!dir.exists(out.dir)){
  dir.create(out.dir)	
} 

# clean up folder before creating new files
file.remove(list.files(out.dir, full.names = TRUE))	

# set vec with filenames
file.names <- paste0(out.dir, pattern.name, seq(1,n.files), '.csv')

# loop it!
for (i.file in file.names){
  # create temp df
  temp.df <- data.frame(x = runif(100))
  
  # write it!
  write.csv(x = temp.df, file = i.file)
}

#' 
#' In the previous example, we used function `if` 
#' 
#' In the _loop_, we used function `runif` to crea
#' 
#' Now, lets check if the files are in the folder:
#' 
## ------------------------------------------------------------------------
# check files
print(list.files(out.dir))

#' 
#' As expected, the files are there. To complete t
#' 
## ------------------------------------------------------------------------
# set empty df
df.agg <- data.frame()
for (i.file in file.names){
  # read file
  temp.df <- read.csv(i.file)
  
  # row bind 
  df.agg <- rbind(df.agg, temp.df)
}

print(head(df.agg))

#' 
#' In the previous code, notice how we bind all `d
#' 
#' Looking at the contents of `df.agg`, we can see
#' 
#' Another practical example of the use _loop_ is 
#' 
#' 
## ---- tidy=FALSE---------------------------------------------------------
# read data
my.f <- 'data/SP500-Stocks_long.csv'
my.df <- read.csv(my.f, colClasses = c('numeric', 'Date','factor'))

# find unique tickers in column ticker
unique.tickers <- unique(my.df$ticker)

# create empty df
tab.out <- data.frame()

# loop tickers
for (i.ticker in unique.tickers){
  
  # create temp df with ticker i.ticker
  temp <- my.df[my.df$ticker==i.ticker, ]
  
  # row bind i.ticker and mean.price
  tab.out <- rbind(tab.out, 
                   data.frame(ticker = i.ticker,
                              mean.price = mean(temp$price.adjusted)))
  
}

# print result
print(tab.out[1:10, ])

#' 
#' In the code, we used function `unique` to find 
#' 
#' 
#' ## Conditional statements (if)
#' 
#' It is very common in R script to make decisions
#' 
## ----eval=FALSE----------------------------------------------------------
## # skeleton for if statement
## if (cond){
## 
##   CodeIfTRUE...
## 
## } else {
## 
##   CodeIfFALSE...
## 
## }

#' 
#' The place holder `cond` is the condition to be 
#' 
## ------------------------------------------------------------------------
# set vec and threshold
my.x <- 1:10
my.thresh <- 5

for (i in my.x){
  if (i > my.thresh){
    cat('\nValue of ', i, ' is higher than ', my.thresh)
  } else {
    cat('\nValue of ', i, ' is lower or equal than ', my.thresh)
  }
}

#' 
#' If we want to apply more than one logical condi
#' 
## ------------------------------------------------------------------------
for (i in my.x){
  if (i > my.thresh){
    cat('\nValue of ', i, ' is higher than ', my.thresh)
  } else if (i==my.thresh) {
    cat('\nValue of ', i, ' is equal to ', my.thresh)
  } else {
    cat('\nValue of ', i, ' is lower than ', my.thresh)
  }
}

#' 
#' 
#' ## Using `apply` functions
#' 
#' In R, there is an alternative to the usage of _
#' 
#' It is noteworthy to point out that all procedur
#' 
#' Now, lets discuss each type of _apply_ function
#' 
#' ### Using the `lapply` function
#' 
#' Function `lapply` takes as input a `list` and a
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list(1:10, 2:5, 10:-20)

# use lapply with mean
my.mean.vec <- lapply(X = my.l, FUN = mean)

# print result
print(my.mean.vec)

#' 
#' The result shows the means of each vector in `m
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list(c(1,NA,2), c(2:5,NA), 10:-20)

# use lapply with mean
my.mean.vec <- lapply(X = my.l, FUN = mean, na.rm=TRUE)

# print result
print(my.mean.vec)

#' 
#' The use of `lapply` is particularly useful when
#' 
## ---- tidy=FALSE---------------------------------------------------------
# function to generate files
create.rnd.file <- function(name.file, N=100){
  # Generates a csv file with random content
  #
  # Args:
  # 	name.file - name of csv file (character)
  #	N - number of rows in random dataframe (integer)
  #
  # Returns:
  # 	TRUE, if successful
  
  if (class(name.file)!='character'){
    stop('ERROR: input name.file is not a character')
  }
  
  if (class(N)!='numeric'){
    stop('ERROR: input N is not an integer!')
  }
  
  # create random df
  temp.df <- data.frame(x = runif(N))
  
  # write it!
  write.csv(x = temp.df, file = name.file)
  
  # return TRUE
  return(TRUE)
}

#' 
#' Now we use function `create.rnd.file` with `lap
#' 
## ------------------------------------------------------------------------
# set options
n.files <- 5
pattern.name <- 'myfiles_with_lapply_'
out.dir <- 'many_datafiles/'

# set file names
file.names <- paste0(out.dir,pattern.name, seq(1,n.files), '.csv')

# test if out.dir exists, if not, create it
if (!dir.exists(out.dir)){
  dir.create(out.dir)	
} 

# clean up folder before creating new files
file.remove(list.files(out.dir, full.names = TRUE))	

# use lapply
out.l <- lapply(X = file.names, FUN = create.rnd.file, N=100)

# print result
print(out.l)

#' 
#' 
#' Everything worked well in the previous code. Th
#' 
#' 
#' 
#' ### Using the `sapply` function
#' 
#' Function `sapply`  works similarly to `lapply`.
#' 
## ------------------------------------------------------------------------
# create list
my.l <- list(1:10, 2:5, 10:-20)

# use sapply
my.mean.vec <- sapply(my.l, mean)

# print result
print(my.mean.vec)

#' 
#' The use of `sapply` is recommended when the out
#' 
#' An important aspect of using `sapply` is that t
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set list
my.l <- list(runif(10), runif(15), rnorm(1000))

my.fct <- function(x){
  # returns mean and standard deviation of a vector
  #
  # Args: 
  #	x - numerical vector
  #
  # Returns:
  #	Vector as c(mean(x), sd(x))
  
  if (!(class(x) %in% c('numeric','integer'))){
    stop('ERROR: Class of x is not numeric or integer.')
  }
  
  x <- na.omit(x)
  
  out <- c(mean(x), sd(x))
  return(out)
  
}

# use sapply
my.vec <- sapply(my.l, my.fct)

# check result
print(my.vec)

#' 
#' When there is more than one output in the under
#' 
#' A practical use of function `sapply` in data an
#' 
## ---- tidy=FALSE---------------------------------------------------------
describe.vec <- function(x){
  # describe numerical vector with mean and other stats
  #
  # Args:
  #	x - numerical vector
  #
  # Returns:
  # 	a vector with mean, maximum and minimum
  
  # error checking
  if (!(class(x) %in% c('numeric','integer'))){
    stop('ERROR: Class of x is not numeric or integer.')
  }
  
  x <- na.omit(x)
  
  # calc vec
  out <- c(mean.price = mean(x), 
           max.price = max(x), 
           min.price = min(x))
  
  return(out)
}

#' 
#' Now, let's load the data and apply function `de
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set file and read it
my.f <- 'data/SP500-Stocks_long.csv'
my.df <- read.csv(my.f,
                  colClasses = c('numeric', 
                                 'Date',
                                 'factor'))

# use split to split prices by ticker
my.l <- split(x = my.df$price, my.df$ticker)

# use sapply
my.tab <- sapply(X = my.l, FUN = describe.vec)

# check result
print(head(t(my.tab)))

#' 
#' In this example, we used function `split` in `s
#' 
#' Using a descriptive table is very helpful to un
#' 
#' 
#' ### Using the `tapply` function
#' 
#' Function `tapply` differs from the others as it
#' 
## ------------------------------------------------------------------------
# set numeric vec and factor
my.x <- 1:150
my.factor <- factor(c(rep('C',50), rep('B',50), rep('A',50)))

# use tapply
my.mean.vec <- tapply(X = my.x, INDEX = my.factor, FUN = mean)

# print result
print(my.mean.vec)

#' 
#' An important point about the use of `tapply` is
#' 
#' Going back to the previous example using stock 
#' 
## ---- tidy=FALSE---------------------------------------------------------
# use tapply for descriptive stats
my.l.out <- tapply(X = my.df$price, 
                   INDEX = my.df$ticker, 
                   FUN = describe.vec)

# print result				   
print(my.l.out[1:5])

#' 
#' The output of `tapply` is a list of values. Eac
#' 
## ------------------------------------------------------------------------
# convert list to dataframe
my.tab <- do.call(what = rbind, args = my.l.out)

# print result
print(head(my.tab))

#' 
#' This is the first appearance of function `do.ca
#' 
#' Going back to the example, we can see that the 
#' 
#' ### Using the `mapply` function
#' 
#' Function `mapply` is a multivariate version of 
#' 
#' Assume that we are interested in creating a `li
#' 
## ------------------------------------------------------------------------
# set size
N <- 10

# prealocate list
my.l <- list()

for (i in seq(1,N)){
  my.l[[i]] <- seq(1,i)
}

# print result
print(my.l)

#' 
#' Another, less verbose solution, is to use `mapp
#' 
## ------------------------------------------------------------------------
# use mapply for creating list
my.l <- mapply(FUN = seq, rep(1,N), seq(1,N))

print(my.l)

#' 
#' Explaining the result, what function `mapply` i
#' 
#' 
#' ### Using the `apply` function
#' 
#' Function `apply` follows the same logic as the 
#' 
## ------------------------------------------------------------------------
# set matrix and print it
my.mat <- matrix(1:15, nrow = 5)
print(my.mat)

# sum rows with apply and print it
sum.rows <- apply(X = my.mat, MARGIN = 1, FUN = sum)
print(sum.rows)

# sum columns with apply and print it
sum.cols <- apply(X = my.mat, MARGIN = 2, FUN = sum)
print(sum.cols)

#' 
#' In the previous example, the `MARGIN` argument 
#' 
#' Expanding the example, we can use `apply` to fi
#' 
## ------------------------------------------------------------------------
# print max by row
print(apply(X = my.mat, MARGIN = 1, FUN = max))

# print max by column
print(apply(X = my.mat, MARGIN = 2, FUN = max))

#' 
#' 
#' ## Data manipulation with package `dplyr`
#' 
#' One of the most important asset of an experienc
#' 
#' For example, the previous operation of averagin
#' 
## ------------------------------------------------------------------------
library('dplyr')

#' 
#' The loading screen of `dplyr` shows several mes
#' 
#' 
#' In its current version `r my.ver`, `dplyr` has 
#' 
#' ### Manipulating a `dataframe` with `dplyr`
#' 
#' Package `dplyr` also includes several functions
#' 
#' If you want to select columns, you can use func
#' 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(dplyr)

# set rnd df
N <- 10
my.df <- data.frame(COL1 = runif(N), 
                    COL2 = runif(N), 
                    G = runif(N),
                    B = runif(N))

# select columns with dplyr::select
my.temp.df <- select(my.df, COL1, G)
print(my.temp.df)

# unselect columns with dplyr::select
my.temp.df <- select(my.df, -COL1, -COL2)
print(my.temp.df)

#' 
#' One innovation from the native way of selecting
#' 
## ---- tidy=FALSE---------------------------------------------------------
# select columnw with dplyr::select
my.temp.df <- select(my.df, starts_with('COL'))
print(my.temp.df)

#' This possibility is particularly interesting wh
#' 
#' For indexing (or filtering) rows, we use functi
#' 
## ------------------------------------------------------------------------
# filter rows with filter() - one condition
my.temp.df. <- filter(my.df, COL1 > 0.25)
print(my.temp.df)

# filter rows with filter() - two condition
my.temp.df <- filter(my.df, COL1 > 0.25, COL2 < 0.75)
print(my.temp.df)

#' 
#' 
#' We can add columns to a `dataframe` using `muta
#' 
## ------------------------------------------------------------------------
# add new columns with mutate
my.temp.df <- mutate(my.df, COL3 = COL1 + COL2,
                     COL4 = COL3 + runif(N) )

my.temp.df <- arrange(my.temp.df, COL1)
# print result
print(my.temp.df)

#' 
#' The use of `dplyr` functions for `dataframe` ma
#' 
#' 
#' ### The pipeline operator `%>%`
#' 
#' Another innovation of package `dplyr` is the _p
#' 
## ---- tidy=FALSE---------------------------------------------------------
# example of using the pipeline operator
my.temp.df.pipeline <- my.df %>% 
  select(COL1, COL2) %>%
  filter(COL1 > 0.25, COL2 < 0.75) %>%
  mutate(COL3 = COL1 + COL2,
         COL4 = COL3 + runif(length(COL1)) ) %>%
  arrange(COL1)

print(my.temp.df.pipeline)

#' 
#' The resulting code produces the same results as
#' 
#' 
#' ### Simple group operations with `group_by` and
#' 
#' To illustrate the use of the functions `group_b
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(dplyr)

# set and read data file
my.f <- 'data/SP500-Stocks_long.csv'
my.df <- read.csv(my.f,colClasses = c('numeric', 'Date','factor'))

# group data by ticker
my.G <- group_by(my.df, ticker)

# calculate stats
my.tab <- summarise(my.G, mean.price = mean(price.adjusted), 
                    max.price = max(price.adjusted), 
                    min.price = min(price.adjusted))

# print result						  
print(my.tab)

#' 
#' The first step in using  `dplyr` is to group th
#' 
#' After we group the data in `my.G`, we feed this
#' 
#' The use of `dplyr` is highly recommended when y
#' 
## ------------------------------------------------------------------------
# set new col week.day
my.df$week.day <- weekdays(my.df$ref.date)

# print it
print(head(my.df$week.day))

#' 
#' Now we proceed by adding column `week.day` in `
#' 
## ---- tidy=FALSE---------------------------------------------------------
# group by ticker and weekday
my.G <- group_by(my.df, ticker, week.day)

# calculate stats
my.tab <- summarise(my.G, mean.price = mean(price.adjusted), 
                    max.price = max(price.adjusted), 
                    min.price = min(price.adjusted))

# print result						  
print(my.tab)

#' 
#' And that's it! In order to group the data to a 
#' 
#' We can further refine the code using pipelines.
#' 
## ---- tidy=FALSE---------------------------------------------------------
# example of using the pipeline operator
my.tab <- my.df %>% 
  group_by(ticker, week.day) %>% 
  summarise(mean.price = mean(price.adjusted),
            max.price = max(price.adjusted),
            min.price = min(price.adjusted))

print(my.tab)

#' 
#' 
#' ### Complex group operations with `group_by` an
#' 
#' 
#' The previous example shows a simple case of gro
#' 
#' Let have a look in the following example, where
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(dplyr)
load('data/SP500-Stocks-WithRet.RData')

my.tab <- my.df %>%
  group_by(ticker) %>%
  do(acum.ret = cumprod(1+.$ret)) %>%
  mutate(last.cumret = acum.ret[length(acum.ret)],
         min.cumret = min(acum.ret))

print(head(my.tab))


#' 
#' Notice how column `acum.ret` is not a single va
#' 
#' The greatest advantage of using complex group o