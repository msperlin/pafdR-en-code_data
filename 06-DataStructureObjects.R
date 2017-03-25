#' # Data structure objects {#DataStructureObjects
#' 
## ---- echo=FALSE---------------------------------------------------------
my.engine <- knitr:::pandoc_to()

#' 
#' In the previous chapter, we learned about the b
#' 
#' 
#' ## `Lists`
#' 
#' A `list` is an extremely flexible object that c
#' 
#' 
#' ### Creating lists
#' 
#' A list can be created with the `list` command, 
#' 
## ------------------------------------------------------------------------
# create a list with three elements
my.l <- list(1, c(1,2,3), c('a', 'b'))

# print result
print(my.l)

#' 
#' Notice how an object of type `list` is printed 
#' 
#' The elements of a `list` can also be named. Thi
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create named list
my.named.l <- list(ticker = 'ABC', 
                   name.company = 'Company ABC',
                   price = c(1,1.5,2,2.3), 
                   market = 'NYSE', 
                   date.price = as.Date('2016-01-01')+0:3)

# print list		  
print(my.named.l)

#' 
#' In this example we have a named list with sever
#' 
#' 
#' ### Accessing the elements of a list
#' 
#' As mentioned before, the individual elements of
#' 
## ------------------------------------------------------------------------
# print second element of my.l
print(my.l[[2]])

# print third element of my.l
print(my.l[[3]])

#' 
#' You can also access the elements of a `list` wi
#' 
## ------------------------------------------------------------------------
# accessing list with [[ ]]
class(my.l[[2]])

# accessing list with [ ]
class(my.l[2])

#' 
#' If we try to add an element to `my.l[2]`, we wi
#' 
## ----error=TRUE----------------------------------------------------------
# adding an element to a list (WRONG)
my.l[2] + 1

#' 
#' An error is returned because a  `list` object c
#' 
## ------------------------------------------------------------------------
# set new list with first and second element of my.l
my.new.l <- my.l[c(1,2)]

# print result
print(my.new.l)

#' 
#' In general, using its position to access elemen
#' 
#' Next we provide several example of how to acces
#' 
## ------------------------------------------------------------------------
# accessing elements of a list using $
print(my.named.l$ticker)
print(my.named.l$price)

# accessing elements of a list using [['name']]
print(my.named.l[['ticker']])
print(my.named.l[['price']])

#' 
#' An useful trick for working with lists is that 
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.l <- list(slot1 = c(num1 = 1, num2 = 2, num3 = 3), 
             slot2 = c('a', 'b'))

# access the second value of the first element of my.l
print(my.l[[1]][2])

# access the first value of the second element of my.l
print(my.l[[2]][1])

# access the value 'num3' in 'slot1'
print(my.l[['slot1']]['num3'])

#' 
#' This operation is very useful when you are inte
#' 
#' 
#' ### Adding and removing elements from a `list`
#' 
#' To add or replace elements in a `list`, just se
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list('a',1,3)

# show it
print(my.l)

# change value at position 4
my.l[[4]] <- c(1:5)

# change value at position 2
my.l[[2]] <- c('b')

# print result
print(my.l)

#' 
#' This operation is also possible with the use of
#' 
## ------------------------------------------------------------------------
# set named list
my.l <- list(slot1 = 'a', slot2 = 5)

# print it
print(my.l)

# add a new slot
my.l$slot3 <- 10

# print result
print(my.l)

#' 
#' To remove elements from a `list`, just set the 
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list(text = 'b', num1 = 2, num2 = 4)

# remove third element
my.l[[3]] <- NULL

# show result
print(my.l)

# remove element 'num1'
my.l$num1 <- NULL

# print result
print(my.l)

#' 
#' Another way of removing elements from a `list` 
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list(a=1, b='text')

# print my.l without second element
print(my.l[[-2]])

#' 
#' As in the case of atomic vectors, removing elem
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list(1, 2, 3, 4)

# remove all elements higher than 2
my.l[my.l > 2] <- NULL

# print result
print(my.l)

#' 
#' However, note that this operation only works be
#' 
## ------------------------------------------------------------------------
# set list
my.l <- list(1, 2, 3, 4, 'a', 'b')

# print logical test (return NA value)
print(my.l > 2)

#' 
#' 
#' ### Processing the elements of a list
#' 
#' An important information about objects of the t
#' 
#' As an example, consider a list of numeric vecto
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set list with different numerical vectors.
my.l.num <- list(c(1,2,3), 
                 seq(1:50), 
                 seq(-5,5, by=0.5))

#' 
#' Let's assume that we wanted to calculate the av
#' 
## ------------------------------------------------------------------------
# calculate means
mean.1 <- mean(my.l.num[[1]])
mean.2 <- mean(my.l.num[[2]])
mean.3 <- mean(my.l.num[[3]])

# print result
print(c(mean.1, mean.2, mean.3))

#' 
#' An easier, more elegant and smarter way of doin
#' 
## ------------------------------------------------------------------------
# using sapply
my.mean <- sapply(my.l.num, mean)

# print result
print(my.mean)

#' 
#' As expected, the result is identical to the pre
#' 
#' This use of generic procedures is one of the pr
#' 
#' 
#' ### Other Functions for manipulating lists
#' 
#' * **unlist** - Returns the elements of a `list`
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create list
my.named.l <- list(ticker = 'XXXX4', 
                   price = c(1,1.5,2,3), 
                   market = 'Bovespa')

# unlist its elements				   
my.unlisted <- unlist(my.named.l)

# print result
print(my.unlisted)
class(my.unlisted)

#' 
#' * **as.list** - Converts an object to the list 
#' 
## ------------------------------------------------------------------------
# set atomic vector
my.x <- 10:13

# convert to list
my.x.as.list <- as.list(my.x)

# print result
print(my.x.as.list)

#' 
#' * **names** - Returns or defines the names of t
#' 
## ------------------------------------------------------------------------
# set named list
my.l <- list(value1 = 1, value2 = 2, value3 = 3)

# print its names
print(names(my.l))

# change its names
names(my.l) <- c('num1', 'num2', 'num3')

# print result
print(my.l)

#' 
#' 
#' ## `Matrix`
#' 
#' As you may remember from your math classes, a m
#' 
#' In R, matrix are objects with two dimensions, w
#' 
#' A simple example of using matrices in finance i
#' 
#' 
#' The above matrix could be created in R with the
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set raw data with prices 
raw.data <- c(40.38,  40.14,  40.49,  40.48,  40.64,
              46.23,  46.17,  45.97,  45.56,  45.46,
              238.58, 239.61, 234.67, 237.25, 238.92,
              43.43,  43.96,  44.26,  44.5,   44.86)

# create matrix          
my.mat <- matrix(raw.data, nrow = 5, ncol = 4)
colnames(my.mat) <- c('AAP', 'COG', 'BLK', 'CAM')
rownames(my.mat) <- c("2010-01-04", "2010-01-05", "2010-01-06", 
                      "2010-01-07", "2010-01-08")

# print result
print(my.mat)

#' 
#' That was a long code! But, do not worry. In pra
#' 
#' In the previous example of creating a `matrix` 
#' 
## ------------------------------------------------------------------------
# print the names of columns 
print(colnames(my.mat))

# print the names of rows
print(rownames(my.mat))

#' 
#' After matrix `my.mat` is created, we have at ou
#' 
#' 
#' 
#' In this formula, `r if (my.engine!='epub3') {'$
#' 
## ------------------------------------------------------------------------
# set vector with shares purchased
my.stocks <- as.matrix(c(200, 300, 100, 50), nrow = 4)

# get value of portfolio with matrix multiplication
my.port <- my.mat %*% my.stocks

# print result
print(my.port)

#' 
#' In this last example, we use symbol `%*%` which
#' 
#' An important point to emphasize here is that a 
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create matrix with character
my.mat.char <- matrix(rep(c('a','b','c'), 3), 
                      nrow = 3, 
                      ncol = 3)

# print it					  
print(my.mat.char)

#' 
#' Now with a `logic` type:
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create matrix with logical
my.mat.logical <- matrix(sample(c(TRUE,FALSE), 
                                size = 3*3,
                                replace = TRUE),
                         nrow = 3, 
                         ncol = 3)

# print it					  
print(my.mat.logical)

#' 
#' This flexibility allows the user to expand the 
#' 
#' 
#' ### Selecting elements from a `matrix`
#' 
#' Following the same notation of atomic vector, y
#' 
#' [^1]: To avoid confusion, it is worth noting th
#' 
#' The extra dimension of matrices requires select
#' 
## ------------------------------------------------------------------------
# create matrix
my.mat <- matrix(1:9, nrow = 3)

# display it
print(my.mat)

# display element in [1,2]
print(my.mat[1,2])

#' 
#' To select an entire row or column, simply leave
#' 
## ------------------------------------------------------------------------
# select all rows from column 2
print(my.mat[ , 2])

# select all columns from row 1
print(my.mat[1, ])

#' 
#' Notice that the result of indexing is an atomic
#' 
## ------------------------------------------------------------------------
# force matrix conversion and print result
print(as.matrix(my.mat[ ,2]))

# force matrix conversion for one row and print result
print(matrix(my.mat[1, ], nrow=1))

#' 
#' Pieces of the `matrix` can also be selected usi
#' 
## ------------------------------------------------------------------------
# select some elements and print it
print(my.mat[2:3,1:2])

#' 
#' Finally, the use of logical tests to select ele
#' 
## ------------------------------------------------------------------------
# set matrix
my.mat <- matrix(1:9, nrow = 3)

# print logical matrix where value is higher than 5
print(my.mat >5)

# print the result
print(my.mat[my.mat >5])

#' 
#' 
#' ### Other useful functions for manipulating mat
#' 
#' * **as.matrix** - Transforms raw data to a `mat
#' 
## ------------------------------------------------------------------------
# set matrix
my.mat <- as.matrix(1:5)

# print it
print(my.mat)

#' 
#' * **t** - Returns a transposed  `matrix`. \inde
#' 
## ------------------------------------------------------------------------
# set matrix
my.mat <- matrix(seq(10,20, length.out = 6), nrow = 3)

# print it
print(my.mat)

# transpose and print
print(t(my.mat))

#' 
#' * **rbind** - Returns the merger (bind) of matr
#' 
## ------------------------------------------------------------------------
# set matrices and print
my.mat.1 <- matrix(1:5, nrow = 1)
print(my.mat.1)
my.mat.2 <- matrix(10:14, nrow = 1)
print(my.mat.2)

# bind them together using the rows
my.rbind.mat <- rbind(my.mat.1, my.mat.2)

# print result
print(my.rbind.mat)

#' 
#' * **cbind** - Returns the merger (bind) of matr
#' 
## ------------------------------------------------------------------------
# set matrices and print
my.mat.1 <- matrix(1:4, nrow = 2)
print(my.mat.1)
my.mat.2 <- matrix(10:13, nrow = 2)
print(my.mat.2)

# bind them together using the columns
my.cbind.mat <- cbind(my.mat.1, my.mat.2)

# print the result
print(my.cbind.mat)

#' 
#' 
#' ## `Dataframes`
#' 
#' In R, objects of type `dataframe` are the most 
#' 
#' Another positive aspect of using  `dataframes` 
#' 
#' 
#' ### Creating `dataframes`
#' 
#' As with other classes, the creation of a `dataf
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set ticker symbols as a vector
ticker <- c(rep('AAP',5), rep('COG', 5), rep('BLK', 5), rep('CAM',5))

# set a date vector
date <- as.Date(rep(c("2010-01-04", "2010-01-05", "2010-01-06", 
                      "2010-01-07", "2010-01-08"), 4) )

# set prices					  
prices <- c(40.38,  40.14,  40.49,  40.48,  40.64,
            46.23,  46.17,  45.97,  45.56,  45.46,
            238.58, 239.61, 234.67, 237.25, 238.92,
            43.43,  43.96,  44.26,  44.5,   44.86)

# create dataframe			
my.df <- data.frame(ticker = ticker, 
                    date = date, 
                    prices = prices)

# print result
print(my.df)

#' 
#' We used function `rep` to replicate and facilit
#' 
#' It is also worth pointing out that the content 
#' 
#' 
#' For those that like to use the prompt, the resu
#' 
#' 
#' ### Accessing information from a `dataframe`
#' 
#' A `dataframe` object makes use of same commands
#' 
#' To find out the names of the columns of a `data
#' 
## ------------------------------------------------------------------------
# get names of columns with names
names(my.df)

# get names of columns with colnames
colnames(my.df)

#' 
#' To access a particular column of a `dataframe`,
#' 
## ------------------------------------------------------------------------
# get column ticker from my.df
my.ticker <- my.df$ticker	

# get column price from my.df
my.prices <- my.df['prices']

# get second column from my.df
my.date <- my.df[ ,2] 

# print the results
print(my.ticker)
print(my.prices)
print(my.date)

#' 
#' Another important information about `dataframe`
#' 
## ------------------------------------------------------------------------
# select column in dataframe with list notation
print(my.df[[2]])

#' 
#' To access specific rows and columns of a `dataf
#' 
## ------------------------------------------------------------------------
# accessing rows 1:5, column 2
print(my.df[1:5,2])

# accessing rows 1:5, columns 1 and 2
print(my.df[1:5,c(1,2)])

# accessing rows 1:5, all columns
print(my.df[1:5, ])

#' 
#' Column selection can also be performed using na
#' 
## ------------------------------------------------------------------------
# selecting rows 1 to 3, columns 'ticker' and 'prices'
print(my.df[1:3, c('ticker','prices')])

#' 
#' 
#' ### Modifying a `dataframe`
#' 
#' To create a new column in a `dataframe`, simply
#' 
## ------------------------------------------------------------------------
# add a sequence to my.df
my.df$my.seq <- 1:nrow(my.df)

# print result
print(my.df)

#' 
#' You can also perform this modification of a `da
#' 
## ------------------------------------------------------------------------
# set new col by name
my.df['my.seq.2'] <- seq(1,100, length.out = nrow(my.df))

# set new col by position
my.df[[6]] <- seq(1,10, length.out = nrow(my.df))
print(my.df)

#' 
#' Note that, when using column position for setti
#' 
## ---- tidy=FALSE---------------------------------------------------------
# rename colnames
colnames(my.df) <- c('ticker', 'date', 'prices', 
                     'my.seq', 'my.seq.2', 'my.seq.3')

# print result
print(my.df)

#' 
#' To remove the columns of a `dataframe`, just se
#' 
## ------------------------------------------------------------------------
# removing some columns
my.df$my.seq <- NULL
my.df$my.seq.2 <- NULL
my.df$V6 <- NULL

# print final result
print(my.df[1:5, ])

#' 
#' You can also remove columns using negative indi
#' 
## ------------------------------------------------------------------------
# create new dataframe without cols 1 and 3 of my.df
new.df <- my.df[ ,c(-1,-3)]

# print result
print(new.df)

#' 
#' Just as we have done for matrices, indexing `da
#' 
## ------------------------------------------------------------------------
# set logical index for selecting data about stock
my.idx <- my.df$ticker == 'AAP'

# create new df with index
my.df.stock <- my.df[my.idx, ]

# print result
print(my.df.stock)

#' 
#' We can also interact different columns using lo
#' 
## ------------------------------------------------------------------------
# find index with which.max
my.idx <- which.max(my.df.stock$price)

# get date
my.date <- my.df.stock$date[my.idx]

# print result
print(my.date)

#' 
#' Therefore, in our dataset, the highest price of
#' 
#' A not well known property of `dataframes`  is t
#' 
## ------------------------------------------------------------------------
# set index with which.max
my.df <- data.frame(a=runif(5))

# set columns name "MyColumn 1" with grave accent
my.df$`My column 1` <- runif(5)

# set columns name "MyColumn 2" with apostrophe
my.df$'My column 2' <- runif(5)

#' 
#' In this case, we used the grave accent and apos
#' 
#' 
#' ### Sorting a `dataframe`
#' 
#' After creating or importing a `dataframe`, you 
#' 
#' For example, consider creating a `data.frame` w
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set new df
my.df <- data.frame(col1 = c(4,1,2), 
                    col2 = c(1,1,3), 
                    col3 = c('a','b','c'))

# print it					
print(my.df)

#' 
#' Function `order` returns the position of the el
#' 
## ------------------------------------------------------------------------
# set index with positions of ascending order in col1
idx <- order(my.df$col1)

# print it
print(idx)

#' 
#' Therefore, when using the output of function `o
#' 
## ------------------------------------------------------------------------
# order my.df by col1
my.df.2 <- my.df[order(my.df$col1), ]

# print result
print(my.df.2)

#' 
#' This operation may also be performed taking int
#' 
## ------------------------------------------------------------------------
# sort df with col2 and col1
my.df.3 <- my.df[order(my.df$col2, my.df$col1), ]

# print result
print(my.df.3)

#' 
#' 
#' ### Combining and aggregating data frames
#' 
#' Sometimes it is necessary to join different `da
#' 
## ------------------------------------------------------------------------
# set two dfs with same colnames
my.df.1 <- data.frame(col1 = 1:5, col2 = rep('a', 5))
my.df.2 <- data.frame(col1 = 6:10, col2 = rep('b', 5))

# bind them by rows
my.df <- rbind(my.df.1, my.df.2)

# print result
print(my.df)

#' 
#' Notice that, in the previous example, the names
#' 
## ---- error=TRUE, eval=FALSE, tidy=FALSE---------------------------------
## # set two df with different colnames
## my.df.1 <- data.frame(col1 = 1:5,
##                       col2 = rep('a', 5))
## my.df.2 <- data.frame(col1 = 6:10,
##                       col3 = rep('b', 5))
## 
## # bind them by rows (ERROR)
## my.df <- rbind(my.df.1, my.df.2)

#' 
## ---- echo=FALSE---------------------------------------------------------
cat('##Error in match.names(clabs, names(xi)) :\n names do not match previous names')

#' 
#' In the case where you got various `dataframes` 
#' 
## ---- message=FALSE, tidy=FALSE------------------------------------------
# load package
library(dplyr)

# set two dfs (names don't match perfectly)
my.df.1 <- data.frame(col1 = 1:5,
                      col2 = rep('a', 5))
my.df.2 <- data.frame(col1 = 6:10, 
                      col3 = rep('b', 5))

# bind them by rows
my.df <- bind_rows(my.df.1, 
                   my.df.2)

# print result (NAs where there should be a column)
print(my.df)

#' 
#' For the case of column bind with function `cbin
#' 
## ------------------------------------------------------------------------
# set two dfs
my.df.1 <- data.frame(col1 = 1:5, col2 = rep('a', 5))
my.df.2 <- data.frame(col3 = 6:10, col4 = rep('b', 5))

# column bind dfs
my.df <- cbind(my.df.1, my.df.2)

# print result
print(my.df)

#' 
#' If the number of rows don't match, an error is 
#' 
## ------------------------------------------------------------------------
# set two dfs with same name in one
my.df.1 <- data.frame(col1 = 1:5, col2 = rep('a', 5))
my.df.2 <- data.frame(col1 = 6:10)

# column bind dfs
my.df <- cbind(my.df.1, my.df.2)

# print result (!)
print(my.df)

#' 
#' Yes, we have two columns named `col1`, but in d
#' 
#' For more complex cases, where the binding proce
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set dfs
my.df.1 <- data.frame(date = as.Date('2016-01-01')+0:10, 
                      x = 1:11)

my.df.2 <- data.frame(date = as.Date('2016-01-05')+0:10,
                      y = seq(20,30, length.out = 11))

# merge dfs by date					  
my.df <- merge(my.df.1, my.df.2, by = 'date')

# print result
print(my.df)

#' 
#' From the result we can see that the resulting `
#' 
#' 
#' ### Reporting a `dataframe` table using `xtable
#' 
#' Objects of type `dataframe` can be used to repr
#' 
#' Another way, a bit more elaborate, is to remove
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(xtable)

# set number of rows in table
N = 10

# set table
my.table <- data.frame('Stocks' = paste('Stock',1:N),
                       'Mean Return' = rnorm(N),
                       'StDev. of Ret' = abs(runif(N)),
                       'Max. Return' = abs(runif(N)),
                       'Min. Return' = abs(runif(N)), 
                       check.names = F)

# set xtable object
my.xtable <- xtable(x = my.table, 
                    label = 'tab:DescRetStats',
                    caption = 'Descriptive Return Statistics')

# print output to latex file
my.f.tex <- 'tabs/MyTable.tex'
print(my.xtable,
      include.rownames = FALSE,
      file = my.f.tex,
      type='latex')

#' 
#' In the previous code, notice how we used column
#' 
## ----echo=FALSE, results='asis'------------------------------------------
print(my.xtable, 
      include.rownames = FALSE,
      comment = FALSE, 
      size = 'small')

#' 
#' Another interesting package that is worth menti
#' 
## ---- tidy=FALSE, results='asis', message=FALSE--------------------------
library(stargazer)

stargazer(my.table, 
          summary = FALSE, 
          title = 'Descriptive Returns Statistics', 
          type = 'latex', 
          style = 'qje', 
          font.size = 'footnotesize', 
          rownames = FALSE, 
          out.header = FALSE,
          header = FALSE,
          label = 'tab:DescRetStats_stargazer' )


#' 
#' As for exporting tables to Word (Microsoft) or 
#' 
## ------------------------------------------------------------------------
# set html file for output
my.f.html <- 'tabs/MyTable.html'

# write it!
print(x = my.xtable,
      file = my.f.html,
      type = 'html',
      include.rownames = FALSE )

#' 
#' Once the file is available, we can open `r my.f
#' 
#' 
#' If you find yourself dealing with lots of figur
#' 
#' 
#' ### The format of the `dataframe` (_long_ and _
#' 
#' After understanding the basics of `dataframe` m
#' 
#' In the **wide format**, the rows are usually in
#' 	
#' | refdate    | STOCK1| STOCK2| STOCK3|
#' |:----------:|:-----:|:-----:|:-----:|
#' | 2015-01-01 | 10    | 3     | 6     |
#' | 2015-01-02 | 11    | 3.1   | 7     |
#' | 2015-01-03 | 10.5  | 3.2   | 7.5   |
#' | 2015-01-04 | 12    | 3.5   | 6     |
#' | ...        | ...   | ...   | ...   |
#' 
#' Note that the above table has three distinct in
#' 
#' In the **long format**, each row of the `datafr
#' 
#' | refdate    | asset.code | Price |
#' |:----------:|:----------:|:-----:|
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
#' At first glance, this argument may seem trivial
#' 
#' In finance, the wide format is generally used i
#' 
#' 
#' #### Converting a `dataframe` structure (long a
#' 
#' The conversion from one format to the other is 
#' 
## ---- tidy=FALSE---------------------------------------------------------
library(tidyr)

# set dates and stock vectors
refdate <- as.Date('2015-01-01') + 0:3
STOCK1 <- c(10, 11, 10.5, 12)
STOCK2 <- c(3, 3.1, 3.2, 3.5)
STOCK3 <- c(6, 7, 7.5, 6)

# create wide dataframe
my.df.wide <- data.frame(refdate, STOCK1, STOCK2, STOCK3)

# convert wide to long
my.df.long <- gather(data = my.df.wide,
                     key = 'ticker',
                     value = 'price',
                     - refdate)

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
#' 
#' 
#' ### Extensions of the `dataframe` class
#' 
#' As already mentioned in the previous chapter, o
#' 
#' For example, it is very common in research to w
#' 
#' See the following example, where we represent t
#' 
## ---- tidy=FALSE---------------------------------------------------------
# load pkg
library(xts)

# set ticker symbols as a vector
ticker <- c('AAP', 'COG', 'BLK', 'CAM')

# set a date vector
date <- as.Date(c("2010-01-04", "2010-01-05", "2010-01-06", 
                  "2010-01-07", "2010-01-08"))

# set prices as  matrix					  
price.mat <- matrix(c(40.38,  40.13,  40.49,  40.48,  40.63,
                      46.23,  46.16,  45.97,  45.56,  45.45,
                      238.58, 239.61, 234.66, 237.25, 238.91,
                      43.43,  43.95,  44.25,  44.5,   44.86),
                    nrow = length(date))

# set xts object
my.xts <- xts(price.mat, order.by = date)

# set colnames
colnames(my.xts) <- ticker

# print it
print(my.xts)

# show its class
class(my.xts)

#' 
#' In the creation of the `xts` object, notice how
#' 
#' The previous code can give the impression that 
#' 
## ------------------------------------------------------------------------
# set number of time periods
N <- 500

# create matrix with data
my.mat <- matrix(c(seq(1, N), seq(N, 1)), nrow=N)

# set xts object
my.xts <- xts(my.mat, order.by = as.Date('2016-01-01')+1:N)

# apply mean function for each weel
my.xts.weekly.mean <- apply.weekly(my.xts, mean)

# print result
print(head(my.xts.weekly.mean))

#' 
#' In finance, these time aggregations with `xts` 
#' 
#' Package `xts` is not alone as an alternative to
#' 
#' It is important to note that most of the basic 
#' 
#' 
#' ### Other useful functions for handling `datafr
#' 
#' * **head** Returns the first `n` rows of a `dat
#' 
## ------------------------------------------------------------------------
# set df
my.df <- data.frame(col1 = 1:5000, col2 = rep('a', 5000))

# print its first 5 rows
print(head(my.df, 5))

#' 
#' * **tail** - Returns the last `n` rows of a `da
#' 
## ------------------------------------------------------------------------
# print its last 5 rows
tail(my.df, 5)

#' 
#' * **complete.cases** - Returns a logical vector
#' 
## ------------------------------------------------------------------------
# create df
my.df <- data.frame(x = c(1:5, NA, 10),
                    y = c(5:10, NA))

# show df
print(my.df)

# print logical test of complete.cases
print(complete.cases(my.df))

# print all rows where there is at least one NA
print(which(!complete.cases(my.df)))

#' 
#' * **na.omit** - Returns a `dataframe` without t
#' 
## ------------------------------------------------------------------------
print(na.omit(my.df))

#' 
#' * **unique** - Returns a `dataframe` where all 
#' 
## ------------------------------------------------------------------------
# set df
my.df <- data.frame(col1 = c(1,1,2,3,3,4,5), 
                    col2 = c('A','A','A','C','C','B','D'))

# print it					
print(my.df)

# print unique df
print(unique(my.df))

#' 
#' 