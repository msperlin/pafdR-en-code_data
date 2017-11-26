#' # Basic Object Classes {#BasicObjects}
#' 
#' **In R, everything is an object**. Previously, 
#' 
#' But, the `character` class has other properties
#' 
#' An important distinction here is the difference
#' 
#' A simpler way to organize our data is to create
#' 
#' 
#' ## `Numeric` Objects
#' 
#' Objects of type `numeric` represent one or more
#' 
#' 
#' ### Creating and Manipulating `numeric` Objects
#' 
#' The creation and manipulation of `numeric` obje
#' 
## ------------------------------------------------------------------------
# create numeric vectors
x <- 1:5
y <- 2:6

# print sum
print(x+y)

# print multiplication
print(x*y)

# print division
print(x/y)

# print exponentiation
print(x^y)

#' 
#' A difference between R and other programming la
#' 
## ------------------------------------------------------------------------
# set x with 4 elements and y with 2
x <- 1:4
y <- 2:1

# print multiplication
print(x + y)

#' 
#' Here, the result of `x + y` is equivalent to `1
#' 
## ----warning=TRUE, error=TRUE--------------------------------------------
# set x = 4 elements and y with 3
x <- c(1, 2, 3, 4)
y <- c(1, 2, 3)

# print sum (recycling rule)
print(x +y)

#' 
#' The first three elements of `x` were summed to 
#' 
#' One great thing about R is that elements of a `
#' 
## ------------------------------------------------------------------------
# create named vector
x <- c(item1 = 10, item2 = 14, item3 = 9, item4 = 2)

# print it
print(x)

#' 
#' Notice how we used symbol `=` to set the names 
#' 
## ------------------------------------------------------------------------
# create unnamed vector
x <- c(10, 14, 9, 2)

# set names of elements
names(x) <- c('item1', 'item2', 'item3', 'item4')

# print it
print(x)

#' 
#' Notice how the use of function `names` works di
#' 
#' Empty `numeric` vectors can also be created. So
#' 
## ------------------------------------------------------------------------
# create empty numeric vector of length 10
my.x <- numeric(length = 10)

# print it
print(my.x)

#' 
#' As you can see, when using `numeric(length = 10
#' 
#' 
#' ### Creating a `numeric` Sequence
#' 
#' In R, you have two ways to create a sequence of
#' 
#' However, using operator `:` limits the possibil
#' 
## ------------------------------------------------------------------------
# create sequence with seq
my.seq <- seq(from = -10, to = 10, by = 2)

# print it
print(my.seq)

#' 
#' Another interesting feature of function `seq` i
#' 
## ------------------------------------------------------------------------
# create sequence with defined number of elements
my.seq <- seq(from = 0, to = 10, length.out = 20)

# print it
print(my.seq)

#' 
#' Observe how the final size of `my.seq` is exact
#' 
#' 
#' ### Creating Vectors with Repeated Elements
#' 
#' Another way to create `numeric` vectors is usin
#' 
## ------------------------------------------------------------------------
# created a vector with repeated elements
my.x <- rep(x = c(1, 2), times = 3)

# print it
print(my.x)

#' 
#' 
#' ### Creating Vectors with Random Numbers
#' 
#' Some applications in finance and economics requ
#' 
#' Function `rnorm` generates random numbers from 
#' 
## ------------------------------------------------------------------------
# generate 10 random numbers from a Normal distribution
my.rnd.vec <- rnorm(n = 10, mean = 0, sd = 1)

# print it
print(my.rnd.vec)

#' 
#' In the previous code, we generated ten random n
#' 
#' Function `runif` generates random values unifor
#' 
## ------------------------------------------------------------------------
# create a random vector with minimum and maximum
my.rnd.vec <- runif(n = 10, min = -5, max = 5)

# print it
print(my.rnd.vec)

#' 
#' Note that both functions, `rnorm` and `runif`, 
#' 
## ------------------------------------------------------------------------
# create sequence
my.vec <- seq(from = 0, to = 25, by=5)

# sample sequence
my.rnd.vec <- sample(my.vec)

# print it
print(my.rnd.vec)

#' 
#' Function `sample` also allows the random select
#' 
## ------------------------------------------------------------------------
# sample one element of my.vec
my.rnd.vec <- sample(my.vec, size = 1)

# print it
print(my.rnd.vec)

#' 
#' If we wanted two random elements from `my.rnd.v
#' 
## ------------------------------------------------------------------------
# sample one element of my.vec
my.rnd.vec <- sample(my.vec, size = 2)

# print it
print(my.rnd.vec)

#' 
#' It is also possible to select values from a sma
#' 
## ------------------------------------------------------------------------
# create vector
my.vec <- c(5, 10, 15)

# sample
my.rnd.vec <- sample(x = my.vec, size = 10, replace = TRUE)
print(my.rnd.vec)

#' 
#' Another important feature of `sample` is it wor
#' 
## ------------------------------------------------------------------------
# example of sample with characters
print(sample(c('elem 1','elem 2','elem 3'), 1))

#' 
#' At this point, it is important to acknowledge t
#' 
#' However, you can explicitly select set the plac
#' 
## ------------------------------------------------------------------------
# set seed with integer 10
set.seed(seed = 10)

# create and print "random" vectors
my.rnd.vec.1 <- runif(5)
print(my.rnd.vec.1)

my.rnd.vec.2 <- runif(5)
print(my.rnd.vec.2)

#' 
#' In the previous code, the value of `set.seed` i
#' 
#' 
#' ### Accessing the Elements of a `numeric` Vecto
#' 
#' As mentioned in the previous chapter, all eleme
#' 
## ------------------------------------------------------------------------
# set vector
x <- c(-1, 4, -9, 2)

# get first element
first.elem.x <- x[1]

# print it
print(first.elem.x)

#' 
#' The same notation is used to extract parts of a
#' 
## ------------------------------------------------------------------------
# sub-vector of x
sub.x <- x[1:2]

# print it
print(sub.x)

#' 
#' To access named elements of a numeric array, si
#' 
## ------------------------------------------------------------------------
# set named vector
x <- c(item1 = 10, item2 = 14, item3 = -9, item4 = -2)

# access elements by name
print(x['item2'])
print(x[c('item2','item4')])

#' 
#' We can also access the elements of a numerical 
#' 
## ------------------------------------------------------------------------
# find all values of x higher than zero
print(x[x > 0])

#' 
#' The selection of elements from a vector, accord
#' 
#' 
#' ### Modifying and Removing Elements of a `numer
#' 
#' The modification of a vector is very simple. Ju
#' 
## ------------------------------------------------------------------------
# set vector
my.x <- 1:4

# modify first element to 5
my.x[1] <- 5

# print result
print(my.x)

#' 
#' This modification can also be performed block-w
#' 
## ------------------------------------------------------------------------
# set vector 
my.x <- 0:5

# set the first three elements to 5
my.x[1:3] <- 5

# print result
print(my.x)

#' 
#' Using conditions to change values in a vector i
#' 
## ------------------------------------------------------------------------
# set vector 
my.x <- -5:5

# set any value lower than 2 to 0
my.x[my.x<2] <- 0

# print result
print(my.x)

#' 
#' The removal of elements of a vector is carried 
#' 
## ------------------------------------------------------------------------
# create vector
my.x <- -5:5

# remove first and second element of my.x
my.x <- my.x[-(1:2)]

# show result
print(my.x)

#' 
#' Notice how using negative index simply returns 
#' 
#' 
#' ### Creating Groups from a `numeric` Vector
#' 
#' In some situations in data analysis, you'll nee
#' 
#' In R, the function used to create intervals fro
#' 
## ------------------------------------------------------------------------
# set random vector
my.x <- rnorm(10)

# create groups with 5 breaks
my.cut <- cut(x = my.x, breaks = 5)

# print it!
print(my.cut)

#' 
#' Note that the names in `my.cut` are defined by 
#' 
#' With the `cut` function, you can also define cu
#' 
## ------------------------------------------------------------------------
# create random vector
my.x <- rnorm(10)

# define breaks manually
my.breaks <- c(min(my.x)-1, -1, 1, max(my.x)+1)

# define labels manually
my.labels <- c('Low','Normal', 'High')

# create group from numerical vector
my.cut <- cut(x = my.x, breaks = my.breaks, labels = my.labels)

# print both!
print(my.x)
print(my.cut)

#' 
#' Notice that, in this example of creating a grou
#' 
#' 
#' ### Other Functions for Manipulating Numerical 
#' 
#' * **as.numeric** - Converts an object to the `n
#' 
## ------------------------------------------------------------------------
# create character object
my.text <- c('1', '2', '3')

# convert to numeric
my.x <- as.numeric(my.text)
print(my.x)
class(my.x)

#' 
#' * **sum** - Sums all elements of a `numeric` ve
#' 
## ------------------------------------------------------------------------
# set vector
my.x <- 1:50

# print its sum
print(sum(my.x))

#' 
#' * **prod** - Returns the product (multiplicatio
#' 	
## ------------------------------------------------------------------------
# set vector
my.x <- 1:10

# print prod
print(prod(my.x))

#' 	
#' * **max** - Returns the maximum value of a `num
#' 	
## ------------------------------------------------------------------------
# set vector
x <- c(10, 14, 9, 2)

# print max value
print(max(x))

#' 
#' * **min** - Returns the minimum value of a `num
#' 	
## ------------------------------------------------------------------------
# set vector
x <- c(12, 15, 9, 2)

# print min value
print(min(x))

#' 
#' * **which.max** - Returns the position of the m
#' 	
## ---- tidy=FALSE---------------------------------------------------------
# set vector
x <- c(100, 141, 9, 2)

# find position of maximum value
which.max.x <- which.max(x)

# show text output
cat(paste('The position of the maximum value of x is',
          which.max.x))
cat(' Its value is ', x[which.max.x])

#' 	
#' * **which.min** - Returns the position of the m
#' 	
## ---- tidy=FALSE---------------------------------------------------------
# set vector
x <- c(10, 14, 9, 2)

# find min value of x
which.min.x <- which.min(x)
cat(paste('The position of the minimum value of x is ', 
          which.min.x))

#' 
#' * **sort** - Returns a sorted (ascending or des
#' 
## ------------------------------------------------------------------------
# set random numbers
x <- runif(5)

# sort ascending and print
print(sort(x, decreasing = FALSE))

# sort descending and print
print(sort(x, decreasing = TRUE))

#' 
#' * **cumsum** - Returns the cumulative sum of th
#' 
## ------------------------------------------------------------------------
# set vector
my.x <- 1:25

# print cumsum
print(cumsum(my.x))

#' 	
#' 
#' * **cumprod** - Returns the cumulative product 
#' 
## ------------------------------------------------------------------------
# set vector
my.x <- 1:10

# print cumprod
print(cumprod(my.x))

#' 
#' * **unique** - Returns all unique values of a n
#' 
## ------------------------------------------------------------------------
# set vector
my.x <- c(1,1,2,3,3,5)

# print unique values
print(unique(my.x))

#' 
#' 	
#' ## `Character` Objects
#' 
#' The `character` class, or simply text class, is
#' 
#' R has several features that facilitate the crea
#' 
## ------------------------------------------------------------------------
library(stringr)

#' 
#' 
#' ### Creating a Simple `character` Object
#' 
#' In R, every `character` object is created by en
#' 
## ------------------------------------------------------------------------
my.tickers <- c('MMM', 'FB', 'ICE')
print(my.tickers)

#' 
#' We can confirm the class of the created object 
#' 
## ------------------------------------------------------------------------
class(my.tickers)

#' 
#' 
#' ### Creating Structured `character` Objects
#' 
#' In some data analysis situations, it will be re
#' 
#' To create a text vector with the junction of te
#' 
## ------------------------------------------------------------------------
# create sequence
my.seq <- 1:20

# create character
my.text <- 'ticker'

# paste objects together (with space)
my.char <- paste(my.text, my.seq)
print(my.char)

# paste objects together (without space)
my.char <- paste0(my.text, my.seq)
print(my.char)

#' 
#' We can do the same procedure with text vectors:
#' 
## ------------------------------------------------------------------------
# set character value
my.x <- 'My name is'

# set character vector
my.names <- c('Marcelo', 'Ricardo', 'Tarcizio')

# paste and print
print(paste(my.x, my.names))

#' 
#' In `stringr`, the equivalent function for pasti
#' 
## ------------------------------------------------------------------------
# paste and print
print(str_c(my.x, my.names))

#' 
#' Another possibility of building structured text
#' 
## ------------------------------------------------------------------------
# replicate with strrep
my.char <- strrep(x = 'abc', times = 5)
print(my.char)

# replicate with stringr::str_dup
print(str_dup(my.char, 2))

#' 
#' ### `character` Constants
#' 
#' R also allows direct access to all letters of t
#' 
## ------------------------------------------------------------------------
# print all letters in alphabet (no cap)
print(letters)

#' 
## ------------------------------------------------------------------------
# print all letters in alphabet (WITH CAP)
print(LETTERS)

#' 
#' Note that, in both cases, `letters` and `LETTER
#' 	
## ------------------------------------------------------------------------
# print abreviation and full names of months
print(month.abb)	
print(month.name)	

#' 
#' ### Selecting Characters of a Text Object
#' 
#' A common beginner's mistake  is to select chara
#' 
## ------------------------------------------------------------------------
# set char object
my.char <- 'ABCDE'

# print its second element (WRONG - RESULT is NA)
print(my.char[2])

#' 
#' The return value `NA` indicates the second elem
#' 
## ------------------------------------------------------------------------
print(my.char[1])

#' 
#' The result is simply the _ABCDE_ text, located 
#' 
## ------------------------------------------------------------------------
# print third and fourth characters with base function
my.substr <- substr(x = my.char, start = 3, stop = 4)
print(my.substr)

# print third and fourth characters with stringr function
my.substr <- str_sub(string = my.char, start = 3, end = 4)
print(my.substr)

#' 
#' These functions also work for atomic vectors. L
#' 
## ---- tidy=FALSE---------------------------------------------------------
# build char vec
my.char.vec <- paste0(c('123','231','321'), 
                      ' - other ignorable text') 
print(my.char.vec)

#' 
#' Here, we only want the information in the first
#' 
## ------------------------------------------------------------------------
# get ids with substr
ids.vec <- substr(my.char.vec, 1, 3)
print(ids.vec)

# get ids with stringr::str_sub
ids.vec <- str_sub(my.char.vec, 1, 3)

#' 
#' Vector operations are common in R. Almost anyth
#' 
#' 
#' ### Finding and Replacing Characters of a Text
#' 
#' A useful operation in handling texts is to loca
#' 
#' Before moving to the examples, it is important 
#' 
#' The following example shows how to find the _D_
#' 
## ------------------------------------------------------------------------
# set character object
my.char <- 'ABCDEF-ABCDEF-ABC'

# find position of FIRST 'D' using regexpr
pos <- regexpr(pattern = 'D', text = my.char, fixed = TRUE) 
print(pos)

# find position of 'D' using str_locate
pos <- str_locate(my.char, fixed('D'))
print(pos)

#' 
#' Note the `regexp` and `str_locate` function ret
#' 
## ------------------------------------------------------------------------
# set object
my.char <- 'ABCDEF-ABCDEF-ABC'

# find position of ALL 'D' using regexpr
pos <- gregexpr(pattern = 'D', text = my.char, fixed = TRUE) 
print(pos)

# find position of ALL 'D' using str_locate_all
pos <- str_locate_all(my.char, fixed('D'))
print(pos)


#' 
#' To replace characters in a text, use functions 
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set char object
my.char <- 'ABCDEF-ABCDEF-ABC'

# substitute the FIRST 'ABC' for 'XXX' with sub
my.char <- sub(x = my.char, 
               pattern = 'ABC', 
			   replacement = 'XXX')
print(my.char)

# substitute the FIRST 'ABC' for 'XXX' with str_replace
my.char <- str_replace(string = my.char, 
                       pattern = 'ABC', 
					   replacement = 'XXX')
print(my.char)

#' 
#' And now we do a global substitution of characte
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set char object
my.char <- 'ABCDEF-ABCDEF-ABC'

# substitute all 'ABC' for 'XXX'  with gsub
my.char <- gsub(x = my.char, 
                pattern = 'ABC', 
				replacement = 'XXX')
				
print(my.char)

# substitute ALL 'ABC' for 'XXX' with str_replace_all

# set my.char again
my.char <- 'ABCDEF-ABCDEF-ABC'

my.char <- str_replace_all(string = my.char, 
                           pattern = 'ABC', 
						   replacement = 'XXX')
print(my.char)

#' 
#' Again, it is worth pointing out that the operat
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set char object
my.char <- c('ABCDEF','DBCFE','ABC')

# create an example of vector
my.char.vec <- paste(sample(my.char, 5, replace = T),
                     sample(my.char, 5, replace = T), 
					 sep = ' - ')

# show it
print(my.char.vec)

# substitute all occurrences of 'ABC'
my.char.vec <- str_replace_all(string = my.char.vec, 
                               pattern = 'ABC', 
							   replacement = 'XXX')

# print result
print(my.char.vec)

#' 
#' ### Splitting Text
#' 
#' In some situations of analyzing text data, it w
#' 
## ------------------------------------------------------------------------
# set char
my.char <- 'ABCXABCXBCD'

# split it based on 'X' and using strsplit
split.char <- strsplit(my.char, 'X')

# print result
print(split.char)

# split it based on 'X' and using stringr::str_split
split.char <- str_split(my.char, 'X')

# print result
print(split.char)


#' 
#' The output of this function is an object of typ
#' 
## ------------------------------------------------------------------------
print(split.char[[1]][3])

#' 
#' To visualize an example of split in character v
#' 
## ------------------------------------------------------------------------
# set char
my.char.vec <- c('ABCDEF','DBCFE','ABFC','ACD')

# split it based on 'B' and using stringr::strsplit
split.char <- strsplit(my.char.vec, 'B')

# print result
print(split.char)

#' 
#' Notice how, again, an object of type `list` is 
#' 
#' 
#' ### Finding the Number of Characters in a Text
#' 
#' To find out the number of characters in a `char
#' 
## ------------------------------------------------------------------------
# set char
my.char <- 'abcdef'

# print number of characters using nchar
print(nchar(my.char))

# print number of characters using stringr::str_length
print(str_length(my.char))

#' 
#' And now an example with vectors.
#' 
## ------------------------------------------------------------------------
#set char
my.char <- c('a', 'ab', 'abc')

# print number of characters using nchar
print(nchar(my.char))

# print number of characters using stringr::str_length
print(str_length(my.char))

#' 
#' ### Generating Combinations of Text
#' 
#' One useful trick in R is to use functions `oute
#' 
## ------------------------------------------------------------------------
# set char vecs
my.vec.1 <- c('a','b')
my.vec.2 <- c('A','B')

# combine in matrix
comb.mat <- outer(my.vec.1, my.vec.2, paste,sep='-')

# print it!
print(comb.mat)

#' 
#' The output of `outer` is a `matrix` type of obj
#' 
## ------------------------------------------------------------------------
print(as.character(comb.mat))

#' 
#' Another way to reach the same objective is usin
#' 
## ------------------------------------------------------------------------
# create df with all combinations
my.df <- expand.grid(my.vec.1, my.vec.2)

# print df
print(my.df)

# paste columns together
my.comb.vec <- paste(my.df$Var1, my.df$Var2, sep='-')

# print result
print(my.comb.vec)

#' 
#' Here, we used function `expand.grid` to create 
#' 
#' 
#' ### Encoding of `character` Objects
#' 
#' Every `character` object in R is encoded in a p
#' 
#' Let's explore an example. Here, we will import 
#' 
## ------------------------------------------------------------------------
# read text file 
my.char <- readLines('data/FileWithLatinChar.txt')

# print it
print(my.char)

#' 
#' The original content of the file is a text in P
#' 
## ---- eval=FALSE, tidy=FALSE---------------------------------------------
## # read text file with utf-8
## my.char <- readLines('data/FileWithLatinChar.txt',
##                      encoding = 'UTF-8')

#' 
#' The output in `my.char` should now be properly 
#' 
#' As for objects available in the environment, yo
#' 
## ------------------------------------------------------------------------
# read text file
my.char <- readLines('data/FileWithLatinChar.txt')

# show its encoding
print(Encoding(my.char))

# change encoding
Encoding(my.char) <- 'UTF-8'

# show its encoding
print(Encoding(my.char))

#' 
#' After reading the contents of `"data/FileWithLa
#' 
#' 
#' ### Other Functions for Manipulating `character
#' 
#' * **tolower** and **stringr::str_to_lower** - C
#' 
## ------------------------------------------------------------------------
print(tolower('ABC'))

print(stringr::str_to_lower('ABC'))

#' 
#' * **toupper** and **stringr::str_to_upper** - C
#' 
## ------------------------------------------------------------------------
print(toupper('abc'))

print(stringr::str_to_upper('abc'))

#' 
#' 
#' ## `Factor` Objects
#' 
#' Object class `factor` is used to represent grou
#' 
#' The `factor` class offers a special object to d
#' 
#' 
#' ### Creating `factors`
#' 
#' The creation of factors is accomplished with fu
#' 
## ------------------------------------------------------------------------
# create factor
my.factor <- factor(c('M','F','M','M','F'))

# print it
print(my.factor)

#' 
#' Notice that, in the previous example, the prese
#' 
## ------------------------------------------------------------------------
# create factor with 3 levels
my.factor <- factor(c('M','F','M','M','F','ND'))

# print factor
print(my.factor)

#' 
#' Here, we also have the `ND` (not defined) group
#' 
#' An important point about creating factors is th
#' 
## ------------------------------------------------------------------------
# set factors with 1 level
my.status <- factor(c('Single', 'Single', 'Single'))

# print it
print(my.status)

#' 
#' On occasion, the data in `my.status` only shows
#' 
## ---- tidy=FALSE---------------------------------------------------------
my.status <- factor(c('Single', 'Single', 'Single'), 
                    levels = c('Single', 'Married'))
print(my.status)

#' 
#' 
#' ### Modifying `factors`
#' 
#' An important point about `factor` type of objec
#' 
## ----warning=TRUE--------------------------------------------------------
# set factor
my.factor <- factor(c('a', 'b', 'a', 'b'))

# change first element of a factor to 'c'
my.factor[1] <- 'c'

# print result
print(my.factor)

#' 
#' As we expected, the first element of `my.factor
#' 
## ------------------------------------------------------------------------
# set factor
my.factor <- factor(c('a', 'b', 'a', 'b'))

# change factor to character
my.char <- as.character(my.factor)

# change first element
my.char[1] <- 'c'

# mutate it back to class factor
my.factor <- factor(my.char)

# show result
print(my.factor)

#' 
#' Using these steps, we have the desired result i
#' 
#' 
#' ### Converting `factors` to Other Classes
#' 
#' Attention is required when converting a `factor
#' 
## ------------------------------------------------------------------------
# create factor 
my.char <-factor(c('a', 'b', 'c'))

# convert and print
print(as.character(my.char))

#' 
#' However, when the same procedure is performed f
#' 
## ------------------------------------------------------------------------
# set factor
my.values <- factor(5:10)

# convert to numeric (WRONG)
print(as.numeric(my.values))

#' 
#' As you can see, all elements in `my.values` wer
#' 
## ------------------------------------------------------------------------
# converting factors to character and then to numeric
print(as.numeric(as.character(my.values)))

#' 
#' As we can see, now we got the result we wanted.
#' 
#' 
#' ### Creating Contingency Tables
#' 
#' After creating a factor, we can find the number
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create factor
my.factor <- factor(sample(c('Pref', 'Ord'), 
                             size = 20, 
							 replace = TRUE))
							 
# print it 
print(my.factor)

# print contingency table
print(table(my.factor))

#' 
#' A more advanced usage of function `table` is to
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set factors
my.factor.1 <- factor(sample(c('Pref', 'Ord'), 
                             size = 20, 
							 replace = TRUE))
							 
my.factor.2 <- factor(sample(paste('Grupo', 1:3), 
                             size = 20, 
							 replace = TRUE))

# print contingency table with two factors
print(table(my.factor.1, my.factor.2))

#' 
#' The previously created table shows the number o
#' 
#' 
#' ### Other Functions for Manipulating `factors`
#' 
#' * **levels** - Returns the `Levels` an object o
#' 
## ------------------------------------------------------------------------
# set factor
my.factor <- factor(c('A', 'A', 'B', 'C', 'B'))

# print levels
print(levels(my.factor))

#' 
#' * **as.factor** - Transforms an object to the c
#' 	
## ------------------------------------------------------------------------
# set char
my.y <- c('a','b', 'c', 'c', 'a')

# mutate to factor
my.factor <- as.factor(my.y)

# print it
print(my.factor)

#' 
#' 
#' * **split** - Based on a grouping variable and 
#' 	
## ------------------------------------------------------------------------
# set factor and numeric
my.factor <- factor(c('A','B','C','C','C','B'))
my.x <- 1:length(my.factor)

# split numeric vector into a list, based on factor
my.l <- split(x = my.x, f = my.factor)

print(my.l)

#' 	
#' 
#' ## `Logical` Objects 
#' 
#' Logical tests are at the heart of R. In one lin
#' 
#' 
#' ### Creating `logical` Objects
#' 
#' In a sequence from 1 to 10, we can check what e
#' 
## ------------------------------------------------------------------------
# set numerical
my.x <- 1:10

# print a logical test
print(my.x > 5)

# print position of elements from logical test
print(which(my.x > 5))

#' 
#' In the previous example, function `which` retur
#' 
#' To perform equality tests, simply use the equal
#' 
## ------------------------------------------------------------------------
# create char
my.char <- rep(c('abc','bcd'),5)

# print its contents
print(my.char)

# print logical test
print(my.char=='abc')

#' 
#' For an inequality test, use symbol `!=`, as sho
#' 
## ------------------------------------------------------------------------
# print inequality test
print(my.char!='abc')

#' 
#' It is also possible to test multiple logical co
#' 
## ------------------------------------------------------------------------
my.x <- 1:10

# print logical for values higher than 4 and lower than 7
print((my.x > 4)&(my.x < 7) )

# print the actual values
idx <- which( (my.x > 4)&(my.x < 7) )
print(my.x[idx])

#' 
#' For non-simultaneous conditions, i.e., the occu
#' 
## ------------------------------------------------------------------------
# location of elements higher than 7 or lower than 4
idx <- which( (my.x > 7)|(my.x < 4) )

# print elements from previous condition
print(my.x[idx])

#' 
#' Be aware that, in both cases, we used parenthes
#' 
#' 	
#' ## Date and Time Objects
#' 
#' The representation and manipulation of dates is
#' 
#' 
#' ### Creating Simple Dates
#' 
#' In R, several classes can represent dates. The 
#' 
## ------------------------------------------------------------------------
# set Date object
my.date <- as.Date('2016-06-24')

# check its class
class(my.date)

# print it
print(my.date)

#' 
#' Notice, in the previous example, dates are repr
#' 
## ------------------------------------------------------------------------
# set Date from dd/mm/yyyy 
my.date <- as.Date('24/06/2016')

# print result (WRONG)
print(my.date)

#' 
#' The date of `r my.date` is wrong! To fix this f
#' 
## ------------------------------------------------------------------------
# set Date from dd/mm/yyyy with the definition of format
my.date <- as.Date('24/06/2016', format = '%d/%m/%Y')

# print result (CORRECT)
print(my.date)

#' 
#' The symbols used in _input_ `format`, such as `
#' 
#' 
#' | Symbol |          Description   |Example |
#' |:------:|:----------------------:|:------:|
#' |%d      |day of month (decimal)  |0       |
#' |%m      |month (decimal)         |12      |
#' |%b      |month (abbreviation)    |Apr     |
#' |%B      |month (complete name)   |April   |
#' |%y      |year (2 digits)         |16      |
#' |%Y      |month (4 digits)        |2016    |
#' 
#' By using the previous table, you'll be able to 
#' 
#' 
#' ### Creating a Sequence of `Dates`
#' 
#' An interesting aspect of objects `Date` is they
#' 
## ------------------------------------------------------------------------
# create date
my.date <- as.Date('2016-06-24')

# find next day
my.date.2 <- my.date + 1

# print result
print(my.date.2)

#'  
#' This property also works with vectors, facilita
#' 
## ------------------------------------------------------------------------
# create a sequence of Dates
my.date.vec <- my.date + 0:15

# print it
print(my.date.vec)

#' 
#' A more customizable way for creating `Date` seq
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set first and last Date
my.date.1 <- as.Date('2017-03-07')
my.date.2 <- as.Date('2017-03-20')

# set sequence
my.vec.date <- seq(from = my.date.1, 
                   to = my.date.2, 
				   by = '2 days')

# print result
print(my.vec.date)

#' 
#' Likewise, if we wanted a sequence of dates cont
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set first and last Date
my.date.1 <- as.Date('2017-03-07')
my.date.2 <- as.Date('2017-10-20')

# set sequence
my.vec.date <- seq(from = my.date.1, 
                   to = my.date.2, 
				   by = '1 month')

# print result
print(my.vec.date)

#' 
#' Another way to use function `seq` is by setting
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set dates
my.date.1 <- as.Date('2016-06-27')
my.date.2 <- as.Date('2016-07-27')

# set sequence with 10 elements
my.vec.date <- seq(from = my.date.1, 
                   to = my.date.2, 
				   length.out = 10)

# print result				   
print(my.vec.date)

#' 
#' 
#' ### Operations with `Dates`
#' 
#' We can calculate difference of days between two
#' 
## ------------------------------------------------------------------------
# set dates
my.date.1 <- as.Date('2015-06-24')
my.date.2 <- as.Date('2016-06-24')

# calculate difference
diff.date <- my.date.2 - my.date.1

# print result
print(diff.date)

#' 
#' The output of the subtraction operation is an o
#' 
## ------------------------------------------------------------------------
# print difference of days as numerical value
print(diff.date[[1]])

#' 
#' Going further, we can test whether a date is mo
#' 
## ------------------------------------------------------------------------
# set date and vector
my.date.1 <- as.Date('2016-06-20')
my.date.vec <- as.Date('2016-06-20') + seq(-5,5)

# test which elements of my.date.vec are older than my.date.1
my.test <- my.date.vec > my.date.1

# print result
print(my.test)

#' 
#' The previous operation is useful when trying to
#' 
## ------------------------------------------------------------------------
# set first and last dates
first.date <- as.Date('2016-06-01')
last.date <- as.Date('2016-06-15')

# create a vector of dates and a vector of "fake" prices
my.date.vec <- as.Date('2016-05-25') + seq(0,30)
my.prices <- seq(1,10, length.out = length(my.date.vec))

# print vectors 
print(my.prices)
print(my.date.vec)

# find dates that are between the first and last date
my.idx <- (my.date.vec >= first.date) & (my.date.vec <= last.date)

# use index to select prices
my.prices <- my.prices[my.idx]

# print result
print(my.prices)

#' 
#' In the previous code, object `my.prices` will o
#' 
#' 
#' ### Dealing with Time
#' 
#' Using the `Date` class is sufficient when deali
#' 
#' In R, one of the classes used for this purpose 
#' 
## ------------------------------------------------------------------------
my.timedate <- as.POSIXlt('2016-01-01 16:00:00')
print(attributes(my.timedate))

#' 
#' As you can see, it stores the hour, minutes, we
#' 
## ------------------------------------------------------------------------
print(my.timedate[['hour']])

#' 
#' Since computer memory is not limited these days
#' 
#' For both `datetime` objects, `POSIXct` and `POS
#' 
## ------------------------------------------------------------------------
# creating a POSIXlt object
my.timedate <- as.POSIXlt('2016-01-01 16:00:00')

# print result
print(my.timedate)

#' 
#' When creating a `POSIXlt` object, the time zone
#' 
## ------------------------------------------------------------------------
# creating a POSIXlt object with custom timezone
my.timedate.tz <- as.POSIXlt('2016-01-01 16:00:00', tz = 'GMT')

# print it
print(my.timedate.tz)

#' 
#' An important note in the case of `POSIXlt` and 
#' 
## ------------------------------------------------------------------------
# Adding values (seconds) to a POSIXlt object and printing it
print(my.timedate.tz + 30)

#' 
#' In the same way as objects of class `Date`, the
#' 
#' 
#' | Symbol |           Description    | Example |
#' |:------:|:------------------------:|:-------:|
#' | %H     | Hour (decimal, 24 hours) | 23      |
#' | %I     | Hour (decimal, 12 hours) | 11      |
#' | %M     | Minutes (decimal, 0-59)  | 12      |
#' | %p     | AM/PM indicator          | AM      |
#' | %S     | Seconds (decimal, 0-59)  | 50      |
#' 
#' 
#' 
#' ### Customizing the Output Format of Dates and 
#' 
#' The basic notation for representing dates and `
#' 
#' To format a date, use the `format` function. It
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create vector of dates
my.dates <- seq(from = as.Date('2016-01-01'), 
                to = as.Date('2016-01-15'), 
				by = '1 day')
				
# change format				
my.dates.brformat <- format(my.dates, '%d/%m/%Y')

# print result
print(my.dates.brformat)

#' 
#' The same procedure can be performed for `POSIXl
#' 
## ------------------------------------------------------------------------
# create vector of date-time
my.datetime <- as.POSIXlt('2016-01-01 12:00:00') + seq(0,560,60)

# change to Brazilian format
my.dates.brformat <- format(my.datetime, '%d/%m/%Y %H:%M:%S')

# print result
print(my.dates.brformat)

#' 
#' One can also customize for very specific format
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set custom format
my.dates.myformat <- format(my.dates, 
                            'Year=%Y | Month=%m | Day=%d')

# print result
print(my.dates.myformat)

#' 
#' Using function `format` is also very helpful wh
#' 
## ------------------------------------------------------------------------
# create vector of date-time
my.datetime <- seq(from = as.POSIXlt('2016-01-01 12:00:00'), 
                   to = as.POSIXlt('2016-01-01 18:00:00'), 
                   by = '1 hour')

# get hours from POSIXlt
my.hours <- format(my.datetime, '%H')

# print result
print(my.hours)

#' 
#' Likewise, by using symbols `%M` and `%S`, we co
#'  
#' 
#' ### Find the Current Date and Time
#' 
#' R has specific functions that allow the user to
#' 
#' To find the the present day, use function `Sys.
#' 
## ------------------------------------------------------------------------
# get today
my.day <- Sys.Date()

# print it
print(my.day)

#' 
#' To find the current date and time, we use funct
#' 
## ------------------------------------------------------------------------
# get time!
print(Sys.time())

#' 
#' Going further, based on these functions, we can
#' 
## ------------------------------------------------------------------------
# example of log message
my.str <- paste0('This code was executed in ', Sys.time())

# print it
print(my.str)

#' 
#' 
#' ### Other Functions for Manipulating Dates and 
#' 
#' * **weekdays** - Returns the day of the week fr
#' 	
## ---- tidy=FALSE---------------------------------------------------------
# set date vector
my.dates <- seq(from = as.Date('2016-01-01'), 
                to = as.Date('2016-01-5'), 
				by = '1 day')

# find corresponding weekdays
my.weekdays <- weekdays(my.dates)

# print it
print(my.weekdays)

#' 
#' * **months** - Returns the month of one or more
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create date vector
my.dates <- seq(from = as.Date('2016-01-01'), 
                to = as.Date('2016-12-31'), 
				by = '1 month')

# find months
my.months <- months(my.dates)

# print result
print(my.months)

#' 
#' * **quarters** - Returns the location of one or
#' 
## ------------------------------------------------------------------------
# get quartiles of the year
my.quarters <- quarters(my.dates)
print(my.quarters)

#' 	
#' * **OlsonNames** - Returns an array with the ti
#' 	
## ------------------------------------------------------------------------
# get possible timezones
possible.tz <- OlsonNames()

# print it
print(possible.tz[1:5])

#' 
#' * **Sys.timezone** - Returns the current timezo
#' 	
## ------------------------------------------------------------------------
# get current timezone
print(Sys.timezone())

#' 	
#' * **cut** - Returns a factor by grouping dates 
#' 	
## ---- tidy=FALSE---------------------------------------------------------
# set example date vector
my.dates <- seq(from = as.Date('2016-01-01'), 
                to = as.Date('2016-03-01'), 
				by = '5 days')

# group vector based on monthly breaks
my.month.cut <- cut(x = my.dates, 
                    breaks = 'month', 
					labels = c('Jan', 'Fev', 'Mar'))

# print result
print(my.month.cut)

#' 
## ------------------------------------------------------------------------
# set example datetime vector
my.datetime <- as.POSIXlt('2016-01-01 12:00:00') + seq(0,250,15)

# set groups for each 30 seconds
my.cut <- cut(x = my.datetime, breaks = '30 secs')

# print result
print(my.cut)

#' 		
#' 		
#' ## Missing Data - `NA` (_Not available_)
#' 
#' One of the main innovations of R, with respect 
#' 
#' 
#' ### Defining `NA` Values
#' 
#' To define omissions in the dataset, use symbol 
#' 
## ------------------------------------------------------------------------
# a vector with NA
my.x <- c(1,2,NA, 4, 5)

# print it
print(my.x)

#' 
#' An important property, at this point, is that a
#' 
## ------------------------------------------------------------------------
# example of NA interacting with other objects
print(my.x + 1)

#' 
#' This property demands special attention if you 
#' 
## ------------------------------------------------------------------------
# set vector with NA
my.x <- c(1:5, NA, 5:10)

# print cumsum (NA after sixth element)
print(cumsum(my.x))

# print cumprod (NA after sixth element)
print(cumprod(my.x))

#' 
#' Therefore, when using functions `cumsum` and `c
#' 
#' 
#' ### Finding and Replacing `NA`
#' 
#' To find `NA` values, use function `is.na`: \ind
#' 
## ------------------------------------------------------------------------
# set vector with NA
my.x <- c(1:2, NA, 4:10)

# find location of NA
idx.na <- is.na(my.x)
print(idx.na)

#' 
#' To replace it, use indexing with the output of 
#' 
## ------------------------------------------------------------------------
# set vector
my.x <- c(1, NA, 3:4, NA)

# replace NA for 2
my.x[is.na(my.x)] <- 2

# print result
print(my.x)

#' 
#' Another way to remove `NA` values is to use fun
#' 
## ------------------------------------------------------------------------
# set vector
my.char <- c(letters[1:3], NA, letters[5:8])

# print it
print(my.char)

# use na.omit to remove NA
my.char <- na.omit(my.char)

# print result
print(my.char)

#' 
#' Although the type of object has been changed du
#' 
## ------------------------------------------------------------------------
# trying nchar on a na.omit object
print(nchar(my.char))

#' 
#' For other objects, however, this property may n
#' 
#' 
#' ### Other Useful Functions for Treating  `NA`
#' 
#' * **complete.cases** - Returns a logical vector
#' 	
## ------------------------------------------------------------------------
# create matrix
my.mat <- matrix(1:15, nrow = 5)

# set an NA value
my.mat[2,2] <- NA

# print index with rows without NA
print(complete.cases(my.mat))
