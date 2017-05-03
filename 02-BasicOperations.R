#' # Basic Operations in R {#basicoperations}
#' 
#' 
#' Before you start developing your code, you need
#' 
#' In this section, we will go through the initial
#' 
#' 
#' ## Working With R
#' 
#' The greatest difficulty a new user experiences 
#' 
#' While this visual and motor interaction format 
#' 
#' In the use of R, the ideal format of work is to
#' 
#' Like other software, R allows us to import data
#' 
#' 
#' ## Objects in R
#' 
#' **In R, everything is an object, and each type 
#' 
#' While we represent data as objects in R, a spec
#' 
## ------------------------------------------------------------------------
mean(1:5, na.rm = TRUE)

#' 
#' The _:_ symbol used above creates a sequence st
#' 
#' Functions are at the heart of R and we will ded
#' 
#'  
#' ## International and Local Formats
#' 
#' Before beginning to explain the use of R and RS
#' 
#' * **decimal:** Following  an international nota
#' 
#' * **Latin characters:** Due to its internationa
#' 
#' * **date format:** Dates in R are formatted acc
#' 
#' If you want to learn more about your local form
#' 
## ---- tidy=TRUE----------------------------------------------------------
Sys.localeconv()

#' 
#' The output of `Sys.localeconv()` shows how R in
#' 
## ---- eval=FALSE---------------------------------------------------------
## Sys.setlocale("LC_ALL", "English")

#' 
#' A note, however, is that you'll need to run thi
#' 
#' 	
#' ## Types of Files in R
#' 
#' Like any other programming platform, R has a fi
#' 
#' * **Files with the extension _.R _**: text file
#' 
#' * **Files with extension _.RData_**: files that
#' 
#' * **Files with extension _.Rmd_, _.md_ and _.Rn
#' 
#' * **Files with extension _.Rproj_**: contain fi
#' 
#' 
#' ## Explaining the RStudio Screen
#' 
#' After installing the two programs, R and RStudi
#'  
#' After opening RStudio, the resulting window sho
#' 
#' 
#' Note that RStudio automatically detected the in
#' 
#' If you do not see something like this on the sc
#' 
#' ```
#' R version 3.3.3 (2017-03-06) -- "Another Canoe"
#' Copyright (C) 2017 The R Foundation for Statist
#' Platform: x86_64-w64-mingw32/x64 (64-bit)
#' 
#' R is free software and comes with ABSOLUTELY NO
#' You are welcome to redistribute it under certai
#' Type 'license()' or 'licence()' for distributio
#' 
#' R is a collaborative project with many contribu
#' Type 'contributors()' for more information and
#' 'citation()' on how to cite R or R packages in 
#' 
#' Type 'demo()' for some demos, 'help()' for on-l
#' 'help.start()' for an HTML browser interface to
#' Type 'q()' to quit R.
#' ```
#' 
#' then R was not installed correctly. Repeat the 
#' 
#' As a first exercise, click _file_, _New File_, 
#' 
#' 
#' The main items/panels of the RStudio screen in 
#' 
#' * **Script Editor:** located on the left side a
#' 
#' * **R prompt:** located on the left side and be
#' 
#' * **Environment:** located on the top-right of 
#' 
#' * **Panel Packages:** shows the packages instal
#' 
#' As an introductory exercise, let's initialize t
#' 
## ------------------------------------------------------------------------
# set x
x <- 1

# set y
y <- 'My humble text'

#' 
#' If done correctly, notice that two objects appe
#' 
#' Now, let's show the values of `x` on the screen
#' 
## ------------------------------------------------------------------------
# print contents of x
print(x)

#' 
#' The `print` function is one of the main functio
#' 
## ------------------------------------------------------------------------
# print a sequence
print(50:100)

#' 
#' In this case, we use the `:` symbol in `50:100`
#' 
#' 
#' ## Running Scripts from RStudio
#' 
#' Now, let's combine all the previously typed cod
#' 
#' 
#' After pasting all the commands in the editor, s
#' 
#' In RStudio, there are some predefined and time-
#' 
#' 
#' Another very useful command is code execution b
#' 
#' Next, I highlight these and other RStudio short
#' 
#' * **control + shift + s**: executes (source) th
#' * **control + shift + enter**: executes the cur
#' * **control + enter**: executes the selected li
#' * **control + shift + b**: executes the codes f
#' * **control + shift + e**: executes the codes o
#' 
#' My suggestion is to use these shortcuts from da
#' 
#' If you want to run code in a _.R_ file within a
#' 
#' To run the support _script_, just call it with 
#' 
## ----eval=FALSE----------------------------------------------------------
## # execute import script
## source('import-data.R')

#' 
#' In this case, all code in `import-data.R` will 
#' 
#' 
#' ## Testing and Debugging Code
#' 
#' The development of code follows a cycle. At fir
#' 
#' When you are trying to find an error in a preex
#' 
#' 
#' 
#' This red circle indicate a code breakpoint that
#' 
#' 
## ----eval=FALSE----------------------------------------------------------
## # set x
## x <- 1
## 
## # set y
## browser()
## y <- 'My humble text'
## 
## # print contents of x
## print(x)

#' 
#' The practical result is the same as using RStud
#' 
#' 
#' ## Creating Simple Objects
#' 
#' One of the most basic and most used commands in
#' 
## ------------------------------------------------------------------------
# set x
x <- 123

# set x, y and z in one line
my.x <- 1 ; my.y <- 2; my.z <- 3

#' 
#' We can read this code as _the value 123 is assi
#' 
#' The use of an arrow symbol `<-` for object defi
#' 
#' You can also use the `=` symbol to define objec
#' 
#' The name of the object is important in R. With 
#' 
#' R executes the code looking for objects availab
#' 
## ---- error=TRUE---------------------------------------------------------
print(z)

#' 
#' The error occurred because object `z` does not 
#' 
#' 
#' ## Creating Vectors
#' 
#' In the previous examples, we have created simpl
#' 
#' One of the most used procedures in R is the cre
#' 
#' Atomic vectors are created in R using the `c` c
#' 
## ------------------------------------------------------------------------
# create numeric atomic vector
x <- c(1,2,3)

# print it
print(x)

#' 
#' This command works the same way for any other c
#' 
## ------------------------------------------------------------------------
# create character atomic vector
y <- c('text 1', 'text 2', 'text 3', 'text 4')

# print it
print(y)

#' 
#' The only restriction on the use of the `c` comm
#' 
## ------------------------------------------------------------------------
# a mixed vector
x <- c(1, 2, '3')

# print result of forced conversion
print(x)

#' 
#' The values of `x` are all of type `character`. 
#' 
## ------------------------------------------------------------------------
# print class of x
class(x)

#' 
#' 
#' ## Knowing Your Environment
#' 
#' After using various commands, further developme
#' 
#' 
## ---- eval=FALSE---------------------------------------------------------
## # set some objects
## x <- 1
## y <- 2
## z <- 3
## 
## # print all objects in the environment
## print(ls())

#' 
#' 
#' The objects `x`, `y` and `z` were created and a
#' 
#' To display the content of each object, just ent
#' 
## ------------------------------------------------------------------------
# print objects by their name
x
y
z

#' 
#' Typing the object name on the screen has the sa
#' 
#' In R, all objects belong to a class. As previou
#' 
## ------------------------------------------------------------------------
# set objects
x <- 1
y <- 'a'
my.fct <- function(){}

# print their classes
print(class(x))
print(class(y))
print(class(my.fct))

#' 
#' Another way to learn more about an object is to
#' 
## ------------------------------------------------------------------------
# print the textual representation of a vector
print(str(1:10))

#' 
#' This function is particularly useful when tryin
#' 
#' 
#' ## Displaying and Formatting Output
#' 
#' So far, we saw that you can show the value of a
#' 
#' However, there are other specific functions to 
#' 
#' For example, if we wanted to show the text, `Th
#' 
## ------------------------------------------------------------------------
# set x
x <- 2

# print customized message
cat('The value of x is', x)

#' 
#' You can also customize the screen output using 
#' 
## ------------------------------------------------------------------------
# set text with break line
my.text <- ' First Line,\n Second line'

# print it
cat(my.text)

#' 
#' Note that the use of `print` would not result i
#' 
## ------------------------------------------------------------------------
print(my.text)

#' 
#' Another example in the use of specific commands
#' 
## ------------------------------------------------------------------------
# set text with tab
my.text <- 'A->\t<-B'

# concatenate and print it!
cat(my.text)

#' 
#' We’ve only scratched the surface on the possibl
#' 
#' 
#' ### Customizing the Output
#' 
#' Another way to customize text output is using s
#' 
#' Function `paste` _glues_ a series of objects to
#' 
## ------------------------------------------------------------------------
# set some text objects
my.text.1 <- 'I am a text'
my.text.2 <- 'very beautiful'
my.text.3 <- 'and informative.'

# paste all objects together and print
cat(paste(my.text.1, my.text.2, my.text.3))

#' 
#' The previous result is not far from what we did
#' 
## ------------------------------------------------------------------------
# example of paste0
cat(paste0(my.text.1, my.text.2, my.text.3))

#' 
#' Another very useful possibility with the `paste
#' 
## ------------------------------------------------------------------------
# example using the argument sep
cat(paste(my.text.1, my.text.2, my.text.3, sep = ', ')) 

#' 
#' If we had an atomic vector with all elements to
#' 
## ------------------------------------------------------------------------
# set character object 
my.text <-c('I am a text', 'very beautiful', 'and informative.')

# example of using the collapse argument in paste
cat(paste(my.text, collapse = ', ')) 

#' 
#' Going forward, command `format` is used to form
#' 
## ------------------------------------------------------------------------
# example of decimal points in R
cat(1/3)

#' 
#' If we wanted only two digits on the screen, we 
#' 
## ------------------------------------------------------------------------
# example of using format on numerical objects
cat(format(1/3, digits=2))

#' 
#' Likewise, if we wanted to use a scientific form
#' 
#' 
## ------------------------------------------------------------------------
# example of using scientific format
cat(format(1/3, scientific=TRUE))

#' 
#' Function `format` has many more options. If you
#' 
#' 
#' ## Finding the Size of Objects
#' 
#' In the practice of programming with R, it is ve
#' 
#' In R, the size of an object can be checked with
#' 
#' Function `length` is intended for objects with 
#' 
## ------------------------------------------------------------------------
# create atomic vector
x <- c(2,3,3,4,2,1)

# get length of x
n <- length(x)

# display message
cat('The size of x is ', n)

#' 
#' For objects with more than one dimension, such 
#' 
## ------------------------------------------------------------------------
# create a matrix
M <- matrix(1:20, nrow = 4, ncol = 5)

# print matrix
print(M)

# calculate size in different ways
my.nrow <- nrow(M)
my.ncol <- ncol(M)
my.n.elements <- length(M)

# display message 
cat('The number of lines in M is ', my.nrow)
cat('The number of columns in M is ', my.ncol)
cat('The number of elements in M is ', my.n.elements)

#' 
#' The `dim` function shows the dimension of the o
#' 
## ------------------------------------------------------------------------
# get dimension of M
my.dim <- dim(M)

# print it
print(my.dim)

#' 
#' In the case of objects with more than two dimen
#' 
## ------------------------------------------------------------------------
# create an array with three dimensions
my.array <- array(1:9, dim = c(3,3,3))

# print it
print(my.array)

# display its dimensions
print(dim(my.array))

#' 
#' An important note here is that the use of the f
#' 
## ------------------------------------------------------------------------
# set text object
my.char <- 'abcde'

# print result of length
print(length(my.char))

#' 
#' This occurred because the `length` function ret
#' 
## ------------------------------------------------------------------------
# find the number of characters in an character object
print(nchar(my.char))

#' 
#' 
#' ## Selecting the Elements of an Atomic Vector
#' 
#' After creating an atomic vector of a class, it 
#' 
#' The selection of _pieces_ of an atomic vector i
#' 
## ------------------------------------------------------------------------
# set x
my.x <- c(1, 5, 4, 3, 2, 7, 3.5, 4.3)

#' 
#' If we wanted only the third element of `my.x`, 
#' 
## ------------------------------------------------------------------------
# get third element of x
elem.x <- my.x[3]

# print it
print(elem.x)

#' 
#' The procedure of indexing also works with vecto
#' 
## ------------------------------------------------------------------------
# get last and penultimate value of my.x
piece.x.1 <- my.x[ (length(my.x)-1):length(my.x) ]

# print it
print(piece.x.1)

#' 
#' A cautionary note. **A unique property of the R
#' 
## ------------------------------------------------------------------------
# set object
my.vec <- c(1,2,3)

# print non-existing fourth element
print(my.vec[4])

#' 
#' It is important to know this behaviour because 
#' 
#' The use of indices is very useful when you are 
#' 
## ------------------------------------------------------------------------
# find all values in my.x that are greater than 3
piece.x.2 <- my.x[my.x>3]

# print it
print(piece.x.2) 

#' 
#' It is also possible to index elements by more t
#' 
## ------------------------------------------------------------------------
# find all values of my.x that are greater than 2 and lower then 4
piece.x.3 <- my.x[ (my.x>2) & (my.x<4) ]
print(piece.x.3)

#' 
#' Likewise, if we wanted all items that are lower
#' 
## ------------------------------------------------------------------------
# find all values of my.x that are lower than 3 or higher than 6
piece.x.4 <- my.x[ (my.x<3)|(my.x>6) ]

# print it
print(piece.x.4)

#' 
#' Moreover, logic indexing also works with the in
#' 
## ------------------------------------------------------------------------
# set my.x and my.y
my.x <- c(1,4,6,8,12)
my.y <- c(-2,-3,4,10,14)

# find all elements of my.x where my.y is higher than 0
my.piece.x <- my.x[ my.y > 0 ]

# print it
print(my.piece.x)

#' 
#' Looking more closely at the indexing process, i
#' 
## ------------------------------------------------------------------------
# create a logical object
my.logical <- my.y > 0

# print it 
print(my.logical) 

# find its class
class(my.logical)

#' 
#' 
#' ## Removing Objects from the Memory
#' 
#' After creating several variables, the R environ
#' 
#' For example, given an object `x`, we can delete
#' 
## ------------------------------------------------------------------------
# set x
x <- 1

# print all available objects
ls()

# remove x
rm('x')

# print again all available objects
ls()

#' 
#' Note that after executing the command `rm('x')`
#' 
## ---- eval=FALSE---------------------------------------------------------
## rm(list=ls())

#' 
#' The term `list` in `rm(list=ls())` is a functio
#' 
#' 
#' ## Displaying and Setting the Working Directory
#' 
#' Like other programming platforms, **R always wo
#' 
#' To show the current working directory, use func
#' 
## ----tidy=TRUE-----------------------------------------------------------
# get current dir 
my.dir <- getwd()

# display it
print(my.dir)

#' 
#' The result of the previous code shows the folde
#' 
#' The change of working directory is performed wi
#' 
## ----eval=FALSE----------------------------------------------------------
## # set where to change directory
## my.d <- 'C:/My Research/'
## 
## # change it
## setwd(my.d)

#' 
#' As for simple cases such as the above, remember
#' 
## ---- eval=FALSE, error=TRUE---------------------------------------------
## # set directory (WRONG WAY)
## my.d <- 'C:\My Research\'

#' 
#' 
#' This message means that R was not able to under
#' 
## ----eval=FALSE----------------------------------------------------------
## # set directory (CORRECT WAY)
## my.d <- 'C:/My Research/'
## 
## # change dir
## setwd(my.d)

#' 
#' You can also use double backslashes `\\` but th
#' 
#' Another important information here is that you 
#' 
## ----eval=FALSE----------------------------------------------------------
## # change to subfolder
## setwd('Data')

#' 
#' Another possibility is to go to a previous leve
#' 
## ----eval=FALSE----------------------------------------------------------
## # change to previous level
## setwd('..')

#' 
#' So, if you are working in the directory `C:/My 
#' 
#' Another, more modern, way of setting the direct
#' 
## ---- eval=FALSE---------------------------------------------------------
## my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
## setwd(my.path)

#' 
#' This way, the script will change the directory 
#' 
#' 
#' ## Cancelling Code Execution
#' 
#' Whenever R is running some code, a visual cue i
#' 
#' To try it out, run the next chunk of code in RS
#' 
## ---- tidy=FALSE, eval=FALSE---------------------------------------------
## for (i in 1:100) {
##   cat('\nRunning code (please make it stop by hitting esc!)')
##   Sys.sleep(1)
## }

#' 
#' In the previous code, we used a `for` loop to d
#' 
#' 
#' ## Code Comments
#' 
#' In R, comments are set using the hash tag symbo
#' 
## ------------------------------------------------------------------------
# this is a comment (R will not parse it)
# this is another comment (R will not parse it)

x <- 'abc' # this is an inline comment 

#' 
#' Comments are a way to communicate any important
#' 
## ---- eval=FALSE---------------------------------------------------------
## # read csv file
## df <- read.csv('MyDataFile.csv')

#' 
#' As you can see, it is quite obvious from line `
#' 
## ---- eval=FALSE, tidy=FALSE---------------------------------------------
## # Script for analyzing a dataset
## # Author: Mr data analyst (dontspamme@emailprovider.com)
## # Last script update: 2017-03-10
## #
## # File downloaded from www.sitewithdatafiles.com/data-files/
## # The description of the data goes here
## # Last file update: 2017-03-10
## df <- read.csv('MyDataFile.csv')

#' 
#' So, by reading the comments, the user will know
#' 
#' Another use of comments is to set sections in t
#' 
## ---- eval=FALSE, tidy=FALSE---------------------------------------------
## # Script for analyzing a dataset
## # Author: Mr data analyst (dontspamme@emailprovider.com)
## # Last script update: 2017-03-10
## #
## # File downloaded from www.sitewithdatafiles.com/data-files/
## # The description of the data goes here
## # Last file update: 2017-03-10
## ...
## 
## # Clean data
## # - remove outliers
## # - remove unnecessary columns
## 
## ...
## 
## # Report results
## # - remove outliers
## # - remove unnecessary columns
## 
## ...
## 

#' 
#' This way, once you need to change a particular 
#' 
#' 
#' ## Looking for Help
#' 
#' A common task in the use of R is to seek help. 
#' 
#' You can get help by using the _help_ panel in R
#' 
#' In R, the help screen of a function is the same
#' 
#' 
#' If we are looking for help for a given text and
#' 
#' As a suggestion of usage, the easiest and most 
#' 
#' Another very important source of help is the In
#' 
#' 
#' ## R Packages
#' 
#' One of the greatest benefits of using R is its 
#' 
#' Every function in R belongs to a package. When 
#' 
#' * [GetHFData](https://CRAN.R-project.org/packag
#' 
#' * [GetTDData](https://CRAN.R-project.org/packag
#' 
#' * [RndTexExams](https://CRAN.R-project.org/pack
#' 
#' * [BatchGetSymbols](https://CRAN.R-project.org/
#' 
#' * [Predatory](https://CRAN.R-project.org/packag
#' 
#' * [pafdR](https://CRAN.R-project.org/package=pa
#' 
#' CRAN is the official repository of R and it is 
#' 
#' The complete list of packages available on CRAN
#' 
#' Another important source for finding packages i
#' 
#' 
#' Unlike CRAN, R-Forge and Github have no restric
#' 
#' The most interesting part of this is that the p
#' 
## ------------------------------------------------------------------------
# get matrix with available packages
df.cran.pkgs <- available.packages()

# find the number of packages
n.cran.packages <- nrow(df.cran.pkgs)

# print it
print(n.cran.packages)

#' 
#' If asked about which mirror to use, simply sele
#' 
## ------------------------------------------------------------------------
# print information about the first three packages
print(df.cran.pkgs[1:3, ])

#' 
#' In short, object `df.cran.pkgs` displays the na
#' 
#' You can also check the amount of locally instal
#' 
## ------------------------------------------------------------------------
# find number of packages currently installed
n.local.packages <- nrow(installed.packages())

# print it 
print(n.local.packages)

#' 
#' In this case, the computer on which the book wa
#' 
#' 
#' ### Installing Packages from CRAN
#' 
#' To install a package, simply use the command `i
#'  
## ----eval=FALSE----------------------------------------------------------
## # install package quantmod
## install.packages("quantmod")

#' 
#' That's it! After executing this simple command,
#' 
#' 
#' ### Installing Packages from Github
#' 
#' To install a package hosted in Github, you must
#' 
## ----eval=FALSE----------------------------------------------------------
## # install devtools
## install.packages('devtools')

#' 
#' After that, load up the package `devtools` and 
#' 
## ----eval=FALSE----------------------------------------------------------
## # load up devtools
## library(devtools)
## 
## # install ggplot2 from github
## install_github("hadley/ggplot2")

#' 
#' Note that the username of the developer is also
#' 
#' 
#' ### Loading Packages
#' 
#' Within a script, use function `library` to load
#' 
## ----eval=FALSE----------------------------------------------------------
## # load package quantmod
## library(quantmod)

#' 
#' After running this command, all functions of th
#' 
## ----error=TRUE----------------------------------------------------------
library(unicorn)

#' 
#' Remember this error message. It will appear eve
#' 
#' If you use a specific package function and do n
#' 
## ------------------------------------------------------------------------
# example of using a function without loading package
fortunes::fortune(10)

#' 
#' In this case, we use the function `fortune` fro
#' 
#' Another way of loading a package is using the `
#' 
#' The use of `require` is left for loading up pac
#' 
## ----eval=FALSE----------------------------------------------------------
## my.fct <- function(x){
##     require(quantmod)
## 	
## 	df <- getSymbols(x, auto.assign = F)
## 	return(df)
## }

#' 
#' In this case, the first time that `my.fct` is c
#' 
#' 
#' ### Upgrading Packages
#' 
#' Over time, it is natural that the packages avai
#' 
#' 
#' The user can also update packages through the p
#' 
## ----eval=FALSE----------------------------------------------------------
## # update all installed packages
## update.packages()

#' 
#' The command `update.packages` compares the vers
#' 
#' 
#' ## Using Code Completion with _tab_ 
#' 
#' A very useful feature of RStudio is _code compl
#' 
#' 
#' This also works for packages. To check it, type
#' 
#' 
#' Note that a description of the package or objec
#' 
#' The use of this tool becomes even more benefici
#' 
#' You can also find files and folders on your com
#' 
#' 
#' The use of autocomplete is also possible for fi
#' 
#' 
#' By using _tab_ inside of a function, we have th
#' 
#' Summing up, using code completion will make you
#' 
#' 
#' ## Interacting with Files and the Operating Sys
#' 
#' In many data analysis situations, it will be ne
#' 
#' 
#' ### Listing Files and Folders
#' 
#' To list files from your computer, use function 
#' 
## ------------------------------------------------------------------------
# list files in data folder
my.f <- list.files(path = "data", full.names = TRUE)
print(my.f)

#' 
#' Note that in this directory, there are several 
#' 
## ----eval=FALSE----------------------------------------------------------
## # list all files for all subfolders (IT MAY TAKE SOME TIME...)
## list.files(path = getwd(), recursive = T, full.names = TRUE)

#' 
#' The previous command will list all files in the
#' 
#' To list folders (directories) on your computer,
#' 
## ------------------------------------------------------------------------
# store names of directories
my.dirs <- list.dirs(recursive = F)

# print it
print(my.dirs)

#' 
#' The command ` list.dirs(recursive = F)` listed 
#' 
## ------------------------------------------------------------------------
# list all files with extension .Rmd
list.files(pattern = "*.Rmd")

#' 
#' The files presented above contain all the conte