#' # Importing and Exporting Data from Local Files
#' 
#' 
#' In chapter \@ref(DataStructureObjects), we stud
#' 
#' 
#' ## Importing Data from Local Files
#' 
#' The easiest way to import data into R is using 
#' 
#' Throughout this chapter, we will assume the imp
#' 
#' Since R allows the use of relative paths, using
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
#' Notice how the code is self-contained and porta
#' 
#' 
#' ### Importing Data from a _.csv_ File (_comma s
#' 
#' 
#' Consider the data file called `r my.f`, located
#' 
#' The first lines of `r my.f`, also called header
#' 
#' To load the contents of file `r my.f` in R, use
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
#' Note that the column of dates (`date`) was impo
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
#' Another possibility for importing _.csv_ files 
#' 
## ---- message=TRUE-------------------------------------------------------
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
#' Now, let's check the classes of the column:
#' 
## ------------------------------------------------------------------------
# print column classes
print(sapply(my.df.sp500, class))

#' 
#' As expected, it looks good. Both columns have t
#' 
#' 
#' ### Importing Data from an _Excel_ File 
#' 
#' In many situations, the data to be analysed is 
#' 
#' R does not have a native function for importing
#' 
#' Although the previous cited packages have simil
#' 
#' In this section, we will give priority to packa
#' 
## ------------------------------------------------------------------------
library(readxl)

# set excel file
my.f <- 'data/SP500-Excel.xlsx'

# read excel file 
my.df <- read_excel(my.f, sheet = 'sp500-prices')

# print classes
print(sapply(my.df, class))

# print with head (first five rows)
print(head(my.df))

#' 
#' As we can see, one of the benefits of using Exc
#' 
#' The downside of using Excel files for storing d
#' 
#' 
#' ### Importing Data from a _.RData_ File
#' 
#' R has a native format to save objects from the 
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
#' As expected, the `r my.file` file is available,
#' 
## ------------------------------------------------------------------------
# clear environment
rm(list=ls())

# load file
load(file = 'data/temp.RData')

# print all objects in environment
print(ls())

#' 
#' We can see object `my.x` was recovered, and it 
#' 
#' 
#' ### Importing Data from SQLITE
#' 
#' The use of _.csv_ or _.RData_ files for storing
#' 
#' This brings us to the topic of _database softwa
#' 
#' Before moving to the examples, we need to under
#' 
#' As an example, let's first create a SQLITE data
#' 
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

#' 
#' The `TRUE` output of `dbWriteTable` indicates e
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
# set sql statement
my.SQL <- "select * from myTable2 where G='A'"

# get query
my.df.A <- dbGetQuery(conn = my.con, statement = my.SQL)

# disconnect from db
dbDisconnect(my.con)

# print with str
print(str(my.df.A))

#' 
#' It also worked as expected. 
#' 
#' In this simple example, we can see how easy it 
#' 
#' 
#' ### Importing Data from a Text File
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
#' In this example, we imported the entire content
#' 
#' 
#' ### Other File Formats
#' 
#' Using the import functions for files with exten
#' 
#' 
#' ## Exporting to Local File
#' 
#' A very common operation in the use of R is to w
#' 
#' In most situations, the use of _.csv_ files sat
#' 
#' 
#' ### Exporting Data to a _.csv_ File 
#' 
#' To write a _.csv_ file, use the `write.csv` fun
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
#' In the previous example, we save the object `my
#' 
#' 
## ------------------------------------------------------------------------
# read it
my.df.import <- read.csv(f.out)

# print first five rows
print(head(my.df.import))

#' 
#' Note that a column called `x`, containing the n
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
#' ### Exporting Data to a _RData_ File
#' 
#' The native solution for exporting R objects is 
#' 
## ---- tidy=FALSE---------------------------------------------------------
# set random data
my.x <- runif(100)
my.df <- data.frame(y = runif(100),
                    z = runif(100))

my.f <- 'data/temp.RData'
save(list = c('my.x', 'my.df'),
     file = my.f)
      

#' 
#' After saving it, the contents of file `r my.f` 
#' 
#' 
#' ### Exporting Data to an Excel File
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
#' If you want to save several `dataframes` into s
#' 
## ---- tidy=FALSE---------------------------------------------------------
# create two dataframes
N <- 25
my.df.A <- data.frame(y = seq(1,N), 
                      z = rep('a',N))

my.df.B <- data.frame(z = rep('b',N))

# set file out
f.out <- 'data/temp.xlsx'

# write in different sheets
write.xlsx(x = my.df.A, 
           file = f.out, 
           sheetName = "my df A")

write.xlsx(x = my.df.B, 
           file = f.out, 
           sheetName = "my df B", 
           append = TRUE )

#' 
#' 
#' ### Exporting Data to a Text File
#' 
#' In some situations, you may need to export an o
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