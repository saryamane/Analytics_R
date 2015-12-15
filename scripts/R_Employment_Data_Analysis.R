# We will look at the employment data and see how good or bad it is.

# We get this data from the Bureau of Labor Statistics of US gov.

# The data is downloaded as the single csv zipped file from their website.

# In this dataset, we will ingest the data into R, tranform, manipulate it,
# create subsets of data and generating visualizations that might provide some insights about
# the patterns in data.

# Load the required libraries for this exercise.

# The package data.table improves upon the data.frame object in R, 
# making operations faster and allowing the specification of an index variable in a dataset, 
# a concept familiar to database experts. It also allows the fast aggregation of large data, 
# including very fast-ordered joins. We will primarily use the data.table package for its function fread,
# which allows (very) fast importing of large-structured datasets into R. 

library(data.table)
library(plyr)

# The dplyr package is the next iteration of the popular package, plyr, by Dr. Hadley Wickham. 
# It is targeted at rectangular data and allows very fast aggregation,
# transformation, summarization, column selection, and joins.

install.packages("dplyr")
library(dplyr)

# The stringr package provides tools for text and string manipulation. 
# This package streamlines and syntactically unifies available string manipulation 
# functionalities available in R, making tasks involving string search, manipulation,
# and extraction much easier

library(stringr)
install.packages("ggplot2")
library(ggplot2)
install.packages("maps")
library(maps)
install.packages("bit64")
library(bit64)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("choroplethr")
library(choroplethr)

# Set the working directory

setwd("~/R_projects/Analytics_R/data")
getwd()

# Read the csv file into the table.

ann2012 <- read.csv('2012.annual.singlefile.csv', stringsAsFactors = FALSE)

# This took a long time. We can try the fread library from the data.table package.

ann2012_1 <- fread(input='2012.annual.singlefile.csv')

