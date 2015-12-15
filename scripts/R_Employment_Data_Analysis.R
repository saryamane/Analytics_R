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

# For the information on the BLS metadata, look at this file.
# http://www.bls.gov/cew/doc/layouts/csv_annual_layout.htm

ann2012 <- read.csv('2012.annual.singlefile.csv', stringsAsFactors = FALSE)

# This took a long time. We can try the fread library from the data.table package.

ann2012_1 <- fread(input='2012.annual.singlefile.csv')

head(ann2012_1)
nrow(ann2012_1)
ncol(ann2012_1)
dim(ann2012_1)
unique(ann2012_1$year)
getwd()

ann2012_1 <- fread(input='../../Practical_DataScienceTools_Cookbook_recipes/data/2012.annual.singlefile.csv', sep=',',colClasses = c('character', rep('integer',5),'character',rep('integer',31)))

head(ann2012_1)

# Let's read the data of those files into its respected variables.

for(u in c('agglevel','area','industry','ownership','size')) {
  assign(u,read.csv(paste('../../Practical_DataScienceTools_Cookbook_recipes/data/',u,'_titles.csv',sep=''),stringsAsFactors = FALSE ))
}

# Let's see which of the fields are common between our lookup files and the actual data set.

intersect(names(ann2012_1),names(agglevel))
# agglvl_code
intersect(names(ann2012_1),names(area))
# area_fips
intersect(names(ann2012_1),names(industry))
# industry_code
intersect(names(ann2012_1),names(ownership))
# own_code
intersect(names(ann2012_1),names(size))
# size_code

head(agglevel)
