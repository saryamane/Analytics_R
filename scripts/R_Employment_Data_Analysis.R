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

library(data.table)
typeof(ann2012_1)
typeof(ann2012)
codes <- c('agglevel','industry','ownership','size')
ann2012full <- ann2012
for(i in 1:length(codes)){
  eval(parse(text=paste('ann2012full <- left_join(ann2012full, ',codes[i],')', sep='')))
}

head(ann2012full)

# Adding the geographical information to the analysis.
# Let's look at the geo distribution of wages across the US.
# For this we have to have maps package installed.

# We already have the area dataset imported into R.

head(area)

# We need to capitalize the names per the conventions, so let's write a function to do tht.

simpleCap <- function(x) {
  if(!is.na(x)){
    s <- strsplit(x,' ')[[1]]
    paste(toupper(substring(s,1,1)),substring(s,2), sep='',collapse = ' ')
  } else {NA}
}

data("county.fips")
head(county.fips)

# This fips has missing leading 0's there, so let's use the stringr package to add that.

county.fips$fips <- str_pad(county.fips$fips, width = 5, pad="0")

# Now we have padded the left with 0's to match our dataset fips value.

# Next we want to separate out the county names from the polynames in the column of polyname

county.fips$polyname <- as.character(county.fips$polyname)
county.fips$polyname <- sapply(
  gsub('[a-z\ ]+,([a-z\ ]+)','\\1',county.fips$polyname),simpleCap)

head(county.fips$polyname)
county.fips <- unique(county.fips)
nrow(county.fips)

# Let's do something similar for the state.fips table.

data('state.fips')
head(state.fips)

# Again pad the fips column with 0's 

state.fips$fips <- str_pad(state.fips$fips, width=2, pad="0", side='left')
state.fips$state <- as.character(state.fips$polyname)
state.fips$state <- gsub("([a-z\ ]+):[a-z\ \\']+",'\\1',state.fips$state)
state.fips$state <- sapply(state.fips$state, simpleCap)
mystatefips <- unique(state.fips[,c('fips','abb','state')])
nrow(mystatefips)

# The setdiff set operation looks for all the elements in the first set that are not in the second set.

lower48 <- setdiff(unique(state.fips$state), c('Hawaii','Alaska'))

# Finally we put all this information we processed into a single dataset.
myarea <- NULL # This resets the variable in case you make a mistake.
myarea <- merge(area, county.fips, by.x='area_fips', by.y='fips', all.x=TRUE)
head(myarea)
myarea$state_fips <- substr(myarea$area_fips,1,2)
myarea <- merge(myarea, mystatefips, by.x='state_fips', by.y='fips', all.x=TRUE)

# Lastly we join this geo info with our dataset, and filter it to keep only the 48 states data

ann2012full <- left_join(ann2012full, myarea)
head(ann2012full)
ann2012full <- filter(ann2012full, state%in% lower48)

# Next lets store this final dataset in R data (rda) file on disk.
# This provides an efficient storage mechanism for R objects on the disk.

save(ann2012full, file='../../Practical_DataScienceTools_Cookbook_recipes/data/ann2012full.rda', compress=TRUE)

# Now we have all the data we need for our analysis. A peek at the metadata tells us that to get the data
# in the state format, we need to use the filter agglvl_code of 50.

d.state <- filter(ann2012full, agglvl_code==50)
head(d.state)

d.state <- select(d.state, state, avg_annual_pay, annual_avg_emplvl)

# We then create two new variables, wage and empquantile for our statistical needs, where we discretize our 
# continous variables.

d.state$wage <- cut(d.state$avg_annual_pay,
                    quantile(d.state$avg_annual_pay, c(seq(0,.8, by=.2), .9,.95,.99, 1)))

d.state$empquantile <- cut(d.state$annual_avg_emplvl, 
                           quantile(d.state$annual_avg_emplvl, c(seq(0,.8,by=.2),.9,.95,.99,1)))

# We want the output of these discrete variables to be useful, so we issue the following commands to 
# harmonize those derived fields.

x <- quantile(d.state$avg_annual_pay, c(seq(0,.8,by=.2),.9, .95, .99, 1))
x
xx <- paste(round(x/1000),'K', sep='') # Convert the large numbers into thousands buckets
xx
Labs <- paste(xx[-length(xx)],xx[-1],sep='-') # This helps us to create a range.
Labs
levels(d.state$wage) <- Labs
head(d.state)

# This is how we do bucketing in R, very slick and clean. Definitely recommend over the MySQL way of doing things.

# Next we do the same for the employee quantile group.

x <- quantile(d.state$annual_avg_emplvl, c(seq(0,.8,by=.2),.9,.95,.99,1))
x
xx <- paste(round(x/1000),'K',sep='')
xx
Labs <- paste(xx[-length(xx)],xx[-1], sep='-')
Labs
levels(d.state$empquantile) <- Labs

head(d.state)

# We now perform the same steps for the city information where the code==70.

d.cty <- filter(ann2012full, agglvl_code==70)
d.cty <- select(d.cty, state, abb, avg_annual_pay, annual_avg_emplvl)
names(d.cty)

d.cty$wage <- cut(d.cty$avg_annual_pay,
                    quantile(d.cty$avg_annual_pay, c(seq(0,.8, by=.2), .9,.95,.99, 1)))

d.cty$empquantile <- cut(d.cty$annual_avg_emplvl, 
                           quantile(d.cty$annual_avg_emplvl, c(seq(0,.8,by=.2),.9,.95,.99,1)))

x <- quantile(d.cty$avg_annual_pay, c(seq(0,.8,by=.2),.9, .95, .99, 1))
x
xx <- paste(round(x/1000),'K', sep='') # Convert the large numbers into thousands buckets
xx
Labs <- paste(xx[-length(xx)],xx[-1],sep='-') # This helps us to create a range.
Labs
levels(d.cty$wage) <- Labs
head(d.cty)

# Now we do the same for the empl salary level on the city level.

x <- quantile(d.cty$annual_avg_emplvl, c(seq(0,.8,by=.2),.9,.95,.99,1))
x
xx <- paste(round(x/1000),'K',sep='')
xx
Labs <- paste(xx[-length(xx)],xx[-1], sep='-')
Labs
levels(d.cty$empquantile) <- Labs

head(d.cty)

# Now lets work on drawing out the map choropleth image using ggplot2

state_df <- map_data('state')
county_df <- map_data('county')

head(state_df)
head(county_df)


