# Install the R library packages

install.packages("plyr")
install.packages("ggplot2")
install.packages("reshape2")

# Load these library packages to our current environment

library("plyr")
library("ggplot2")
library("reshape2")

# These 3 libraries enable you to use the split-apply-combine function to any of your data analysis tasks.

# Lets read the data csv file into memory first.

setwd("~/R_projects/Practical_DataScienceTools_Cookbook_recipes/data")

vechiles <- read.csv("vehicles.csv", stringsAsFactors = F)

head(vechiles)

# Let us now read the variables lables in.

# lables <- read.table("varlables.txt", sep = '-', header = F) # Failed attempt

# Instead we do this.

lables <- do.call(rbind, strsplit(readLines("varlables.txt"), " - "))

head(lables)
tail(lables)

# Thus we have now successfully loaded in the labels as well for the data file.

# Let's inspect our dataset in more depth

nrow(vechiles)
# Wow there are about 36K observations.

ncol(vechiles)
# There are about 83 columns.

names(vechiles) # Inspect the column names of the dataset.

unique(vechiles[,"year"]) # It has data points from 1985 to 2016. That means our dataset is pretty updated.

length(unique(vechiles[,"year"]))

min(vechiles[,"year"])
max(vechiles[,"year"])

# Let's see what the fuel types are for most vehicles.

table(vechiles$fuelType1)

# This gives you the distribution of the dataset for the records with the different fuel type in it.

table(vechiles$fuelType2) # checking for whats in fuel type 2.

table(vechiles$trany)

# Let's set the NA values for the transmission column.

vechiles$trany[vechiles$trany == ""] <- NA

table(vechiles$trany)

vechiles$trany2 <- ifelse(substr(vechiles$trany,1,4) == "Auto", "Auto", "Manual")

table(vechiles$trany2)

# Convert the new variable as a factor.

vechiles$trany2 <- as.factor(vechiles$trany2)
table(vechiles$trany2)

# There are roughly as twice many automobiles in Auto transmission vs those in manual gear.

with(vechiles, table(sCharger, year))

# we use the with command. This command tells R to use vehicles as the 
# default data when performing the subsequent command, in this case, table. 
# Thus, we can omit prefacing the sCharger and year 
# column names with the name of the data frame and vehicles, followed by the dollar sign.


class(vechiles$sCharger)
unique(vechiles$sCharger)

class(vechiles$tCharger)
unique(vechiles$tCharger)

vechiles$tCharger <- ifelse(vechiles$tCharger==TRUE, "t", "")
vechiles$tCharger <- ifelse(is.na(vechiles$tCharger),"",vechiles$tCharger)

# Now we have converted the tCharger to be very similar as that of the sCharger column,
# where earlier it wasn't the case.

# Applying the ddply method of split-apply-combine feature.

mpgByYr <- ddply(vechiles, ~year, summarise, avgMPG = mean(comb08), avgHwy = mean(highway08), avgCity = mean(city08))

summary(mpgByYr)

# Use the ggplot to vizualize the data

ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("All cars")

# We have not ignored the hybrid cars from this mix, so we need to make sure that we look only
# at the gasoline powered cars for this analysis, where we take a subset of the original dataset.

unique(vechiles$fuelType1)
unique(vechiles$fuelType2)
unique(vechiles$atvType)

gasCars <- subset(vechiles, fuelType1 %in% c("Regular Gasoline","Premium Gasoline","Midgrade Gasoline") & fuelType2 == "" & atvType != "Hybrid")

mpgByYr_Gas <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08), avgHwy = mean(highway08), avgCity = mean(city08))

ggplot(mpgByYr_Gas, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("Gas cars")

# Let's validate the hypothesis that the cars with smaller engine or displacement has better average MPG

typeof(gasCars$displ)
length(unique(gasCars$displ))
unique(gasCars$displ)

gasCars$displ <- ifelse(is.na(gasCars$displ),0,gasCars$displ)

ggplot(gasCars, aes(displ,comb08)) + geom_point() + geom_smooth()

# Thus we see there is a negative correlation found between the 2 variables.

# Now lets see if the small cars were made in later years to see the drastic improvement
# in the mpg metric.

avgCarSize <- ddply(gasCars, ~year, summarise, avgDispl = mean(displ))
head(avgCarSize)

ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average engine size displacement") + ggtitle("Displacement vs year make chart")


# Lets create a sub table which has the average MPG with the average displaement by year.

byYear <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08), avgDispl = mean(displ))
head(byYear)

# To use the faceting capability of ggplot2 to display 
# Average MPG and Avg engine displacement by year on separate 
# but aligned plots, we must melt the data frame, converting it 
# from what is known as a wide format to a long format:

byYear2 <- melt(byYear, id="year")
levels(byYear2$variable) <- c("Average MPG","Average engine Displacement")
head(byYear2)
tail(byYear2)

# Plot the 2 graphs now side by side using ggplot

ggplot(byYear2, aes(year, value)) + geom_point() + geom_smooth() + facet_wrap(~variable, ncol=1, scales = "free_y") + xlab("Year") + ylab("")

# Given the trend toward smaller displacement engines, 
# let's see whether automatic or manual transmissions are more 
# efficient for four cylinder engines, and how the efficiencies have changed over time.

gasCars4 <- subset(gasCars, cylinders == "4")
ggplot(gasCars4, aes(factor(year), comb08)) + geom_boxplot() + 
  facet_wrap(~trany2, ncol=1) + labs(x = "Year", y= "MPG") + 
  theme(axis.text.x = element_text(angle = 45))

# Next, let's look at the change in proportion of manual cars available each year

ggplot(gasCars4, aes(factor(year), fill = factor(trany2))) + geom_bar(position = "fill") + labs(x = "Year", y = "Proportion of car", fill = "Transmission") + theme(axis.text.x = element_text(angle = 45)) + geom_hline(yintercept=0.5, linetype=2)

# We will now analyze the make and model of the cars and how they have changed
# over the period of time.

carsMake <- ddply(gasCars4, ~year, summarise, numberOfMakes = length(unique(make)))
head(carsMake)

ggplot(carsMake, aes(year, numberOfMakes)) + geom_point() + labs(x = "Year", y="Number of available makes") + ggtitle("Four cylinder cars")

# Let's look at the makes that are being available.

uniqMakes <- dlply(gasCars4, ~year, function(x) unique(x$make))
head(uniqMakes)

commonMakes <- Reduce(intersect, uniqMakes) # Which makes are available in all years.
commonMakes

carCommonMakes4 <- subset(gasCars4, make %in% commonMakes)
avgMPG_commonMakes <- ddply(carCommonMakes4, ~year + make, summarise, avgMPG = mean(comb08))

ggplot(avgMPG_commonMakes, aes(year, avgMPG)) + geom_line() + facet_wrap(~make, nrow = 4) + ggtitle("MPG evolution of 4 cylinder cars per Popular models")

# Analyze the BMW chart.

bmwCar <- subset(gasCars4, make == "BMW")
head(bmwCar)
avg_MPG_bmw <- ddply(bmwCar, ~year, summarise, avgMPG = mean(comb08))
ggplot(avg_MPG_bmw, aes(year, avgMPG)) + geom_line() + labs(x = "Year", y = "BMW MPG") + ggtitle("How BMW has improved MPG")


# End of script - End of analysis
# Topics covered: ddply, dlply, ggplot and other R utilities