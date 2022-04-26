covid19=read.csv("covid19.csv")

#Look at structure and summary

summary(covid19)
str(covid19)

cols <- c("vaccines", "date", "region", "country")
covid19[ ,cols] <- lapply(covid19[ ,cols] , factor)
str(covid19)

library(DescTools)
PlotMiss(x=covid19)


#We are going to omit the variables GDP growth rate, GDP per capita, and both population ages because < 5% of the data is NA
#We are going to omit the variables GDP growth rate, GDP per capita, and both population ages because < 5% of the data is NA
#We are going to use imputation for the rest of the N/As

#Imputation

summary(covid19$people_vaccinated)    # There are 20 NAs
na_rows <- which(is.na(covid19$people_vaccinated))  # returns indexes of rows with missing data
covid19_copy = covid19

#Log transformation

covid19_copy$people_vaccinated <- log(covid19_copy$people_vaccinated)

covid19_copy$people_vaccinated_per_hundred <- log(covid19_copy$people_vaccinated_per_hundred)


# median() function to obtain the median value for the variable 
# set na.rm = TRUE to ignore the NA values when computing the mean 

covid19_copy$people_vaccinated[is.na(covid19_copy$people_vaccinated)] <- median(covid19_copy$people_vaccinated, na.rm = TRUE)
covid19_copy$people_vaccinated_per_hundred[is.na(covid19_copy$people_vaccinated_per_hundred)] <- median(covid19_copy$people_vaccinated_per_hundred, na.rm = TRUE)
covid19_copy$employment_industry[is.na(covid19_copy$employment_industry)] <- median(covid19_copy$employment_industry, na.rm = TRUE)
covid19_copy$CO2_emission[is.na(covid19_copy$CO2_emission)] <- median(covid19_copy$CO2_emission, na.rm = TRUE)
covid19_copy$Health_expenditure[is.na(covid19_copy$Health_expenditure)] <- median(covid19_copy$Health_expenditure, na.rm = TRUE)

PlotMiss(covid19_copy)

#Omit the rest of the NA's

Covid19_noNAs <- na.omit(covid19_copy)
PlotMiss(Covid19_noNAs)                 #No more NA's


#See if there is any duplicates

duplicated(Covid19_noNAs)   #no duplicates

summary(Covid19_noNAs)
str(Covid19_noNAs)


library(ggplot2)
library(tidyverse)
library(dplyr)

# Create a new column for each vaccine - Example: Moderna
Covid19_noNAs[  , "Moderna_new"] = NA

# If any vaccine value includes "Moderna" this new column will be set to 1, otherwise 0

for (i in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[i] %in% c("Johnson&Johnson, Moderna, Pfizer/BioNTech","Moderna", "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech", "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V", "Moderna, Pfizer/BioNTech") ) {
    Covid19_noNAs$Moderna_new[i] <- 1
  }
  else {
    Covid19_noNAs$Moderna_new[i] <- 0
  }
}

# Check out the new column and its counts
str(Covid19_noNAs$Moderna_new)
table(Covid19_noNAs$Moderna_new)

# If any vaccine value includes "Pfizer/BioNTech" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Pfizer_BioNTech_new"] = NA

for (p in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[p] %in% c("Johnson&Johnson, Moderna, Pfizer/BioNTech", "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech", "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V", "Moderna, Pfizer/BioNTech") ) {
    Covid19_noNAs$Pfizer_BioNTech_new[p] <- 1
  }
  else {
    Covid19_noNAs$Pfizer_BioNTech_new[p] <- 0
  }
}

str(Covid19_noNAs$Pfizer_BioNTech_new)
table(Covid19_noNAs$Pfizer_BioNTech_new)

# If any vaccine value includes "Johnson&Johnson" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Johnson_Johnson_new"] = NA

for (j in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[j] %in% c("Johnson&Johnson" , "Johnson&Johnson, Moderna, Pfizer/BioNTech" ) ) {
    Covid19_noNAs$Johnson_Johnson_new[j] <- 1
  }
  else {
    Covid19_noNAs$Johnson_Johnson_new[j] <- 0
  }
}

str(Covid19_noNAs$Johnson_Johnson_new)
table(Covid19_noNAs$Johnson_Johnson_new)
# If any vaccine value includes "Oxford/AstraZeneca" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Oxford_AstraZeneca_new"] = NA

for (o in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[o] %in% c("Covaxin, Oxford/AstraZeneca" , "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech" , "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V"  , "Oxford/AstraZeneca" , "Oxford/AstraZeneca, Pfizer/BioNTech" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sinopharm/Wuhan, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sputnik V" , "Oxford/AstraZeneca, Sinopharm/Beijing" , "Oxford/AstraZeneca, Sinopharm/Beijing, Sputnik V" , "Oxford/AstraZeneca, Sinovac") ) {
    Covid19_noNAs$Oxford_AstraZeneca_new[o] <- 1
  }
  else {
    Covid19_noNAs$Oxford_AstraZeneca_new[o] <- 0
  }
}

str(Covid19_noNAs$Oxford_AstraZeneca_new)
table(Covid19_noNAs$Oxford_AstraZeneca_new)
# If any vaccine value includes "Sinopharm/Beijing" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Sinopharm_Beijing_new"] = NA

for (s in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[s] %in% c( "Moderna, Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sinopharm/Wuhan, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V" , "Oxford/AstraZeneca, Sinopharm/Beijing" , "Oxford/AstraZeneca, Sinopharm/Beijing, Sputnik V" , "Pfizer/BioNTech, Sinopharm/Beijing" , "Sinopharm/Beijing" , "Sinopharm/Beijing, Sinopharm/Wuhan, Sinovac" , "Sinopharm/Beijing, Sputnik V") ) {
    Covid19_noNAs$Sinopharm_Beijing_new[s] <- 1
  }
  else {
    Covid19_noNAs$Sinopharm_Beijing_new[s] <- 0
  }
}

str(Covid19_noNAs$Sinopharm_Beijing_new)
table(Covid19_noNAs$Sinopharm_Beijing_new)
# If any vaccine value includes "Sputnik V" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Sputnik_V_new"] = NA

for (b in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[b] %in% c("Moderna, Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sinopharm/Wuhan, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sputnik V" , "Oxford/AstraZeneca, Pfizer/BioNTech, Sputnik V" , "Oxford/AstraZeneca, Sinopharm/Beijing, Sputnik V" , "Sinopharm/Beijing, Sputnik V" , "Sputnik V") ) {
    Covid19_noNAs$Sputnik_V_new[b] <- 1
  }
  else {
    Covid19_noNAs$Sputnik_V_new[b] <- 0
  }
}

str(Covid19_noNAs$Sputnik_V_new)
table(Covid19_noNAs$Sputnik_V_new)
# If any vaccine value includes "Sinovac" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Sinovac_new"] = NA

for (r in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[r] %in% c( "Oxford/AstraZeneca, Sinovac" , "Pfizer/BioNTech, Sinovac" , "Sinopharm/Beijing, Sinopharm/Wuhan, Sinovac" , "Sinovac") ) {
    Covid19_noNAs$Sinovac_new[r] <- 1
  }
  else {
    Covid19_noNAs$Sinovac_new[r] <- 0
  }
}

str(Covid19_noNAs$Sinovac_new)
table(Covid19_noNAs$Sinovac_new)

# If any vaccine value includes "Sinopharm/Wuhan" this new column will be set to 1, otherwise 0

Covid19_noNAs[  , "Sinopharm_Wuhan_new"] = NA

for (w in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$vaccines[w] %in% c( "Oxford/AstraZeneca, Pfizer/BioNTech, Sinopharm/Beijing, Sinopharm/Wuhan, Sputnik V" , "Sinopharm/Beijing, Sinopharm/Wuhan, Sinovac") ) {
    Covid19_noNAs$Sinopharm_Wuhan_new[w] <- 1
  }
  else {
    Covid19_noNAs$Sinopharm_Wuhan_new[w] <- 0
  }
}

str(Covid19_noNAs$Sinopharm_Wuhan_new)
table(Covid19_noNAs$Sinopharm_Wuhan_new)

# Here is an example for filtering before graphing

plot_data <- Covid19_noNAs  %>%
  filter(country %in% c("China", "India", "United States of America"))

ggplot(plot_data, aes(x = people_vaccinated_per_hundred, y = Urban_population , color = country)) +
  geom_point()

#Imputing for population size

Covid19_noNAs[  , "Pop_size_indicator"] = NA

for (i in 1:nrow(Covid19_noNAs)) {
  if (Covid19_noNAs$population[i] > 200000 ) {
    Covid19_noNAs$ Pop_size_indicator[i] <- "Large"
  }
  else if (Covid19_noNAs$population[i] > 40000 ) {
    Covid19_noNAs$ Pop_size_indicator[i] <- "Medium"
  }
  else if (covid19_copy$population[i] > 5000 ) {
    Covid19_noNAs$ Pop_size_indicator[i] <- "Small"
  }
  else {
    Covid19_noNAs$ Pop_size_indicator[i] <- "Island"
  }
}


Covid19_noNAs$Pop_size_indicator = as.factor(Covid19_noNAs$Pop_size_indicator)

str(Covid19_noNAs)

# SCATTERPLOTS
# What is the relationship between GDP per Capita and other numerical variables, 
# and how does this relationship differ based on country and region?



str(Covid19_noNAs)
ggplot(Covid19_noNAs, aes(x = people_vaccinated, y = Health_expenditure, color = Covid19_noNAs$Pop_size_indicator)) + geom_point() +
  xlab(label = "People Vaccinated") + 
  ylab(label = "Health Expenditure") +
  ggtitle(label = "Scatterplot of People Vaccinated and Health Expenditure" ) +
  facet_wrap(~Covid19_noNAs$Pop_size_indicator)


#str(Covid19_noNAs)
#ggplot(Covid19_noNAs, aes(x = people_vaccinated, y = Health_expenditure, color = Covid19_noNAs$Pop_size_indicator, )) + geom_point() +
 # xlab(label = "People Vaccinated") + 
  #ylab(label = "Health Expenditure") +
  #ggtitle(label = "Scatterplot of People Vaccinated and Health Expenditure" )
  #facet_wrap(~Covid19_noNAs$Pop_size_indicator)
  
ggplot(Covid19_noNAs, aes(x = people_vaccinated, y = Individuals_using_internet, color = Covid19_noNAs$Population_age_0_14, )) + geom_point() +
    xlab(label = "People Vaccinated") + 
    ylab(label = "Individuals Using Internet") +
    ggtitle(label = "Scatterplot of People Vaccinated and Individuals Using Internet" ) 

+
    facet_wrap(~Covid19_noNAs$Pop_size_indicator)


# HISTOGRAMS
library(ggplot2)

ggplot(Covid19_noNAs, aes(x = Urban_population , fill = people_vaccinated, color = Pop_size_indicator))  + geom_histogram(bins = 10) + xlab(label = "Urban Population Size") + ggtitle(label ="Histogram of Urban Population by People Vaccinated")
ggplot(Covid19_noNAs, aes(x = Health_expenditure , fill = Individuals_using_internet, color = Pop_size_indicator))  + geom_histogram(bins = 10) + xlab(label = "Health Expenditure") + ggtitle(label ="Histogram of Health Expenditure by Individuals using Internet")
ggplot(Covid19_noNAs, aes(x = Individuals_using_internet , fill = people_vaccinated, color = Pop_size_indicator))  + geom_histogram(bins = 10) + xlab(label = "Individuals Using Internet") + ggtitle(label ="Histogram of Individuals Using Internet by People Vaccinated")



#Barplot of GDP of Large Country
plot_data1 <- Covid19_noNAs %>%
  filter(Pop_size_indicator %in% c("Large"))

ggplot(plot_data1, aes(x = region, y = GDP_per_capita)) +
  geom_bar(stat = "identity", color="gray50", alpha = 0.7, size = 0.5) + theme_bw() +
  xlab(label = "Region") + 
  ylab(label = "GDP per Capita") +
  ggtitle(label = "BoxPlot of Region and GDP" )
facet_wrap(~Covid19_noNAs$Pop_size_indicator)

ggplot(plot_data1, aes(x = region, y = GDP_per_capita)) +
  geom_bar(stat = "identity", color="gray50", alpha = 0.7, size = 0.5) + theme_bw() +
  xlab(label = "Region") + 
  ylab(label = "GDP per Capita") +
  ggtitle(label = "BarPlot of Region and GDP" ) +
  facet_wrap(~plot_data1$Pop_size_indicator)



#Barplot of GDP of Medium Country
plot_data2 <- Covid19_noNAs %>%
  filter(Pop_size_indicator %in% c("Medium"))


ggplot(plot_data2, aes(x = region, y = GDP_per_capita)) +
  geom_bar(stat = "identity", color="gray50", alpha = 0.7, size = 0.5) + theme_bw() +
  xlab(label = "Region") + 
  ylab(label = "GDP per Capita") +
  ggtitle(label = "BarPlot of Region and GDP" ) +
  facet_wrap(~plot_data2$Pop_size_indicator)


#Barplot of GDP for small country
plot_data3 <- Covid19_noNAs %>%
  filter(Pop_size_indicator %in% c("Small"))

ggplot(plot_data3, aes(x = region, y = GDP_per_capita)) +
  geom_bar(stat = "identity", color="blue", alpha = 0.7, size = 0.5) + theme_bw() +
  xlab(label = "Region") + 
  ylab(label = "GDP per Capita") +
  ggtitle(label = "BarPlot of Region and GDP" ) +
  facet_wrap(~plot_data3$Pop_size_indicator)

#Barplot of "Island"

plot_data4 <- Covid19_noNAs %>%
  filter(Pop_size_indicator %in% c("Island"))

ggplot(plot_data4, aes(x = region, y = GDP_per_capita)) +
  geom_bar(stat = "identity", color="gray50", alpha = 0.7, size = 0.5) + theme_bw() +
  xlab(label = "Region") + 
  ylab(label = "GDP per Capita") +
  ggtitle(label = "BarPlot of Region and GDP" ) +
  facet_wrap(~plot_data4$Pop_size_indicator)


#Comparing GDP and vacciantion based off region

barplot_data1 <- Covid19_noNAs %>%
  filter(Pop_size_indicator %in% c("Large"))

ggplot(barplot_data1, aes(x = people_vaccinated, y = GDP_per_capita)) + geom_point() +
  xlab(label = "People Vaccinated") + 
  ylab(label = "GDP per Capita") +
  ggtitle(label = "Scatterplot of People Vaccinated and GDP" )




#NEW PLOT

featurePlot(x = plot_data1[ , c("CO2_emission", "Health_expenditure")], 
            y = plot_data1$population, 
            plot = "scatter",
            labels = c("Explanatory Variables", "Population"),
            layout = c(2, 1))










