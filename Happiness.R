install.packages("dplyr")
library(dplyr)
library(data.table)


#reading datasets in
countries_income <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/countries.csv')
year1 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year1.csv')
year2 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year2.csv')

#removing areas not in europe
    countries_income <- countries_income[-c(16,28,30,31,42,44,45), ]
    year1 <- year1[-c(18,32,34,35,46,48,49),]
    year2 <- year2[-c(19,29,36,37,45,47,49),]
    
    #removing areas not in income dataset
    year1 <- year1[-c(14,21,29,12),]
    year2 <- year2[-c(16,22,27,13),]
    
    #getting mean values for 5 years -
    
    #adding empty columns
    countries_income[,5] <- NA
    countries_income[,6] <- NA
    countries_income[,7] <- NA
    
    #changing new column names
    names(countries_income)[5] <- "Happiness_Score"

    
    
    
    