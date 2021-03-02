install.packages("dplyr")
library(dplyr)
library(data.table)


#reading datasets in
countries_income <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/countries.csv')
year1 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year1.csv')
year2 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year2.csv')

#removing areas not in europe or income dataset 
    countries_income <- countries_income[-c(16,28,30,31,42,44,45), ]
    year1 <- year1[-c(48,35,14,49,22,34,29,46,32,12,18),]
    year2 <- year2[-c(47,36,16,49,21,37,27,45,29,13,19),]
    
      #changing column names year1 & year2
    names(year1)[1:10] <- c("Country","Region","Happiness_Rank",
                            "Happiness_Score","GDP_Per_Capita","Family",
                            "Life_Expectancy","Freedom","Corruption_Perception","Generosity")
    names(year2)[1:10] <- c("Country","Region","Happiness_Rank",
                            "Happiness_Score","GDP_Per_Capita","Family",
                            "Life_Expectancy","Freedom","Corruption_Perception","Generosity")
    
    #joins data frame year1 & year2 based off similar 'country' field
    joined_df <- left_join(year1, year2, 
                               by = c("Country" = "Country"))
    

    #gets average values for happiness rating
    average_happiness <- (joined_df[4] + joined_df[13])/2
    
     #adding empty columns
    countries_income[,5] <- NA
    countries_income[,6] <- NA
    countries_income[,7] <- NA
    
    
    
    
    