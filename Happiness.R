install.packages("dplyr")
library(dplyr)
library(data.table)

#test2

#reading datasets in (Ryan)
countries_income <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/countries.csv')
year1 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year1.csv')
year2 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year2.csv')

#reading datasets in (Darragh)
countries_income <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/countries.csv')
year1 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year1.csv')
year2 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year2.csv')

#removing areas not in europe or income dataset 
    countries_income <- countries_income[-c(16,28,30,31,42,44,45),]
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
    
    joined_df_countries_added <- left_join(year1, countries_income, 
                           by = c("Country" = "Country"))
    
    #adds country names to average
    joined_df$Country <- paste(joined_df$Country)

    #adds the average happiness into the column that used to have the happiness for 1 year
    joined_df$Happiness_Score.x <- paste(average_happiness$Happiness_Score)
    
    #changes name for column 1 in avg happiness data frame
    names(average_happiness)[1] <- "Happiness_Score"

    #gets average values for happiness rating
    average_happiness <- (joined_df[4] + joined_df[13])/2

    #gets average values for happiness rating
    average_happiness <- (joined_df[4] + joined_df[13])/2
    average_gdp <- (joined_df[5] + joined_df[14])/2
    average_family <- (joined_df[6] + joined_df[15])/2
    average_lifeexpectancy <- (joined_df[7] + joined_df[16])/2
    average_freedom <- (joined_df[8] + joined_df[17])/2
    average_corruption <- (joined_df[9] + joined_df[18])/2
    average_generosity <- (joined_df[10] + joined_df[19])/2
    
    #changes name for column 1 in avg happiness data frame
    names(average_happiness)[1] <- "Happiness_Score"
    
    #adds the average happiness into the column that used to have the happiness for 1 year
    joined_df$Happiness_Score.x <- paste(average_happiness$Happiness_Score)

    #test