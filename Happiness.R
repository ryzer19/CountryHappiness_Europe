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
    
    #gets average values & creates dataframes
    average_happiness <- (joined_df[4] + joined_df[13])/2
    average_gdp <- (joined_df[5] + joined_df[14])/2
    average_family <- (joined_df[6] + joined_df[15])/2
    average_lifeexpectancy <- (joined_df[7] + joined_df[16])/2
    average_freedom <- (joined_df[8] + joined_df[17])/2
    average_corruption <- (joined_df[9] + joined_df[18])/2
    average_generosity <- (joined_df[10] + joined_df[19])/2
    
    #adds country to averages dataframes
    average_happiness$Country <- paste(joined_df$Country)
    average_gdp$Country <- paste(joined_df$Country)
    average_family$Country <- paste(joined_df$Country)
    average_lifeexpectancy$Country <- paste(joined_df$Country)
    average_freedom$Country <- paste(joined_df$Country)
    average_corruption$Country <- paste(joined_df$Country)
    average_generosity$Country <- paste(joined_df$Country)
    
    #adds the average happiness into the column that used to have the happiness for 1 year
    joined_df$Happiness_Score.x <- paste(average_happiness$Happiness_Score)
    joined_df$GDP_Per_Capita.x <- paste(average_gdp$GDP_Per_Capita.x)
    joined_df$Family.x <- paste(average_family$Family.x)
    joined_df$Life_Expectancy.x <- paste(average_lifeexpectancy$Life_Expectancy.x)
    joined_df$Freedom.x <- paste(average_freedom$Freedom.x)
    joined_df$Corruption_Perception.x <- paste(average_corruption$Corruption_Perception.x)
    joined_df$Generosity.x <- paste(average_generosity$Generosity.x)
    
    #countries income & country code added to joined_df
    joined_df <- left_join(joined_df, countries_income, 
                           by = c("Country" = "Country"))
    
    #delete rows 11-19 on joined df country frame as they are not needed anymore
    joined_df <- select(joined_df, -11:-19)
    
    #reorder to put country code beside country
    joined_df <- joined_df[, c(11,1,2,3,4,5,6,7,8,9,10,12,13)]
    
    #RENAMING
        #changing dataframe name
        happiness_factors <- joined_df
        
        #changing column names of finalised data frame - happiness_factors
        names(happiness_factors)[1:13] <- c("Country_ID","Country","Region",
                                "Happiness_Rank","Happiness_Score","GDP_PerCapita",
                                "Family","Life_Expectancy","Freedom","Corruption_Perception","Generosity", "Monthly_Net", "Annual_Net")
        

    