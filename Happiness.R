install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("readr")
library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(readr)
library(caTools)
library(factoextra)
library(tidyverse)
library(cluster)
library(gridExtra)

#reading datasets

  #(Ryan)
countries_income <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/countries.csv')
year1 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year1.csv')
year2 <- read.csv(file = '/Users/ryanjohnston/development/r/datamining/year2.csv')
cluster_data <- read.csv("/Users/ryanjohnston/development/r/datamining/cluster_data.csv", header = TRUE, row.names = 1, sep = ",")


  #(Darragh)
countries_income <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/countries.csv')
year1 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year1.csv')
year2 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year2.csv')
cluster_data <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data.csv", header = TRUE, row.names = 1, sep = ",")

#Pre-Process Data

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
    
  #delete columns 11-19 on joined df country frame as they are not needed anymore
    joined_df <- select(joined_df, -11:-19)
    joined_df <- joined_df[ , -c(2,3)]
    
  #reorder to put country code beside country
    joined_df <- joined_df[, c(9,1,2,3,4,5,6,7,8,9,10,11)]
    
#Renaming
    
  #changing dataframe name
    happiness_factors <- joined_df
        
  #changing column names of finalised data frame - happiness_factors
    names(happiness_factors)[1:11] <- c("Country_ID","Country","Happiness_Score","GDP_PerCapita",
                                     "Family","Life_Expectancy","Freedom","Corruption_Perception","Generosity", "Monthly_Net", "Annual_Net")
        
  #Changing values from characters to numerical values
    happiness_factors$Happiness_Score <- as.numeric(happiness_factors$Happiness_Score)
    happiness_factors$GDP_PerCapita <- as.numeric(happiness_factors$GDP_PerCapita)
    happiness_factors$Family <- as.numeric(happiness_factors$Family)
    happiness_factors$Life_Expectancy <- as.numeric(happiness_factors$Life_Expectancy)
    happiness_factors$Freedom <- as.numeric(happiness_factors$Freedom)
    happiness_factors$Corruption_Perception <- as.numeric(happiness_factors$Corruption_Perception)
    happiness_factors$Generosity <- as.numeric(happiness_factors$Generosity)
        
#Cluster data frame creation 
    vars <- c("Country", "Happiness_Score", "Annual_Net")
    cluster_data <- happiness_factors[vars]
    write.csv(cluster_data,"C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data.csv", row.names = FALSE)

    cluster_data <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data.csv", header = TRUE, row.names = 1, sep = ",")
    
#creating data frame with happiness score & annual income figures

#reading in 3 year income dataset
    income_3year <- read.csv("/Users/ryanjohnston/development/r/datamining/income_3years.csv", header = TRUE, row.names = 1, sep = ",")
    
    
    
    
    
    
    
    
    
    
    
    
    
        
#Visualisations & Data Mining Techniques 
        
  #Map
    happiness_map = plot_geo(happiness_factors,
                               locationmode = 'europe') %>%
        
        add_trace (locations = ~Country_ID,
                     text = paste0('Country: ', happiness_factors$Country, '<br>Happiness Score: ', happiness_factors$Happiness_Score, '<br>Annual Income: ', happiness_factors$Annual_Net),
                     hoverinfo = 'text',
                     z = ~Annual_Net,
                     zmin = 0,
                     zmax = max(happiness_factors$Annual_Net),
                     color = ~Annual_Net,
                     colorscale = 'heat') %>%
          
        layout(geo = list(scope = 'europe'),
                 title = "Country's With The Highest Annual Income") %>%
          
        config(displayModeBar = FALSE)
        
    happiness_map
        
  #Net income x Happiness score Plot https://www.youtube.com/watch?v=BB2O4VCu5j8
    model_data = subset(happiness_factors, select = c(Happiness_Score, GDP_PerCapita, Family, Life_Expectancy, Freedom, Corruption_Perception, Generosity, Annual_Net))
    
    ggplot(model_data,aes(x=Happiness_Score,y=Annual_Net))+geom_point()+geom_smooth(method = "lm",se=F)
        
  #Model creation & testing
    model_data = subset(happiness_factors, select = c(Happiness_Score, GDP_PerCapita, Family, Life_Expectancy, Freedom, Corruption_Perception, Generosity, Annual_Net))
        
    sample.split(model_data$Happiness_Score, SplitRatio = 0.90) -> split_index     #sample split makes 90% of values true
    train <- subset (model_data,split_index==T) #train is 35 values (90%)
    test <- subset (model_data,split_index==F) #test gives 4 values (10%)
    nrow(train) #number of rows
    nrow(test) #number of rows
        
  #Building models
    lm(Happiness_Score~.,data=train) -> model1
    predict(model1,test)->result
    cbind(actual=test$Happiness_Score,predicted=result)->compare_results
    as.data.frame(compare_results)->compare_results
    compare_results$actual-compare_results$predicted->error
    cbind(compare_results,error)->compare_results
    sqrt(mean(compare_results$error^2))->rmse1
    rmse1
        
    summary(model1)
        
    lm(Happiness_Score~.-Life_Expectancy-Freedom-Corruption_Perception,data=train) -> model2
    predict(model2,test)->result2
    cbind(actual=test$Happiness_Score,predicted=result2)->compare_results2
    as.data.frame(compare_results2)->compare_results2
    compare_results2$actual-compare_results2$predicted->error
    cbind(compare_results2,error)->compare_results2
    sqrt(mean(compare_results2$error^2))->rmse2
    rmse2
        
    summary(model2)
        
  #Kmeans Cluster Plot
    k2 <- kmeans(cluster_data, centers = 2, nstart = 25)
    fviz_cluster(k2, data = cluster_data)
    
    k3 <- kmeans(cluster_data, centers = 3, nstart = 25)
    k4 <- kmeans(cluster_data, centers = 4, nstart = 25)
    k5 <- kmeans(cluster_data, centers = 5, nstart = 25)
    
    p1 <- fviz_cluster(k2, geom = "point", data = cluster_data) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = cluster_data) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = cluster_data) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = cluster_data) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    wssplot <- function(data, max_clusters=15) {
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (k in 2:max_clusters){
        wss[k] <- sum(kmeans(data, centers=k)$withinss)
      }
      plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    }
    
    set.seed(42)
    wssplot(cluster_data,10)
    
  #Best number of centers
    k2 <- kmeans(cluster_data, centers = 2, nstart = 25)
    fviz_cluster(k2, data = cluster_data)