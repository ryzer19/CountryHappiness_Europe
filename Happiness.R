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

  #(Darragh)
countries_income <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/countries.csv')
year1 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year1.csv')
year2 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year2.csv')
cluster_data2015 <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2015.csv", header = TRUE, row.names = 1, sep = ",")
cluster_data2016 <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2016.csv", header = TRUE, row.names = 1, sep = ",")
cluster_data2017 <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2017.csv", header = TRUE, row.names = 1, sep = ",")


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
    joined_df <- joined_df[, c(9,1,2,3,4,5,6,7,8,10,11)]
    
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

              #creating data frame with happiness score & annual income figures
                  #reading in 3 year income dataset
                      income_3year <- read.csv("/Users/ryanjohnston/development/r/datamining/income_3years.csv")
                      
                      income_3year <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/income_3years.csv")
                  #reading in happiness 3 years
                      happiness2015 <- read.csv('/Users/ryanjohnston/development/r/datamining/2015.csv')
                      happiness2016 <- read.csv('/Users/ryanjohnston/development/r/datamining/2016.csv')
                      happiness2017 <- read.csv('/Users/ryanjohnston/development/r/datamining/2017.csv')
                      
                      happiness2015 <- read.csv('C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/2015.csv')
                      happiness2016 <- read.csv('C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/2016.csv')
                      happiness2017 <- read.csv('C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/2017.csv')
                      
                            #joining happiness years into 1 dataframe
                                #joining 2015 & 2016
                                happiness <- left_join(happiness2015, happiness2016, 
                                                       by = c("Country" = "Country"))
                                #joining 2015+16 & 2017
                                happiness <- left_join(happiness, happiness2017, 
                                                       by = c("Country" = "Country"))
                                #removes data frames - not needed anymore
                                rm(happiness2015,happiness2016,happiness2017)
                                
                                #removing unnecessary columns from 'happiness' data frame
                                happiness <- happiness[ , -c(3,5,6,7,8,9,10,11,
                                                             12,13,14,16,17,18,19,
                                                             20,21,22,23,24,25,27,
                                                             28,29,30,31,32,33,34,35)]
                                
                                #creates criteria variable which stores two strings
                                criteria <- c("Central and Eastern Europe", "Western Europe")
                                     #deletes everything that doesn't contain the two strings in the criteria variable
                                     happiness <- happiness[happiness$Region.x %in% criteria, ]
                                    
                                     #joining happiness for 3 years & income for 3 years
                                     hap_inc <- left_join(happiness, income_3year, 
                                                            by = c("Country" = "Country"))
                                           #removing rows that contain no income values
                                            hap_inc <- hap_inc[-c(12,14,18,22,23,29,32,34,35,46,48,49),]
                                            
                                    #putting country code at start of data frame
                                    hap_inc <- hap_inc[, c(6,1,2,3,4,5,7,8,9,10,11,12)]
                                    
                                    #removing columns
                                    hap_inc <- hap_inc[ , -c(3,7,8,9)]
                                    
                                    #renaming columns
                                    names(hap_inc)[1:8] <- c("Country_ID","Country","Happiness_Score2015",
                                                                       "Happiness_Score2016","Happiness_Score2017",
                                                                       "Annual_Gross2015","Annual_Gross2016","Annual_Gross2017")
                                    
    #Cluster data frame creation (run once then read in at the top)
        #2015
        vars <- c("Country", "Happiness_Score2015", "Annual_Gross2015")
        cluster_data2015 <- hap_inc[vars]
        write.csv(cluster_data2015,"C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2015.csv", row.names = FALSE)
        cluster_data2015 <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2015.csv", header = TRUE, row.names = 1, sep = ",")
        
        #2016
        vars <- c("Country", "Happiness_Score2016", "Annual_Gross2016")
        cluster_data2016 <- hap_inc[vars]
        write.csv(cluster_data2016,"C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2016.csv", row.names = FALSE)
        cluster_data2016 <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2016.csv", header = TRUE, row.names = 1, sep = ",")
        
        #2017
        vars <- c("Country", "Happiness_Score2017", "Annual_Gross2017")
        cluster_data2017 <- hap_inc[vars]
        write.csv(cluster_data2017,"C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2017.csv", row.names = FALSE)
        cluster_data2017 <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/cluster_data2017.csv", header = TRUE, row.names = 1, sep = ",")
        
#Visualisations & Data Mining Techniques 
        
  #Map 2015
    happiness_map_2015 = plot_geo(hap_inc,
                               locationmode = 'europe') %>%
        
        add_trace (locations = ~Country_ID,
                     text = paste0('Country: ', hap_inc$Country, '<br>Happiness Score: ', hap_inc$Happiness_Score2015, '<br>Annual Income: ', hap_inc$Annual_Gross2015),
                     hoverinfo = 'text',
                     z = ~Happiness_Score2015,
                     zmin = 0,
                     zmax = max(hap_inc$Happiness_Score2015),
                     color = ~Happiness_Score2015,
                     col = 'heat') %>%
          
        layout(geo = list(scope = 'europe'),
                 title = "Country's With The Highest Happiness Score 2015") %>%
          
        config(displayModeBar = FALSE)
        
    happiness_map_2015
    
  #Map 2016
    happiness_map_2016 = plot_geo(hap_inc,
                               locationmode = 'europe') %>%
      
      add_trace (locations = ~Country_ID,
                 text = paste0('Country: ', hap_inc$Country, '<br>Happiness Score: ', hap_inc$Happiness_Score2016, '<br>Annual Income: ', hap_inc$Annual_Gross2016),
                 hoverinfo = 'text',
                 z = ~Happiness_Score2016,
                 zmin = 0,
                 zmax = max(hap_inc$Happiness_Score2016),
                 color = ~Happiness_Score2016,
                 col = 'heat') %>%
      
      layout(geo = list(scope = 'europe'),
             title = "Country's With The Highest Happiness Score 2016") %>%
      
      config(displayModeBar = FALSE)
    
    happiness_map_2016
    
  #Map 2017
    happiness_map_2017 = plot_geo(hap_inc,
                               locationmode = 'europe') %>%
      
      add_trace (locations = ~Country_ID,
                 text = paste0('Country: ', hap_inc$Country, '<br>Happiness Score: ', hap_inc$Happiness_Score2017, '<br>Annual Income: ', hap_inc$Annual_Gross2017),
                 hoverinfo = 'text',
                 z = ~Happiness_Score2017,
                 zmin = 0,
                 zmax = max(hap_inc$Happiness_Score2017),
                 color = ~Happiness_Score2017,
                 col = 'heat') %>%
      
      layout(geo = list(scope = 'europe'),
             title = "Country's With The Highest Happiness Score 2017") %>%
      
      config(displayModeBar = FALSE)
    
    happiness_map_2017
    
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
        
  #Kmeans Cluster Plot 2015
    k2 <- kmeans(cluster_data2015, centers = 2, nstart = 25)
    fviz_cluster(k2, data = cluster_data2015)
    
    k3 <- kmeans(cluster_data2015, centers = 3, nstart = 25)
    k4 <- kmeans(cluster_data2015, centers = 4, nstart = 25)
    k5 <- kmeans(cluster_data2015, centers = 5, nstart = 25)
    
    p1 <- fviz_cluster(k2, geom = "point", data = cluster_data2015) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = cluster_data2015) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = cluster_data2015) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = cluster_data2015) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    wssplot <- function(data, max_clusters=15) {
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (k in 2:max_clusters){
        wss[k] <- sum(kmeans(data, centers=k)$withinss)
      }
      plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    }
    
    set.seed(42)
    wssplot(cluster_data2015,10)
    
  #Best number of centers
    k2 <- kmeans(cluster_data2015, centers = 3, nstart = 25)
    fviz_cluster(k2, data = cluster_data2015, main = "Cluster Plot 2015")
    
  #Kmeans Cluster Plot 2016
    k2 <- kmeans(cluster_data2016, centers = 2, nstart = 25)
    fviz_cluster(k2, data = cluster_data2016)
    
    k3 <- kmeans(cluster_data2016, centers = 3, nstart = 25)
    k4 <- kmeans(cluster_data2016, centers = 4, nstart = 25)
    k5 <- kmeans(cluster_data2016, centers = 5, nstart = 25)
    
    p1 <- fviz_cluster(k2, geom = "point", data = cluster_data2016) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = cluster_data2016) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = cluster_data2016) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = cluster_data2016) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    wssplot <- function(data, max_clusters=15) {
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (k in 2:max_clusters){
        wss[k] <- sum(kmeans(data, centers=k)$withinss)
      }
      plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    }
    
    set.seed(42)
    wssplot(cluster_data2016,10)
    
  #Best number of centers
    k2 <- kmeans(cluster_data2016, centers = 3, nstart = 25)
    fviz_cluster(k2, data = cluster_data2016, main = "Cluster Plot 2016")
    
  #Kmeans Cluster Plot 2017
    k2 <- kmeans(cluster_data2017, centers = 2, nstart = 25)
    fviz_cluster(k2, data = cluster_data2017)
    
    k3 <- kmeans(cluster_data2017, centers = 3, nstart = 25)
    k4 <- kmeans(cluster_data2017, centers = 4, nstart = 25)
    k5 <- kmeans(cluster_data2017, centers = 5, nstart = 25)
    
    p1 <- fviz_cluster(k2, geom = "point", data = cluster_data2017) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = cluster_data2017) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = cluster_data2017) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = cluster_data2017) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    wssplot <- function(data, max_clusters=15) {
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (k in 2:max_clusters){
        wss[k] <- sum(kmeans(data, centers=k)$withinss)
      }
      plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    }
    
    set.seed(42)
    wssplot(cluster_data2017,10)
    
  #Best number of centers
    k2 <- kmeans(cluster_data2017, centers = 3, nstart = 25)
    fviz_cluster(k2, data = cluster_data2017, main = "Cluster Plot 2017")