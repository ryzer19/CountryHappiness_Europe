#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("plotly")
#install.packages("caTools")
#install.packages("factoextra")
#install.packages("cluster")
#install.packages("gridExtra")
#install.packages("heatmaply")
#install.packages("hrbrthemes")

library(data.table) #allows join/subset functions
library(plotly)     #map visualisation
library(caTools)    #maths functions
library(factoextra) #allows visualisation of cluster output
library(tidyverse)  #ggplot2, dplyr, readr
library(cluster)    #clustering
library(gridExtra)  #allows graphs to be displayed in groups
library(heatmaply)  #allows heatmap to be created
library(hrbrthemes) #themes histogram


#Read in CSV files
countries_income <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/countries.csv')
year1 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year1.csv')
year2 <- read.csv(file = 'C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/year2.csv')


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
                      income_3year <- read.csv("C:/Users/darra/OneDrive/Documents/datamining/countryhappiness_europe/income_3years.csv")
      
                  #reading in happiness factors datasets for 3 years
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
                                
                                #creates criteria variable which stores two strings, to help filter unnecessary data
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
                                    
    #Cluster data frame creation (makes first column country names(column with no name) to allow for country labels on cluster map)
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
        
  #Maps 2015
        
        #Happiness Map
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
                 title = "Country's Happiness Scores 2015") %>%
          
        config(displayModeBar = FALSE)
        
    happiness_map_2015
        
        #Income Map
    
    income_map_2015 = plot_geo(hap_inc,
                                  locationmode = 'europe') %>%
      
      add_trace (locations = ~Country_ID,
                 text = paste0('Country: ', hap_inc$Country, '<br>Happiness Score: ', hap_inc$Happiness_Score2015, '<br>Annual Income: ', hap_inc$Annual_Gross2015),
                 hoverinfo = 'text',
                 z = ~Annual_Gross2015,
                 zmin = 0,
                 zmax = max(hap_inc$Annual_Gross2015),
                 color = ~Annual_Gross2015,
                 col = 'heat') %>%
      
      layout(geo = list(scope = 'europe'),
             title = "Country's average gross annual income 2015") %>%
      
      config(displayModeBar = FALSE)
    
    income_map_2015
    
  #Maps 2016
    
        #Happiness Map
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
             title = "Country's Happiness Scores 2016") %>%
      
      config(displayModeBar = FALSE)
    
    happiness_map_2016
    
        #Income Map
    
    income_map_2016 = plot_geo(hap_inc,
                               locationmode = 'europe') %>%
      
      add_trace (locations = ~Country_ID,
                 text = paste0('Country: ', hap_inc$Country, '<br>Happiness Score: ', hap_inc$Happiness_Score2015, '<br>Annual Income: ', hap_inc$Annual_Gross2015),
                 hoverinfo = 'text',
                 z = ~Annual_Gross2016,
                 zmin = 0,
                 zmax = max(hap_inc$Annual_Gross2016),
                 color = ~Annual_Gross2016,
                 col = 'heat') %>%
      
      layout(geo = list(scope = 'europe'),
             title = "Country's average gross annual income 2016") %>%
      
      config(displayModeBar = FALSE)
    
    income_map_2016
    
  #Maps 2017
    
        #Happiness Map
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
             title = "Country's Happiness Scores 2017") %>%
      
      config(displayModeBar = FALSE)
    
    happiness_map_2017
    
        #Income Map
    income_map_2017 = plot_geo(hap_inc,
                               locationmode = 'europe') %>%
      
      add_trace (locations = ~Country_ID,
                 text = paste0('Country: ', hap_inc$Country, '<br>Happiness Score: ', hap_inc$Happiness_Score2015, '<br>Annual Income: ', hap_inc$Annual_Gross2015),
                 hoverinfo = 'text',
                 z = ~Annual_Gross2017,
                 zmin = 0,
                 zmax = max(hap_inc$Annual_Gross2017),
                 color = ~Annual_Gross2017,
                 col = 'heat') %>%
      
      layout(geo = list(scope = 'europe'),
             title = "Country's average gross annual income 2017") %>%
      
      config(displayModeBar = FALSE)
    
    income_map_2017
        
  #Model creation & testing
    #Data splitting
    model_data = subset(happiness_factors, select = c(Happiness_Score, GDP_PerCapita, Family, Life_Expectancy, Freedom, Corruption_Perception, Generosity, Annual_Net))
    
    set.seed(1290)    
    sample.split(model_data$Happiness_Score, SplitRatio = 0.90) -> split_index #sample split makes 90% of values true
    train <- subset (model_data,split_index==T) #train is 35 values (90%)
    test <- subset (model_data,split_index==F) #test gives 4 values (10%)
    nrow(train) #number of rows
    nrow(test) #number of rows
        
    #Building models - *used to determine which factor has the highest affect on the happiness score* 
        #model 1
        lm(Happiness_Score~.,data=train) -> model1
        predict(model1,test)->result
        cbind(actual=test$Happiness_Score,predicted=result)->compare_results
        as.data.frame(compare_results)->compare_results
        compare_results$actual-compare_results$predicted->error
        cbind(compare_results,error)->compare_results
        sqrt(mean(compare_results$error^2))->rmse1
        rmse1 #root mean square error - closer to 0 means less error and better results
            
        summary(model1) #more stars means higher correlation with the intercept value (happiness score)
            
        #model 2 - less happiness factors
        lm(Happiness_Score~.-Life_Expectancy-Freedom-GDP_PerCapita-Corruption_Perception,data=train) -> model2
        predict(model2,test)->result2
        cbind(actual=test$Happiness_Score,predicted=result2)->compare_results2
        as.data.frame(compare_results2)->compare_results2
        compare_results2$actual-compare_results2$predicted->error
        cbind(compare_results2,error)->compare_results2
        sqrt(mean(compare_results2$error^2))->rmse2
        rmse2 
            
        summary(model2)
        
#Clustering
        
  #Kmeans Cluster Plot 2015
        #clusters created with different number of centers
    k2 <- kmeans(cluster_data2015, centers = 2, nstart = 25)
    fviz_cluster(k2, data = cluster_data2015)
    
    k3 <- kmeans(cluster_data2015, centers = 3, nstart = 25)
    k4 <- kmeans(cluster_data2015, centers = 4, nstart = 25)
    k5 <- kmeans(cluster_data2015, centers = 5, nstart = 25)
    
        #clusters displayed side by side
    p1 <- fviz_cluster(k2, geom = "point", data = cluster_data2015) + ggtitle("k = 2")
    p2 <- fviz_cluster(k3, geom = "point", data = cluster_data2015) + ggtitle("k = 3")
    p3 <- fviz_cluster(k4, geom = "point", data = cluster_data2015) + ggtitle("k = 4")
    p4 <- fviz_cluster(k5, geom = "point", data = cluster_data2015) + ggtitle("k = 5")
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
        #graph created to determine best number of centers
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
    
    #LINEAR REGRESSION
        #fitting linear model (y, x)
        model2015 <- lm(Annual_Gross2015 ~ Happiness_Score2015, data=hap_inc)
        model2016 <- lm(Annual_Gross2016 ~ Happiness_Score2016, data=hap_inc)
        model2017 <- lm(Annual_Gross2017 ~ Happiness_Score2017, data=hap_inc)
        
        #income/happiness scatter plots
          #2015
        summary(hap_inc$Happiness_Score2015)
        
        newx <- seq(4.218, 7.587, by=0.05)
        plot(hap_inc$Happiness_Score2015, hap_inc$Annual_Gross2015, xlab="Happiness Score", ylab="Income", main="2015")
        abline(model2015, col="lightblue", lwd = 2.5)
        
        conf_interval <- predict(model2015, newdata=data.frame(Happiness_Score2015=newx), interval="confidence",
                                 level = 0.95)
        lines(newx, conf_interval[,2], col="blue", lty=2)
        lines(newx, conf_interval[,3], col="blue", lty=2)
        
        pred_interval <- predict(model2015, newdata=data.frame(Happiness_Score2015=newx), interval="prediction",
                                 level = 0.95)
        lines(newx, pred_interval[,2], col="orange", lty=2)
        lines(newx, pred_interval[,3], col="orange", lty=2)
        
          #2016
        summary(hap_inc$Happiness_Score2016)
        
        newx <- seq(4.217, 7.526, by=0.05)
        plot(hap_inc$Happiness_Score2016, hap_inc$Annual_Gross2016, xlab="Happiness Score", ylab="Income", main="2016")
        abline(model1, col="pink", lwd = 2.5)
        
        conf_interval <- predict(model2016, newdata=data.frame(Happiness_Score2016=newx), interval="confidence",
                                 level = 0.95)
        lines(newx, conf_interval[,2], col="blue", lty=2)
        lines(newx, conf_interval[,3], col="blue", lty=2)
        
        pred_interval <- predict(model2016, newdata=data.frame(Happiness_Score2016=newx), interval="prediction",
                                 level = 0.95)
        lines(newx, pred_interval[,2], col="orange", lty=2)
        lines(newx, pred_interval[,3], col="orange", lty=2)
        
          #2017
        summary(hap_inc$Happiness_Score2017)
        
        newx <- seq(4.096, 7.537, by=0.05)
        plot(hap_inc$Happiness_Score2017, hap_inc$Annual_Gross2017, xlab="Happiness Score", ylab="Income", main="2017")
        abline(model1, col="yellow", lwd = 2.5)
        
        conf_interval <- predict(model2017, newdata=data.frame(Happiness_Score2017=newx), interval="confidence",
                                 level = 0.95)
        lines(newx, conf_interval[,2], col="blue", lty=2)
        lines(newx, conf_interval[,3], col="blue", lty=2)
        
        pred_interval <- predict(model2017, newdata=data.frame(Happiness_Score2017=newx), interval="prediction",
                                 level = 0.95)
        lines(newx, pred_interval[,2], col="orange", lty=2)
        lines(newx, pred_interval[,3], col="orange", lty=2)
                  
            #all time regression (2015,2016 & 2017)
              regression_3years <- data.frame(
                score = c(hap_inc$Happiness_Score2015,hap_inc$Happiness_Score2016,hap_inc$Happiness_Score2017),
                income = c(hap_inc$Annual_Gross2015, hap_inc$Annual_Gross2016, hap_inc$Annual_Gross2017)
              )
              
            #scatterplot with lines produced from linear modelling - shows all 3 years of data
              plot(regression_3years$score, regression_3years$income, main="All Time - Regression",
                xlab = "Happiness Score", ylab = "Income"
              ) + abline(model2015, col=5, lwd=2) + abline(model2016, col=6, lwd=2) + abline(model2017, col=7, lwd=2)
              
            #correlation between happiness & gross income for the year
              #closer to 1 indicates strong(high impact on value) correlation
              cor(hap_inc$Happiness_Score2015, hap_inc$Annual_Gross2015)
              cor(hap_inc$Happiness_Score2016, hap_inc$Annual_Gross2016)
              cor(hap_inc$Happiness_Score2017, hap_inc$Annual_Gross2017)
            

        #histogram
                #dataframe creation
                hist3Years <- data.frame(
                  type = c( rep("2017"), rep("2016"), rep("2015")),
                  value = c(hap_inc$Happiness_Score2017, hap_inc$Happiness_Score2016, hap_inc$Happiness_Score2015)
                )
                
              
                #histogram showing frequency of happiness rates
                ggplot(hist3Years, aes(x = value, fill = type)) +  theme_ipsum() +             
                  geom_histogram(position = "identity", alpha = 0.5, bins = 13)
                
                
                #happiness factors heatmap (related to model 2) - used to show correlation between high impact factors
                mat <- happiness_factors
                rownames(mat) <- mat[,2]
                mat <- mat %>% dplyr::select(-Country, -GDP_PerCapita, -Country_ID, -Life_Expectancy, -Freedom, -Monthly_Net, -Corruption_Perception)
                mat <- as.matrix(mat)
                
                #plotting heat map
                heat2015 <- heatmaply(mat,
                                      dendrogram = "none",
                                      xlab = "", ylab = "", 
                                      main = "",
                                      scale = "column",
                                      margins = c(60,100,40,20),
                                      grid_color = "white",
                                      grid_width = 0.00001,
                                      titleX = FALSE,
                                      hide_colorbar = TRUE,
                                      branches_lwd = 0.1,
                                      label_names = c("Country","Column" ,"Value"),
                                      fontsize_row = 5, fontsize_col = 5,
                                      labCol = colnames(mat),
                                      labRow = rownames(mat),
                                      heatmap_layers = theme(axis.line=element_blank()
                                      ))
                #shows heatmap
                heat2015
                
