# Data Science Portfolio

Repository containing portfolio of data science projects completed by me for academic and self learning purposes. 

## Contents

### Machine Learning

[Prediction of Check-In Behavior for the Restaurant Category](https://github.com/tatiana-norkina/data-science-portfolio/blob/69fa6224ab9ea08d158c15f14b44c45f1efd2516/Prediction%20of%20Check-In%20Behavior/DailyLevelData_analysis_short.r) 

Goal: Restaurants need to know what internal and external factors drive customer behavior. For that, it is needed to accurately predict whether a consumer will visit a restaurant and to determine the most important variables influencing his behavior.

Solution: 29k reviews with check-in information and various attributes were collected from the Yelp website and combined with information on daily weather conditions. Handling outliers and missing values, treating imbalance data and data normalization were considered in order to use ML algorithms and increase model performance.

Result: The Boosting algorithm with the highest TDL and GINI coefficient among other ML algorithms found that the volume of online reviews of a restaurant significantly affects the number of restaurant visits. Also, the online popularity of a reviewer influences visiting behavior. With this information restaurant owners can consider the refinements of their digital strategies I order to increase restaurant visits.


[Prediction of Spotify Song’s Popularity and EDA](https://github.com/tatiana-norkina/data-science-portfolio/blob/69fa6224ab9ea08d158c15f14b44c45f1efd2516/Prediction%20of%20Check-In%20Behavior/DailyLevelData_analysis_short.r) 

Goal: To draw meaningful patterns and insights from a data set with various song attributes, and build the most accurate prediction model of song popularity.

Solution: 170k Spotify songs were merged with key song attributes such as their valence, acousticness, energy etc. The correlation between variables was examined and machine learning algorithms were used to overperform a benchmark simple regression model.

Result: Exploratory Data Analysis has revealed that over time songs have become shorter due to financial incentives of the streaming market. Today danceability and energy of songs are key song features which determine the success on Spotify. For the prediction task completion, the lowest RMSE was achieved by implementation of Random Forest algorithm among other algorithms.    

[Prediction of Cross-Selling Opportunities of a Bank!](https://github.com/tatiana-norkina/data-science-portfolio/blob/69fa6224ab9ea08d158c15f14b44c45f1efd2516/Prediction%20of%20Check-In%20Behavior/DailyLevelData_analysis_short.r) 

Goal: To build an accurate predictive model in order to correctly classify customers and therefore increase effectiveness of direct marketing of a bank.

Solution: The XGBoost method was implemented on an ING bank data set with information on bank account openings as a dependent variable and various customer features. 

Result: The hyperparameter-tuned model could outperform the accuracy of the Random Forest used as a benchmark for comparison. Through feature importance analysis it was found that specific customer characteristics have a strong influence on the prediction (customer activity, age, product ads).






