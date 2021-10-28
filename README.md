# Data Science Portfolio

Repository containing portfolio of data science projects completed by me for academic and self-learning purposes. 

## Contents

### Machine Learning

[Prediction of Spotify Song’s Popularity and EDA](https://github.com/tatiana-norkina/data-science-portfolio/blob/main/Prediction%20of%20Spotify%20Song%E2%80%99s%20Popularity/main.ipynb) 

Goal: To draw meaningful patterns and insights from a data set with various song attributes, and build the most accurate prediction model of song popularity.

Solution: 170k Spotify songs were merged with key song attributes such as their valence, acousticness, energy etc. The correlation between variables was examined and machine learning algorithms were used to overperform a benchmark simple regression model.

Result: Exploratory Data Analysis has revealed that over time songs have become shorter due to financial incentives of the streaming market. Today danceability and energy of songs are key song features which determine the success on Spotify. For the prediction task completion, the lowest RMSE was achieved by implementation of Random Forest algorithm among other algorithms.    

[Prediction of Cross-Selling Opportunities of a Bank](https://github.com/tatiana-norkina/data-science-portfolio/blob/main/Prediction%20of%20Cross-Selling%20Opportunities%20of%20a%20Bank/main.R)

Goal: To build an accurate predictive model in order to correctly classify customers and therefore increase effectiveness of direct marketing of a commercial bank.

Solution: The XGBoost method was implemented on a bank data set with information on bank account openings as a dependent variable and various customer features. 

Result: The hyperparameter-tuned model could outperform the accuracy of the Random Forest used as a benchmark for comparison. Through feature importance analysis it was found that specific customer characteristics have a strong influence on the prediction (customer activity, age, product ads).

