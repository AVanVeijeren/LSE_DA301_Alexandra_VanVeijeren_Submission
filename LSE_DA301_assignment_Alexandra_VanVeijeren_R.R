## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################
# Reset the working directory
setwd('C:/Users/Alexandra/OneDrive/LSE Data Analytics/Course 3/Assignment 3/LSE_DA301_assignment_files')

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)
library(ggplot2)
# Load and install the dataframe
sales <- read.csv(file.choose(), header=T)
dim(sales)
head(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)
dim(sales2)

# View the data frame.
head(sales2)

# View the descriptive statistics.
summary(sales2)

## Product should be categorical (Factor)

# Transform Product into a factor variable
sales2 <- mutate(sales2, Product_factor = as_factor(Product))

# Sense check 
dim(sales2)
head(sales2)
summary(sales2)

# Check for missing values
sum(is.na(sales2))

## No missing values.

################################################################################

# 2. Review plots to determine insights into the data set.

# Visualize categorical variables
qplot(Product_factor, data=sales2)
qplot(Platform, fill= Product_factor, data=sales2)

## 2a) Scatterplots
# What is the relationship between global, EU and NA sales
qplot(Global_Sales, NA_Sales, data=sales2, geom=c('point', 'smooth'))

qplot(Global_Sales, EU_Sales, data=sales2, geom=c('point', 'smooth'))

qplot(EU_Sales, NA_Sales, data=sales2, geom=c('point', 'smooth'))


## One clear outlier. 
sales2 %>% summarise_if(is.numeric, max)
which.max(sales2[,5])

# Which product is this? 
sales2[1,1]

## 2b) Histograms
# Create histograms.
qplot(Global_Sales, data=sales2)
qplot(NA_Sales, data=sales2)
qplot(EU_Sales, data=sales2)

## 2c) Boxplots
# Create boxplots and visualize the distribution of numerical variables.
qplot(NA_Sales, data=sales2, geom='boxplot')
qplot(EU_Sales, data=sales2, geom='boxplot')
qplot(Global_Sales, data=sales2, geom='boxplot')


## The outliers in the data may not be erroneous, because they make
## sense if you consider that Global Sales = EU + NA + 'other'. 
## Where NA is large, so is EU for the outliers. 

# Check if outliers skew the data by checking kurtosis and skewness
install.packages('moments')
library('moments')

skewness(sales2$Global_Sales)
kurtosis(sales2$Global_Sales)

skewness(sales2$NA_Sales)
kurtosis(sales2$NA_Sales)

skewness(sales2$EU_Sales)
kurtosis(sales2$EU_Sales)

## The data is highly rightly skewed with large excess kurtosis, 
## indicating that the distribution for all three sales columns
## has much heavier tails than normal distribution. Outliers are 
## removed to avoid the analysis being skewed. 

# Store the data frame before removal of outliers
write.csv(sales2, 'sales_out.csv', row.names=F)

# Remove outliers
Q1 <- quantile(sales2$Global_Sales, .25)
Q3 <- quantile(sales2$Global_Sales, .75)
IQR <- Q3-Q1

sales_clean <- subset(sales2, sales2$Global_Sales> (Q1 - 1.5*IQR) & sales2$Global_Sales < (Q3 + 1.5*IQR))

Q1_na <- quantile(sales_clean$NA_Sales, 0.25)
Q3_na <- quantile(sales_clean$NA_Sales, 0.75)
IQR_na <- Q3 - Q1

sales_clean <- subset(sales_clean, sales_clean$NA_Sales> (Q1_na - 1.5*IQR_na) & sales_clean$NA_Sales < (Q3_na + 1.5*IQR_na))

Q1_eu <- quantile(sales_clean$EU_Sales, .25)
Q3_eu <- quantile(sales_clean$EU_Sales, .75)
IQR_eu <- Q3-Q1

sales_clean <- subset(sales_clean, sales_clean$EU_Sales> (Q1_eu - 1.5*IQR_eu) & sales_clean$EU_Sales < (Q3_eu + 1.5*IQR_eu))

# Sense check 
dim(sales_clean)

# Check the distribution of the cleaned data
qplot(NA_Sales, data=sales_clean, geom='boxplot')
qplot(EU_Sales, data=sales_clean, geom='boxplot')
qplot(Global_Sales, data=sales_clean, geom='boxplot')

# Check the relationship of the cleaned data
qplot(Global_Sales,NA_Sales, data=sales_clean, geom=c('point', 'smooth'))
qplot(Global_Sales,EU_Sales, data=sales_clean, geom=c('point', 'smooth'))
qplot(EU_Sales,NA_Sales, data=sales_clean, geom=c('point', 'smooth'))

# Export the cleaned data to use later
write.csv(sales_clean, "sales_clean.csv")

## Decision regarding outliers: 
## The goal of the analysis is to get a picture of the true relationship 
## between NA, EU and Global sales. 
## Removing the outliers in this data may distort the relationship 
## and generate results that show the relationship to be more linear 
## than it seems. 

## I will not be removing outliers going forward. 
###############################################################################

# 3. Determine the impact on sales per product_id.

dim(sales2)

# Add a difference col to calc the difference in sales per product in NA vs EU.
sales_2 <- mutate(sales2, diff_NA_EU = sales2$NA_Sales - sales2$EU_Sales)
# Plot diff_NA_EU to get a sense of the data
qplot(diff_NA_EU, data=sales_2)
qplot(diff_NA_EU, data=sales_2, geom='boxplot')

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
Global_ <- aggregate(sales_2$Global_Sales, by=list(sales_2$Product_factor), FUN=sum)
NA_ <- aggregate(sales_2$NA_Sales, by=list(sales_2$Product_factor), FUN=sum)
EU_ <-aggregate(sales_2$EU_Sales, by=list(sales_2$Product_factor), FUN=sum)

# Sense check groupings
head(Global_)
head(NA_)
head(EU_)

sales_byProduct <-cbind(Global_, NA_$x, EU_$x) 
head(sales_byProduct)
colnames(sales_byProduct) <- c('Product_Id', 'global_sales', 'na_sales', 'eu_sales')

# View the data frame.
head(sales_byProduct)

# Explore the data frame.
summary(sales_byProduct)
qplot(global_sales, data=sales_byProduct, geom='boxplot')
qplot(na_sales, data=sales_byProduct, geom='boxplot')
qplot(eu_sales, data=sales_byProduct, geom='boxplot')

# Find the proportion of global sales that eu and na make up
sales_byProduct <- mutate(sales_byProduct, na_prop = (na_sales / global_sales)*100)
sales_byProduct <- mutate(sales_byProduct, eu_prop = (eu_sales / global_sales)*100)
head(sales_byProduct)

# Which products had the highest sales amount globally?  
sales_byProduct_global <- arrange(sales_byProduct, desc(global_sales))
head(sales_byProduct_global)
top_ten_global <- sales_byProduct_global[1:10,]
top_ten_global

# In the EU? 
sales_byProduct_eu <- arrange(sales_byProduct, desc(eu_sales))
top_ten_eu <- sales_byProduct_eu[1:10,]
top_ten_eu

# In NA? 
sales_byProduct_na <- arrange(sales_byProduct, desc(na_sales))
top_ten_na <- sales_byProduct_na[1:10,]
top_ten_na

# Which products had the lowest sales amount globally?  
dim(sales_byProduct_global)
bottom_ten_global <- sales_byProduct_global[166:175,]
bottom_ten_global

# In the EU? 
dim(sales_byProduct_eu)
bottom_ten_eu <- sales_byProduct_eu[166:175,]
bottom_ten_eu

# In the NA? 
dim(sales_byProduct_na)
bottom_ten_na <- sales_byProduct_na[166:175,]
bottom_ten_na



## 3b) Determine which plot is the best to compare game sales.
# Proportion bar chart

# Create histograms.
qplot(eu_prop, data=sales_byProduct)
qplot(na_prop, data=sales_byProduct)

# Top ten Prod. Ids
toptens <- rbind(top_ten_global, top_ten_eu, top_ten_na)
toptens

qplot(Product_Id, data=toptens)

# Bar chart of best selling products
ggplot(data=toptens, aes(x=Product_Id))+
  geom_bar(fill='Blue') +
  labs(title= 'Top Selling Products across markets', x='Product Id',
       y= 'Count') +
  theme_minimal()

# Jitter plot of proportions of eu sales of the top ten best sellers globally 
ggplot(data=top_ten_global, aes(x=Product_Id, y=eu_prop)) + 
  geom_jitter() +
  labs(title='The distribution of proportion of sales in Europe', 
       subtitle='Using the Global Top Ten Sellers',
       x= 'Product Id',
       y='Proportion of EU Sales')

# Boxplot of eu sales for global best sellers
ggplot(data=top_ten_global, aes(x=eu_prop)) + 
  geom_boxplot() +
  labs(title='The distribution of proportion of sales in Europe', 
       subtitle='Using the Global Top Ten Sellers',
       x= 'Product Id',
       y='Proportion of EU Sales')

# Boxplot of na sales for global best sellers
ggplot(data=top_ten_global, aes(x=na_prop)) + 
  geom_boxplot() +
  labs(title='The distribution of proportion of sales in Europe', 
       subtitle='Using the Global Top Ten Sellers',
       x= 'Product Id',
       y='Proportion of NA Sales')

# Jitter plot of proportions of eu sales of the top ten best sellers globally 
ggplot(data=top_ten_global, aes(x=Product_Id, y=na_prop)) + 
  geom_jitter() +
  labs(title='The distribution of proportion of sales in Europe', 
       subtitle='Using the Global Top Ten Sellers',
       x= 'Product Id',
       y='Proportion of NA Sales')  

# Top ten global sales coloured by product Id
ggplot(data=top_ten_global, aes(x=global_sales, fill=Product_Id))+
  geom_histogram() +
  labs(title='Top Ten Sales Numbers in Europe, NA and Globally',
       subtitle='By Product Id',
       x='Global Sales (in millions)', 
       y='Count of Products')

# Top ten eu sales coloured by product Id
ggplot(data=top_ten_eu, aes(x=global_sales, fill=Product_Id))+
  geom_histogram() +
  labs(title='Top Ten Sales Numbers in Europe',
       subtitle='By Product Id',
       x='Global Sales (in millions)', 
       y='Count of Products')

# Top ten na sales coloured by product Id
ggplot(data=top_ten_na, aes(x=global_sales, fill=Product_Id))+
  geom_histogram() +
  labs(title='Top Ten Sales Numbers in Europe',
       subtitle='By Product Id',
       x='Global Sales (in millions)', 
       y='Count of Products')

#Bottome ten prod
bottomtens <- rbind(bottom_ten_global, bottom_ten_eu, bottom_ten_na)
bottomtens

qplot(Product_Id, data=bottomtens)

# Create boxplots.
qplot(eu_prop, data=sales_byProduct, geom='boxplot')
qplot(na_prop, data=sales_byProduct, geom='boxplot')

# Export sales_byProduct for future use
write.csv(sales_byProduct, "Sales_byProduct.csv", row.names=FALSE)

###############################################################################

# 4. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# Using the data with outliers as per the reasoning above. 
# View data frame created in Week 4.
sales_df <- read.csv(file.choose(), header=TRUE)
View(sales_df)
dim(sales_df)
sales_df <- subset(sales_df, select = -Product)

# View the descriptive statistics.
library(tidyverse)
summary(sales_df)
sales_df2 <- mutate(sales_df, Product_Id = as_factor(Product_factor))
summary(sales_df2)
sales_df2 <- subset(sales_df2, select = -Product_factor)
head(sales_df2)

# Rename columns
colnames(sales_df2) <- c('platform','na_sales', 'eu_sales', 'global_sales', 'Product_ID')
head(sales_df2)

# Check output: Determine the min, max, and mean values.
apply(sales_df2[,c(-1,-5)],2, min)
apply(sales_df2[,c(-1,-5)],2, max)
apply(sales_df2[,c(-1,-5)],2, mean)

# Check for null values
sum(is.na(sales_df2))

###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.

# Global Sales
qqnorm(sales_df2$global_sales, col='red', xlab='Z-value',
       ylab='Global Sales')
qqline(sales_df2$global_sales, col='blue')

# North American Sales
qqnorm(sales_df2$na_sales, col='green', xlab='Z-value',
       ylab='North American Sales')
qqline(sales_df2$na_sales, col='blue')

# European sales
qqnorm(sales_df2$eu_sales, col='blue', xlab='Z-value',
       ylab='European Sales')
qqline(sales_df2$eu_sales, col='blue')

## QQ plots indicate that the data could be normally distributed. 

## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library('moments')

# Perform Shapiro-Wilk test.
shapiro.test(sales_df2$global_sales)
shapiro.test(sales_df2$na_sales)
shapiro.test(sales_df2$eu_sales)

## All p-values are less than 0.05. Reject the null hypothesis and conclude
## that the data is not normally distributed. 

## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_df2$global_sales)
kurtosis(sales_df2$global_sales)

skewness(sales_df2$na_sales)
kurtosis(sales_df2$na_sales)

skewness(sales_df2$eu_sales)
kurtosis(sales_df2$eu_sales)

## Skewness for all three columns is large. This indicates that the data
## is extremely positively skewed. Large amount of excess kurtosis due
## to the outliers in the tails of the distribution. 

## 2d) Determine correlation
# Determine correlation.
cor(sales_df2[,c(-1,-5)])

## Strongly correlated with global sales.
## EU and NA sales are correlated, but not as strongly as with
## Global Sales. This may create a problem when fitting a multiple
## linear regression.


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.

# Histogram to visualise the distributions of each col. 
ggplot(data=sales_df2, mapping=aes(global_sales))+
  geom_histogram() +
  xlab("Global Sales (in millions)") +
  ylab("Count") +
  labs(title="The distribution of global sales of Turtle Games products")

ggplot(data=sales_df2, mapping=aes(eu_sales))+
  geom_histogram() +
  xlab("European Sales (in millions)") +
  ylab("Count") +
  labs(title="The distribution of sales in Europe of Turtle Games products")

ggplot(data=sales_df2, mapping=aes(na_sales))+
  geom_histogram() +
  xlab("North American Sales (in millions)") +
  ylab("Count") +
  labs(title="The distribution of sales in North America of Turtle Games products")

# Visualise the relationship between NA, EU and Global Sales
ggplot(data=sales_df2, mapping=aes(x=global_sales, y=na_sales), color=Product_Id) +
  geom_point(alpha=0.5, size=3, color='red') +
  geom_smooth(method='lm',size=1.5) +
  xlab("Global Sales")  +
  ylab("North American Sales") +
  labs(title="Global vs North American Sales", 
       subtitle="Values are in pounds, displayed in millions")+
  theme_minimal()

## Suggests that there is a linear relationship between NA and global
## sales.

ggplot(data=sales_df2, mapping=aes(x=global_sales, y=eu_sales), color=Product_Id) +
  geom_point(alpha=0.5, size=3, color='purple') +
  geom_smooth(method='lm',size=1.5) +
  xlab("Global Sales")  +
  ylab("European Sales") +
  labs(title="Global vs European Sales", 
       subtitle="Values are in pounds, displayed in millions")+
  theme_minimal()

## Suggests that there is a linear relationship between EU and global
## sales.

ggplot(data=sales_df2, mapping=aes(x=na_sales, y=eu_sales), color=Product_Id) +
  geom_point(alpha=0.5, size=3, color='green') +
  geom_smooth(method='lm',size=1.5) +
  xlab("North American")  +
  ylab("European Sales") +
  labs(title="North American vs European Sales", 
       subtitle="Values are in pounds, displayed in millions")+
  theme_minimal()

## Slight linear pattern in the data suggests that there is not a strong
## linear correlation between NA and EU sales. 
###############################################################################

# 4. Observations and insights
# Your observations and insights here...

# The qq-plot suggests that the data is not normally distributed but the 
# Shapiro-Wilk test suggests the same. 
# Further visualisation indicated that the data is not normally distributed. 
# The values are correlated with global_sales strongly, but eu and na sales 
# are not as strongly correlated. 
# Weak linear relationship between EU and NA sales. 

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_df2)

# Determine a summary of the data frame.
summary(sales_df2)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# Global Sales regressed on EU Sales
model1 <- lm(global_sales ~eu_sales, data=sales_df2)
summary(model1)

# Global Sales regressed on NA Sales
model2 <- lm(global_sales ~ na_sales, data=sales_df2)
summary(model2)

# R-Squared and Adjusted R-Squared is slightly higher when Global Sales is
# regressed on NA than EU. Suggests a stronger relationship than NA. 

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Model1: eu sales
plot(global_sales~eu_sales, data=sales_df2)
abline(model1)

## Can see the linear relationship, but the distance from the line
## for many data points is large. The model is not a great fit for 
## the data. 

# Plot the residuals to identify if they are normal
plot(model1$residuals)

## The residuals do not have constant variance, decaying towards zero. 

qqnorm(model1$residuals)
qqline(model1$residuals)

## Residuals are not normally distributed, with heavier tails than the
## normal distribution. 

# Model2: na sales
plot(global_sales~na_sales, data=sales_df2)
abline(model2)

## Better fit for the data. 

# Plot the residuals to identify if they are normal
qplot(model2$residuals)

## Same pattern, the residuals decay towards zero, implying a non-constant
## variance. 

qqnorm(model2$residuals)
qqline(model2$residuals)

## Are not normally distributed, perhaps a polynomial curve fits the data
## better.

###############################################################################

# 3. Create a multiple linear regression model

# Multiple linear regression model.
model3 <- lm(global_sales ~eu_sales+na_sales, data=sales_df2)
summary(model3)

# Visualise the residuals
plot(model3$residuals)

## The residuals decay towards zero. 

qqnorm(model3$residuals)
qqline(model3$residuals)

# The residuals are not normally distributed. This indicates that some of 
# Global Sales cannot be fully explained by EU and NA sales. 
# Corresponds with what the metadata tells us. 
###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create a test set
na_sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
eu_sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
sales_df_test <- data.frame(na_sales, eu_sales)

# View the test set
sales_df_test

# Predict values using the test set
predict_test = predict(model3, newdata=sales_df_test, interval='confidence')

# Check to see if residuals are uncorrelated
qplot(x=predict_test[,1], y=predict_test$residuals)

# View the predicted values
predict_test
fitted_values = predict_test[,1]

# Get the actual values
sales_df2[c(sales_df2$na_sales==34.02,sales_df2$eu_sales==23.80),]
actual_1 <- sales_df2[1,4]
# Was removed as an outlier. 

sales_df2[c(sales_df2$na_sales==3.93,sales_df2$eu_sales==1.56),]
actual_2 <- sales_df2[99,4]

sales_df2[c(sales_df2$na_sales==2.73,sales_df2$eu_sales==0.65),]
actual_3 <-sales_df2[176,4]

sales_df2[c(sales_df2$na_sales==2.26,sales_df2$eu_sales==0.97),]
actual_4 <- sales_df2[211,4]

sales_df2[c(sales_df2$na_sales==22.08,sales_df2$eu_sales==0.52),]
actual_5 <- sales_df2[10,4]


# Compare the fitted values to the actual values
actual_vals <- cbind(actual_1, actual_2, actual_3, actual_4, actual_5)
actual_vals
fitted_values

## The model is good at predicting smaller values than it is at predicting
## large values, particularly the outlying data. 

# Log transform the independent variables to resolve multicollinearity
library(tidyverse)
sales_df2 <- mutate(sales_df2, na_log=log(na_sales+1))
sales_df2 <- mutate(sales_df2, eu_log=log(eu_sales+1))
head(sales_df2)

cor(sales_df2[,c(-1,-5)])

# Create a multiple linear regression
model_a <- lm(global_sales~eu_log+na_log, data=sales_df2)

summary(model_a)

# Plot the residuals
qqnorm(model_a$residuals)
qqline(model_a$residuals)

## Much more normal looking residuals, with slight deviations at the tails. 

# Check the predictive accuracy of the model 
sales_df_test_log <- mutate(sales_df_test, eu_log=log(eu_sales+1), na_log=log(na_sales+1))
sales_df_test_log

# Predict using the test values
predict_test_log = predict(model_a, newdata=sales_df_test_log[,c(3,4)], interval='confidence')

# View the predicted values
predict_test_log

# Compare the fitted to the actual values
fitted_vals_log <- predict_test_log[,1]

fitted_vals_log
actual_vals

## A log transformation on the independent variables makes the
## the non-normality of the residuals less extreme. Implies that a different
## transformation could be used. 
## The log-model was not the best fit for the data. 

## Try fit one more model, na_log with global sales, since that model had
## better R-squared than the simple eu model.

# Fit the model
model_b <- lm(global_sales~na_log, data=sales_df2)
summary(model_b)

## Adjusted R-squared has decreased in comparison to model2

# Plot the model
plot(global_sales~na_log, data=sales_df2)
abline(model_b)

## Seems to fit the data reasonably but does not fit outliers at all. 

# Plot the residuals
qqnorm(model_b$residuals)
qqline(model_b$residuals)

## These residuals look more normal. Now to check the predictive accuracy. 
predict_test_log2 <- predict(model_b, newdata=sales_df_test_log[,c(3,4)], interval='confidence')

# View the predicted values
predict_test_log2

# Compare the fitted to the actual values
fitted_vals_log2 <- predict_test_log[,1]

fitted_vals_log
actual_vals
##############################################################

# 5. Observations and insights
# Your observations and insights here...

## The multiple linear regression using the original sales data fits the
## data the best out of all of the models, considering multiple R-Squared and the test on predicted
## values. 
## The residuals are not completely normally distributed. Independent reading
## indicated that this would not be a problem in terms of the predictive accuracy
## of the model. The predictions would still be valid. The inferential
## reliability of the model must be treated with caution. 
## We can say that the coefficients estimated by the model must be treated
## with some skepticism due to the correlation in the two dependent variables. 
## NA_sales and EU_sales are not completely independent and so the coefficients
## cannot be interpreted in the same way. 
## Applying a log transformation to the independent variable normalise the residuals
## to some extent, but compromised the model's predictive ability.

## Overall, I will be considering model_3 as the best-fitting, most reliable
## model to use. 

###############################################################################
###############################################################################



