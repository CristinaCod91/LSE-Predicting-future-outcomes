## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################


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

# 1. Load and explore the data
turtle_sales <- read.csv(file.choose(), header=T)

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
turtle_sales

# Print the data frame.
view(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales_new <-select(turtle_sales, Product, Platform, Genre, NA_Sales, EU_Sales, Global_Sales)

# View the data frame.
head(turtle_sales_new)

# View the descriptive statistics.
summary(turtle_sales_new)

#View structure
str(turtle_sales_new)
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
qplot(NA_Sales, EU_Sales, data=turtle_sales_new)
qplot(NA_Sales, Global_Sales, data=turtle_sales_new)
qplot(Global_Sales, EU_Sales , data=turtle_sales_new)
## 2b) Histograms
qplot(NA_Sales, bins=15, data=turtle_sales_new)
qplot(EU_Sales, bins=15, data=turtle_sales_new)
qplot(Global_Sales, bins=15, data=turtle_sales_new)
## 2c) Boxplots
qplot(NA_Sales, data=turtle_sales_new, colour = I("red"), geom="boxplot")
qplot(EU_Sales, data=turtle_sales_new, colour = I("red"), geom="boxplot")
qplot(Global_Sales, data=turtle_sales_new, colour = I("red"), geom="boxplot")
###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
turtle_sales_group <- turtle_sales_new %>% group_by(Product,Genre) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# View the data frame.
head(as_tibble(turtle_sales_group))

# View the data frame.
str(turtle_sales_group)

# Explore the data frame.
summary(turtle_sales_group)

# Convert 'product' to factor (categorical variable).
mutate(turtle_sales_group,
 Product=as.factor(Product))

## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(NA_Sales_sum, EU_Sales_sum, data=turtle_sales_group)
qplot(NA_Sales_sum, Global_Sales_sum, data=turtle_sales_group)
qplot(Global_Sales_sum, EU_Sales_sum , data=turtle_sales_group)
# Create histograms.
qplot(NA_Sales_sum, bins=15, data=turtle_sales_group)
qplot(EU_Sales_sum, bins=15, data=turtle_sales_group)
qplot(Global_Sales_sum, bins=15, data=turtle_sales_group)
# Create boxplots.
qplot(NA_Sales_sum, data=turtle_sales_group, colour = I("red"), geom="boxplot")
qplot(EU_Sales_sum, data=turtle_sales_group, colour = I("red"), geom="boxplot")
qplot(Global_Sales_sum, data=turtle_sales_group, colour = I("red"), geom="boxplot")

###############################################################################

# 4. Observations and insights

#By looking at the sales data, an exploration was made using R by plotting 
#scatterplots, histograms, and boxplots. The sales were grouped per Product ID 
#to remove the noise and get a cleaner plot. 
#The scatterplot in between regions, showed the correlation for all regions 
#having biggest volume of sales up to 5 million. Global sales had a higher concentration of up to 20 mil.
#The histograms by regions showed a positively skewed data along with a few 
#good outliers detected. 
#Also, boxplots were added to see the outliers from a different view along with 
#a better understanding of summary of statistics. It can be clearly seen that 
#EU is having the smallest number of sales, followed by NA and Global performing the best.

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

# View data frame created in Week 4.
view(turtle_sales_group)

#Change all column names to lower case letters.
names(turtle_sales_group) <- tolower(names(turtle_sales_group))
turtle_sales_group

# Check output: Determine the min, max, and mean values.
min(turtle_sales_group$na_sales_sum)
max(turtle_sales_group$na_sales_sum)
mean(turtle_sales_group$na_sales_sum)

min(turtle_sales_group$eu_sales_sum)
max(turtle_sales_group$eu_sales_sum)
mean(turtle_sales_group$eu_sales_sum)

min(turtle_sales_group$global_sales_sum)
max(turtle_sales_group$global_sales_sum)
mean(turtle_sales_group$global_sales_sum)

# View the descriptive statistics.
summary(turtle_sales_group)

###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(turtle_sales_group$na_sales_sum)
qqline(turtle_sales_group$na_sales_sum)

qqnorm(turtle_sales_group$eu_sales_sum)
qqline(turtle_sales_group$eu_sales_sum)

qqnorm(turtle_sales_group$global_sales_sum)
qqline(turtle_sales_group$global_sales_sum)

## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
# Perform Shapiro-Wilk test.
library (moments)
shapiro.test(turtle_sales_group$na_sales_sum)
shapiro.test(turtle_sales_group$eu_sales_sum)
shapiro.test(turtle_sales_group$global_sales_sum)

## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(turtle_sales_group$na_sales_sum) 
kurtosis(turtle_sales_group$na_sales_sum)

skewness(turtle_sales_group$eu_sales_sum) 
kurtosis(turtle_sales_group$eu_sales_sum)

skewness(turtle_sales_group$global_sales_sum) 
kurtosis(turtle_sales_group$global_sales_sum)


## 2d) Determine correlation
# Determine correlation.
cor(turtle_sales_group$na_sales_sum, turtle_sales_group$eu_sales_sum)
cor(turtle_sales_group$na_sales_sum, turtle_sales_group$global_sales_sum)
cor(turtle_sales_group$global_sales_sum, turtle_sales_group$eu_sales_sum)

###############################################################################

# 3. Plot the data
#Create plot to find the correlation in sales by genre(EU and Na)
ggplot(data=turtle_sales_group,
       mapping=aes(x=na_sales_sum, y=eu_sales_sum, color=genre)) +
  geom_point(alpha=.5, size=3) +
  geom_smooth(method='lm', se=FALSE, size=.5) + 
  scale_x_continuous(breaks=seq(0, 40, 5)) +
  scale_y_continuous(breaks=seq(0, 40, 5)) +
  labs(title="Relationship between EU and NA sales",
       subtitle="Turtle Games research",
       #  [3] Add labels to labs function.
       caption="Source: Turtle Games website",
       x="NA Sales",
       y="EU Sales",
       color="Genre Type") +
  # Add a theme layer. 
  theme_minimal() 

#Create plot to find the correlation in sales by genre(Global and Na)
ggplot(data=turtle_sales_group,
       mapping=aes(x=na_sales_sum, y=global_sales_sum, color=genre)) +
  geom_point(alpha=.5, size=3) +
  geom_smooth(method='lm', se=FALSE, size=.5) + 
  scale_x_continuous(breaks=seq(0, 40, 5)) +
  scale_y_continuous(breaks=seq(0, 70, 10)) +
  labs(title="Relationship between Global and NA sales",
       subtitle="Turtle Games research",
       #  [3] Add labels to labs function.
       caption="Source: Turtle Games website",
       x="NA Sales",
       y="Global Sales",
       color="Genre Type") +
  # Add a theme layer. 
  theme_minimal()

#Create plot to find the correlation in sales by genre(EU and Global)
ggplot(data=turtle_sales_group,
       mapping=aes(x=global_sales_sum, y=eu_sales_sum, color=genre)) +
  geom_point(alpha=.5, size=3) +
  geom_smooth(method='lm', se=FALSE, size=.5) + 
  scale_x_continuous(breaks=seq(0, 70, 10)) +
  scale_y_continuous(breaks=seq(0, 40, 5)) +
  labs(title="Relationship between EU and Global sales",
       subtitle="Turtle Games research",
       #  [3] Add labels to labs function.
       caption="Source: Turtle Games website",
       x="Global Sales",
       y="EU Sales",
       color="Genre Type") +
  # Add a theme layer. 
  theme_minimal()

#Find the top 10 products by genre by region
#Top 10 NA
turtle_sales_natop10 <-select(turtle_sales_group, product, genre, na_sales_sum)
turtle_sales_natop10[order(turtle_sales_natop5$na_sales_sum, decreasing = TRUE),]


#Top 10 EU
turtle_sales_eutop10 <-select(turtle_sales_group, product, genre, eu_sales_sum)
turtle_sales_eutop10[order(turtle_sales_eutop5$eu_sales_sum, decreasing = TRUE),]


#Top 10 Global
turtle_sales_globaltop10 <-select(turtle_sales_group, product, genre, global_sales_sum)
turtle_sales_globaltop10[order(turtle_sales_globaltop5$global_sales_sum, decreasing = TRUE),]


###############################################################################

# 4. Observations and insights
#By analysing the data extracted from the Turtle Games website, a delve was done 
#by looking into the central tendency and variability. It was confirmed that the 
#highest values in sales are in Global, followed by NA and EU. 
#Normality was tested and it was concluded that the data is not normally distributed.
#To understand the data better and detect outliers, measures of shape were applied. 
#Skewness shows that all sales data is positively skewed and kurtosis being 
#greater than 3, meaning that data is leptokurtic or a heavy-tailed distribution with many outliers. 
#It is also interesting to point that there is a strong correlation between NA 
#and Global sales, followed by EU/Global and a smaller correlation of .62 for NA/EU. 
#These results would explain that the high relation is due to global sales 
#including both EU and NA regions. A separation of these 2 regions should be 
#made to avoid duplication and obtain accurate results in product performances.
#By looking at genres across regions, the most popular are sports, shooter, 
#role-playing and platform. There is a strong correlation by regions on sports, 
#being positioned in the lowest pricing sales point. However, the plot shows an 
#outlier on sports and action category, having the highest sales point. 
#An investigation should be made onto that and see whether there is such product 
#available online.
#Top 10 by product was looked at and sports, action and shooter products are dominating.

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

# 1. Load and explore the data
# View data frame created in Week 5.
turtle_sales_group

# Determine a summary of the data frame.
summary(turtle_sales_group)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Select numeric variables
turtle_sales_num <-select(turtle_sales_group, na_sales_sum, eu_sales_sum, global_sales_sum)

turtle_sales_num

cor(turtle_sales_num)
## 2b) Create a plot (simple linear regression)
# Plot the relationship with base R graphics.
plot(turtle_sales_num$na_sales_sum, turtle_sales_num$eu_sales_sum)
plot(turtle_sales_num$global_sales_sum, turtle_sales_num$na_sales_sum)
plot(turtle_sales_num$global_sales_sum, turtle_sales_num$eu_sales_sum)

#Test the relationship between na_sales_sum and eu_sales_sum
# Create a linear regression model and print the summary stats.
model1 <- lm(na_sales_sum~eu_sales_sum, data=turtle_sales_num)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)

# Plot the residuals.
plot(model1$residuals)

# Add a line of best fit.
# Specify the coefficients(model1).
abline(coefficients(model1))

# Calculate the sum of squares error (SSE) to determine strength.
SSE1 = sum(model1$residuals^2)

# View the result.
SSE1

# Complete a log transformation with dplyr's mutate() function.
turtle_sales_num$log_na_sales_sum = log(turtle_sales_num$na_sales_sum)
turtle_sales_num$log_eu_sales_sum = log(turtle_sales_num$eu_sales_sum)
turtle_sales_num$log_global_sales_sum = log(turtle_sales_num$global_sales_sum)

# View data frame.
head(as_tibble(turtle_sales_num))

#Replace NAN and infinite values in our dataframe with NA 
turtle_sales_num_new <- turtle_sales_num
turtle_sales_num_new[is.na(turtle_sales_num_new) | turtle_sales_num_new == "Inf"]<- NA  
turtle_sales_num_new[turtle_sales_num_new < 0] <- NA 

# Create a new model using log.
model1log <- lm(log_na_sales_sum~log_eu_sales_sum, data=turtle_sales_num_new)

# View full regression table.
summary(model1log)

# Plot the relationship between NA and EU
plot(turtle_sales_num_new$log_eu_sales_sum, turtle_sales_num_new$log_na_sales_sum)

# Add a line-of-best fit to existing plot.
abline(coefficients(model1log))

# Calculate the sum of squares error (SSE) to determine strength.
SSE1log = sum(model1log$residuals^2)

# View the result.
SSE1log

# SSE = model1log is a better fit than model1.
# The closer the SSE is to 0, the better the fit.

#Test the relationship between na_sales_sum and global_sales_sum
# Create a linear regression model and print the summary stats.
model2 <- lm(global_sales_sum~na_sales_sum, data=turtle_sales_num)

# View the model.
model2

# View more outputs for the model - the full regression table.
summary(model2)

# Plot the residuals.
plot(model2$residuals)

# Add a line of best fit.
# Specify the coefficients(model2).
abline(model2)

# Calculate the sum of squares error (SSE) to determine strength.
SSE2 = sum(model2$residuals^2)

# View the result.
SSE2

# Create a new model using log.
model2log <- lm(log_global_sales_sum~log_na_sales_sum, data=turtle_sales_num_new)

# View full regression table.
summary(model2log)

# Plot the relationship between NA and Global
plot(turtle_sales_num_new$log_na_sales_sum, turtle_sales_num_new$log_global_sales_sum)

# Add a line-of-best fit to existing plot.
abline(coefficients(model2log))

# Calculate the sum of squares error (SSE) to determine strength.
SSE2log = sum(model2log$residuals^2)

# View the result.
SSE2log

# SSE = model2log is better than model2 and shows a stronger relationship than model1.
# The closer the SSE is to 0, the better the fit.

#Test the relationship between global_sales_sum and eu_sales_sum.
# Create a linear regression model and print the summary stats.
model3 <- lm(global_sales_sum~eu_sales_sum, data=turtle_sales_num)

# View the model.
model3

# View more outputs for the model - the full regression table.
summary(model3)

# Plot the residuals.
plot(model3$residuals)

# Add a line of best fit.
# Specify the coefficients(model3).
abline(coefficients(model3))

# Calculate the sum of squares error (SSE) to determine strength.
SSE3 = sum(model3$residuals^2)

# View the result.
SSE3

# Create a new model using log.
model3log <- lm(log_global_sales_sum~log_eu_sales_sum, data=turtle_sales_num_new)

# View full regression table.
summary(model3log)

# Plot the relationship between EU and Global
plot(turtle_sales_num_new$log_eu_sales_sum, turtle_sales_num_new$log_global_sales_sum)

# Add a line-of-best fit to existing plot.
abline(coefficients(model3log))

# Calculate the sum of squares error (SSE) to determine strength.
SSE3log = sum(model3log$residuals^2)

# View the result.
SSE3log

# SSE = model3log is a much better fit than model3 and shows the strongest relationship than model 1 and 2.
# The closer the SSE is to 0, the better the fit.

#Summary statistics

# Model 2 and 3 have higher adj. R squared values and show higher correlation.
# The p-value for model 2 and 3 are very small showing a high significance of the variable.
# Based on the SSE, models 2 and 3 have the best fit as SSE is closer to 0.
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
modelm = lm(global_sales_sum~.,
            data=turtle_sales_num)


# View the model.
modelm

# View more outputs for the model.
summary(modelm)

#################################################################

# 4. Predictions based on given values
#Predict global sales based on provided values. Compare your prediction to the observed value(s).
# Create a new data frame for the forecast values.
product <-107
na_sales_sum <- c(34.02)
eu_sales_sum <- c(23.80)
global_sales_forecast<- data.frame(product, na_sales_sum, eu_sales_sum)
predict(modelm, newdata=global_sales_forecast)
##66.35 predicted vs. 67.85 observed


# Create a new data frame for the forecast values.
product <-3267
na_sales_sum <- c(3.93)
eu_sales_sum <- c(1.56)
global_sales_forecast<- data.frame(product, na_sales_sum, eu_sales_sum)
predict(modelm, newdata=global_sales_forecast)
##7.55 predicted vs. 6.04 observed


# Create a new data frame for the forecast values.
product <-6815
na_sales_sum <- c(2.73)
eu_sales_sum <- c(0.65)
global_sales_forecast<- data.frame(product, na_sales_sum, eu_sales_sum)
predict(modelm, newdata=global_sales_forecast)
##4.24 predicted vs. 4.32 observed

# Create a new data frame for the forecast values.
product <-2877
na_sales_sum <- c(2.26)
eu_sales_sum <- c(0.97)
global_sales_forecast<- data.frame(product, na_sales_sum, eu_sales_sum)
predict(modelm, newdata=global_sales_forecast)
##5.19 predicted vs. 3.53 observed.

# Create a new data frame for the forecast values.
product <-326
na_sales_sum <- c(22.08)
eu_sales_sum <- c(0.52)
global_sales_forecast<- data.frame(product, na_sales_sum, eu_sales_sum)
predict(modelm, newdata=global_sales_forecast)
## 26.54 predicted vs. 23.21 observed.

###############################################################################

# 5. Observations and insights
#Three separate linear regression models where build to find the best fit 
#between EU/NA, Global/EU, Global/NA. The values of the variables have been 
#mutated to return the natural logarithm and reduce the sum of square errors 
#and strengthening the fit. In turn, there was a strong relation found in 
#Global/EU and Global/NA, showing very low p value and hence, proving its significance.
#An evaluation has been made on creating a multiple linear regression, which, 
#showed a much stronger correlation of 97% and low p values, proving significance 
#of global sales with NA and EU region. To test its accuracy, predictions of 
#global sales were made, given NA and EU values. The output showed a good 
#prediction when comparing the observed values.
#However, confidence cannot be stated based on the models provided, 
#due to Global variable including NA and EU sales values that distorts the 
#results. Therefore, a high accuracy in prediction and correlations is 
#interpreted wrong in that scenario.



###############################################################################
###############################################################################




