# Install required packages
#install.packages("regclass")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("broom")
#install.packages("car")

library(regclass)
library(ggplot2)
library(corrplot)
library(broom)
library(car)

# Load the raw data and check the structure of the data
df <- read.csv("housing.csv")
str(df)

# Check for missing values
colSums(is.na(df))

# Drop rows with missing values in 'total_bedrooms'
cleaned_df <- df[!is.na(df$total_bedrooms), ]

# Create new variables based on the cleaned data
cleaned_df$bedroom_room_ratio <- cleaned_df$total_bedrooms / cleaned_df$total_rooms
cleaned_df$rooms_per_household <- cleaned_df$total_rooms / cleaned_df$households
cleaned_df$income_rooms_ratio <- cleaned_df$median_income / cleaned_df$rooms_per_household

# Assign cleaned data to the final dataset
housing <- cleaned_df[, c("longitude", "latitude", "housing_median_age", "total_rooms",
                          "total_bedrooms", "population", "households", "median_income",　
                          "bedroom_room_ratio", "rooms_per_household", "income_rooms_ratio",
                          "ocean_proximity", "median_house_value")]
head(housing)

summary(housing)

# Calculate all correlations with 'median_house_value'
all_correlations(housing, interest="median_house_value", sorted="magnitude")

# Correlation matrix visualization excluding 'ocean_proximity' using `corrplot`
# Reference: https://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
cor_matrix <- cor(housing[, !names(housing) %in% "ocean_proximity"])

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  diag = FALSE,
  col = colorRampPalette(c("darkblue", "white", "darkred"))(200), 
  addCoef.col = "black", 
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45
)

# Filter correlations (|r| >= 0.7) to check multicollinearity
cor <- all_correlations(housing, sorted = "magnitude")
cor[abs(cor$correlation) >= 0.7, ]

# Create a scatter plot of median income vs. median house value, with points colored by median income
# Reference1: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# Reference2: https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(housing, aes(x = median_income, y = median_house_value, color = median_income)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "green", linetype = "solid", linewidth = 1) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Median Income and Median House Value",
    x = "Median Income (Tens of Thousands USD)", y = "Median House Value (USD)",
    color = "Median Income") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    aspect.ratio = 1
  )

# Analyze association between 'median_income' and 'median_house_value'
associate(median_house_value ~ median_income, data = housing, permutations = 500, seed = 298)

# Log-transform and analyze association between 'median_income' and 'median_house_value'
housing$log_median_house_value <- log(housing$median_house_value)
housing$log_median_income <- log(housing$median_income)
associate(log_median_house_value ~ log_median_income, data = housing, permutations = 500, seed = 298)

# Create a scatter plot of income_rooms_ratio vs. median house value
ggplot(housing, aes(x = income_rooms_ratio, y = median_house_value, color = income_rooms_ratio)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Income-Rooms Ratio and Median House Value",
    x = "Income-Rooms Ratio (Median Income / Rooms per Household)", 
    y = "Median House Value (USD)",
    color = "Income-Rooms Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    aspect.ratio = 1
  )

# Analyze association between 'income_rooms_ratio' and 'median_house_value'
associate(median_house_value ~ income_rooms_ratio, data = housing, permutations = 500, seed = 298)

# Log-transform and analyze association between 'income_rooms_ratio' and 'median_house_value'
housing$log_income_rooms_ratio <- log(housing$income_rooms_ratio)
associate(log_median_house_value ~ log_income_rooms_ratio, data = housing, permutations = 500, seed = 298)

# Create a scatter plot of bedroom_room_ratio vs. median house value
ggplot(housing, aes(x = bedroom_room_ratio, y = median_house_value, color = bedroom_room_ratio)) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Bedroom-to-Room Ratio and Median House Value",
    x = "Bedroom-to-Room Ratio (Bedrooms / Total Rooms)",
    y = "Median House Value (USD)",
    color = "B/R Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    aspect.ratio = 1
  )

# Create a histogram of median house values, grouped by the bedroom-to-room ratio categories
ggplot(housing, aes(
  x = median_house_value,
  fill = cut(
    bedroom_room_ratio,
    breaks = c(0, 0.075, 0.15, 0.225, Inf),
    labels = c("Very Low", "Low", "Medium", "High")
  )
)) +
  geom_histogram(bins = 30, alpha = 0.4, position = "identity") +
  scale_fill_manual(
    values = c("Very Low" = "gray", "Low" = "dark blue", "Medium" = "yellow", "High" = "orange")
  ) +
  scale_x_continuous(limits = c(0, 500000)) +
  labs(
    title = "Median House Value by Bedroom to Room Ratio",
    x = "Median House Value (USD)",
    y = "Count",
    fill = "Bedroom Room Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Analyze association between 'bedroom_room_ratio' and 'median_house_value'
associate(median_house_value ~ bedroom_room_ratio, data = housing, permutations = 500, seed = 298)

# Log-transform and analyze association between 'bedroom_room_ratio' and 'median_house_value'
housing$log_bedroom_room_ratio <- log(housing$bedroom_room_ratio)
associate(log_median_house_value ~ log_bedroom_room_ratio, data = housing, permutations = 500, seed = 298)

# Create a scatter plot of rooms_per_household vs. median house value
ggplot(housing, aes(x = rooms_per_household, y = median_house_value, color = rooms_per_household)) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Rooms per Household and Median House Value",
    x = "Rooms per Household (Total Rooms / Households)",
    y = "Median House Value (USD)",
    color = "Rooms/Households"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    aspect.ratio = 1
  )

# Analyze association between 'rooms_per_household' and 'median_house_value'
associate(median_house_value ~ rooms_per_household, data = housing, permutations = 500, seed = 298)

# Log-transform and analyze association between 'rooms_per_household' and 'median_house_value'
housing$log_rooms_per_household <- log(housing$rooms_per_household)
associate(log_median_house_value ~ log_rooms_per_household, data = housing, permutations = 500, seed = 298)

# Create a scatter plot showing the geographic distribution of median house values
ggplot(housing, aes(x = longitude, y = latitude, color = median_house_value)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Geographic Distribution of Median House Value",
    x = "Longitude",
    y = "Latitude",
    color = "Median House Value"
  ) +
  theme_minimal()

# Create a scatter plot of latitude vs. median house value
ggplot(housing, aes(x = latitude, y = median_house_value, color = latitude)) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Latitude and Median House Value",
    x = "Latitude (Degrees, How Far North)",
    y = "Median House Value (USD)",
    color = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    aspect.ratio = 1
  )

# Analyze association between 'latitude' and 'median_house_value'
associate(median_house_value ~ latitude, data = housing, permutations = 500, seed = 298)

# Log-transform and analyze association between 'latitude' and 'median_house_value'
housing$log_latitude <- log(housing$latitude)
associate(log_median_house_value ~ log_latitude, data = housing, permutations = 500, seed = 298)

# Create a boxplot to visualize the distribution of median house values for different ocean proximity categories
# Reference1: https://www.geeksforgeeks.org/how-to-plot-means-inside-boxplot-using-ggplot2-in-r/
# Reference2: https://r-graph-gallery.com/38-rcolorbrewers-palettes
ggplot(housing, aes(x = ocean_proximity, y = median_house_value, fill = ocean_proximity)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Median House Value by Ocean Proximity",
    x = "Ocean Proximity",
    y = "Median House Value (USD)",
    fill = "Ocean Proximity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    aspect.ratio = 1/3      
  )

# Analyze association between 'ocean_proximity' and 'median_house_value'
associate(median_house_value ~ ocean_proximity, data = housing, permutations = 500, seed = 298)

# Log-transform and analyze association between 'ocean_proximity' and 'median_house_value'
associate(log_median_house_value ~ ocean_proximity, data = housing, permutations = 500, seed = 298)

# Linear Regression. Let's first test baseline model "model that includes all dependent variables"

names(housing)
model1<-lm(median_house_value~longitude+latitude+housing_median_age+total_rooms+total_bedrooms+
            population+households+ median_income+bedroom_room_ratio+ rooms_per_household+income_rooms_ratio
          +ocean_proximity, data=housing)
confint(model1,level=0.95)
summary(model1)

# Let's enhance the model:
# We will be checking for multicollinearity to enhance our model "New technique"
vif(model1)

#Based on our output: longitude, latitude, total_rooms , total_bedrooms, households  have 
# an GVIF value>10, therefore indicating severe multicollinearity. Furthermore, income_rooms_rate
# and median_income, showing high multicollinearity. Let's see what's highly correlated 
#to each other. We will checking for multicollinearity using the following:

# Filter correlations (|r| >= 0.7) to check multicollinearity
cor <- all_correlations(housing, sorted = "magnitude")
cor[abs(cor$correlation) >= 0.7, ]

# Calculate all correlations with 'median_house_value'
all_correlations(housing, interest="median_house_value", sorted="magnitude")

#Based on the Filtered correlations: median_income & income_rooms_ratio are highly correlated,
#so we are just going to choose one. Since median_income explains slightly 
#more variance (correlation: 0.688) than income_rooms_ratio (0.665) will prioritize median_income 
#and drop income_rooms_ratio.

# Fitting a linear regression (with keeping median_income and removing income_rooms_ratio ).


model2<-lm(median_house_value~longitude+latitude+housing_median_age+total_rooms+total_bedrooms+
            population+households+ median_income+bedroom_room_ratio+ rooms_per_household
          +ocean_proximity, data=housing)
confint(model2,level=0.95)
summary(model2)
vif(model2)

#note: already improving. Median_income is no longer correlated to other variables in model
#(GVIF<5)

#Based on the Filtered correlations: longitude & latitude are highly correlated,
#so we are just going to choose one. Since latitude explains slightly 
#more variance (correlation: -0.145) than longitude (-0.045) will prioritize latitude
#and drop longitude.
model3<-lm(median_house_value~latitude+housing_median_age+total_rooms+total_bedrooms+
             population+households+ median_income+bedroom_room_ratio+ rooms_per_household
           +ocean_proximity, data=housing)
confint(model3,level=0.95)
summary(model3)
vif(model3)

#note: improved F-statistic, lower Adjusted r-square. Latitude is no longer correlated to 
#other variables in model (GVIF<5)

# Based on Filtered correlation and GVIF total_rooms , total_bedrooms and households have 
#severe association among themselves. To choose which ones to keep, we will try variations
#in the next section and base it on F-score(if it increases) and adjusted r-square

#Model 4: Remove household first (high gvif)
model4<-lm(median_house_value~latitude+housing_median_age+total_rooms+total_bedrooms+
             population+ median_income+bedroom_room_ratio+ rooms_per_household
           +ocean_proximity, data=housing)
confint(model4,level=0.95)
summary(model4) 
vif(model4)

#note: total_bedroom gvif decreased significantly. So we will remove it for sure. Improved
#F-statistic but a bit lower adjusted r-square value. 

#Model 5: Remove total_bedrooms and keep total_rooms
model5<-lm(median_house_value~latitude+housing_median_age+total_rooms+
             population+ median_income+bedroom_room_ratio+ rooms_per_household
           +ocean_proximity, data=housing)
confint(model5,level=0.95)
summary(model5)
vif(model5)

#note: total_rooms gvif decreased significantly still slightly above 5 so let's try removing it
#and keeping total_bedroom.F-statistic has increased as well. 

#model 6: Remove total_rooms and keep total_bedrooms
model6<-lm(median_house_value~latitude+housing_median_age+total_bedrooms+
             population+ median_income+bedroom_room_ratio+ rooms_per_household
           +ocean_proximity, data=housing)
confint(model6,level=0.95)
summary(model6)
vif(model6)

#note: total_bedrooms gvif is under 5. So removing total_rooms and keeping gvif is better.

# Final note: model6 had the best F-statistics "highest value"among all models with a p-value of
# p-value: < 2.2e-16 meaning the model is overall meaningful and statistically significant. 
#Even though Adjusted r-square is 0.638, which is slighly lower than other models, model6
#takes into account multicollinearity that avoids potential instability in the coefficients
#which is essential. Therfore, we believe model 6 is the best fit for predicting median_house_value.

# Summarizing model6 using broom
model6_summary <- tidy(model6)
model6_summary

# Create a horizontal bar plot to visualize the coefficients of Model 6 including confidence intervals using error bars for each predictor
# Reference1: https://r-charts.com/ranking/bar-plot-ggplot2/
# Reference2: https://www.statology.org/ggplot-geom_errorbar/
ggplot(model6_summary, aes(x = reorder(term, estimate), y = estimate)) +
    geom_col(fill = "skyblue") +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
    coord_flip() +
    labs(title = "Model 6 Coefficients with Confidence Intervals",
         x = "Predictor",
         y = "Estimate")
