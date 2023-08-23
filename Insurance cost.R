library(tidyverse)
library(plotly)
library(GGally)
library(ggplot2)
library(skimr)
library(caret)
df<- read_csv("C:/Users/HP/Downloads/insurance.csv")
View(df)
summary(df)
skim(df)
structure(df)
# Let's plot the distribution of charges in relation to the gender

plot1 <- plot_ly(df, y = ~charges, color = ~sex, type = 'violin')
plot1 <- plot1 %>%
  layout(
    title = "Distribution of Charges by Gender",
    xaxis = list(title = "Gender"),
    yaxis = list(title = "Charges")
  )
print(plot1)
# Let's plot the charges as per the age
plot2<-plot_ly(df,x=~age,y=df$charges,type='bar')
plot2 <- plot2 %>%
  layout(
    title = "Distribution of Charges by Age",
    xaxis = list(title = "Age"),
    yaxis = list(title = "Charges")
  )
print(plot2)
# Charges for the smoking category
plot3<-plot_ly(df,labels=~smoker,values=df$charges,type='pie')
plot3 <- plot3 %>%
  layout(
    title = "Distribution of Charges by Smoker",
    xaxis = list(title = "Smoker"),
    yaxis = list(title = "Charges")
  )
print(plot3)

# Charges based on number of children
plot4<-plot_ly(df,x=~children,y=df$charges,type='bar')
plot4 <- plot4 %>%
  layout(
    title = "Distribution of Charges by number of Children",
    xaxis = list(title = "Number of children"),
    yaxis = list(title = "Charges")
  )
print(plot4)
# Distribution of charges in respect to BMI
plot5<-plot_ly(df,y=~bmi,x=~charges,type='histogram')
print(plot5)
#Distribution of charges
plot6 <- plot_ly(data = df, x = ~charges, type = 'histogram', marker = list(color = "rgba(58, 71, 8)"), 
                 opacity = 0.9, hoverinfo = 'x') %>%
  layout(
    title = "Distribution of Charges",
    xaxis = list(title = "Charges"),
    yaxis = list(title = "Frequency"),
    bargap = 0.1,  # Adjust the gap between bars
    showlegend = FALSE,  # Set to TRUE if you want to add a legend
    plot_bgcolor = "rgba(240, 240, 240, 0.95)",  # Set background color
    paper_bgcolor = "white",  # Set plot area background color
    xaxis = list(gridcolor = "lightgrey"),  # Customize grid lines
    yaxis = list(gridcolor = "lightgrey")
  )
print(plot6)
# Now we encode our categorical data
df$sex=factor(df$sex,
levels=c('male','female'),
labels=c(0,1))

df$smoker=factor(df$smoker,
              levels=c('yes','no'),
              labels=c(1,0))

df$region=factor(df$region,
              levels=c('southwest','northwest','northeast','southeast'),
              labels=c(0,1,2,3))
View(df)


# Split the data into X and y(target column)
y <- df$charges
X <- df %>% select(-charges)

# Now we perform Cross validation
K <-9 # This is the value of K (number of folds)

# Next, we create the K-fold cross-validation object
folds <- createFolds(y, k = K, list = TRUE, returnTrain = TRUE)

# Create a function to perform linear regression and return the model's performance
perform_linear_regression <- function(train_indices, test_indices) {
  # Split the data into training and testing sets
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[test_indices, ]
  y_test <- y[test_indices]
  
  # We now fit the linear model
  lm_model <- lm(y_train ~ ., data = X_train)
  
  # Predict the test set
  y_pred <- predict(lm_model, newdata = X_test)
  
  # We then calculate the performance metrics (RMSE)
  rmse <- sqrt(mean((y_test - y_pred)^2))
  return(list(predicted_values = y_pred, test_values = y_test, rmse = rmse))
}

# Perform cross-validation and get the performance metrics
cv_results <- lapply(folds, perform_linear_regression)

# Calculate the average performance metric across all folds
average_rmse <- mean(sapply(cv_results, function(x) x$rmse))

# Print the average RMSE
cat("Average RMSE:", average_rmse, "\n")


#Combine the predicted values and test values from all folds into a single dataframe
predicted_df <- data.frame()
for (i in seq_along(cv_results)) {
  predicted_values <- cv_results[[i]]$predicted_values
  test_values <- cv_results[[i]]$test_values
  
  fold_df <- data.frame(Predicted = predicted_values, Test = test_values)
  predicted_df <- rbind(predicted_df, fold_df)
}

# View the dataframe

View(predicted_df)
print(average_rmse)

plot7<- predicted_df %>%
  plot_ly(data = ., y = ~Test, type = "box", name = "Test") %>%
  add_trace(y = ~Predicted, name = "Predicted") %>%
  layout(yaxis = list(title = "Values"))
# Show the plot
plot7





