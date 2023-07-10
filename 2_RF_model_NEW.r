#RANDOM FOREST MODEL#

#combine all predictors into a single dataframe
predictors_df <- do.call(rbind, lapply(gldas1_stacked, as.data.frame, xy = TRUE))
predictors_df <- predictors_df[, -c(1, 2)] # Exclude x and y columns

#combine all response variables into a single dataframe
response_df <- do.call(rbind, lapply(grace_clipped_list, function(x) as.data.frame(x, xy = TRUE)))
response_df <- response_df[, -c(1, 2)] # Exclude x and y columns

#combine predictors and response into a single dataframe
model_data <- cbind(predictors_df, response_df)

#split the data into training and testing sets
set.seed(234)
n <- nrow(model_data)
train_rows <- sample(1:n, size = floor(0.80 * n), replace = FALSE)
test_rows <- setdiff(1:n, train_rows)

train_data <- model_data[train_rows, ]
test_data <- model_data[test_rows, ]

#TRAIN MODEL
model <- randomForest(response_df ~ ., data = train_data, ntree = 1500, mtry = 5, nodesize = 4)


print(model)
prediction <- predict(model, newdata = test_data)
