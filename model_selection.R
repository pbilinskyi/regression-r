# 1. stepwise regression
# model based on correlations

# splitting
smp_size = floor(0.5*nrow(data))
set.seed(0)
train_ind = sample(seq_len(nrow(data)), smp_size)
data_MS = data[train_ind,]

# my model: select the modt correlated features
cor_SP <- cor(data, data$SalePrice)
cor_SP <- cor_SP[order(-cor_SP), ]
corr_level = 0.3
correlated_features <-  names(cor_SP[abs(cor_SP) > corr_level])
model_corr <- lm(SalePrice ~ ., data = data_MS[, correlated_features])

# data contains all features
full <- lm(SalePrice ~ ., data = data_MS)
summary(full)
library(MASS)
#AIC. both directions
step_both <- stepAIC(full, trace = FALSE)
print(step$anova)
print(summary(step))

#AIC. forward direction
initial <- lm(SalePrice ~ 1, data = data_MS)
step_forward <- stepAIC(initial, direction="forward", 
                        scope = list(lower=initial, upper=full))
print(step_forward$anova)
print(summary(step_forward))


# simple stepwise regression
library(leaps)
step_backward_simple <- regsubsets(SalePrice ~ ., data = data_MS, method = "backward")
summary(step_backward_simple)

# get names of selected predictors:
features_forward <- sort(names(coef(step_forward)))
features_both <- sort(names(coef(step_both)))
features_simple_backward <-  sort(names(coef(step_backward_simple, 8)))
features_corr <- sort(names(coef(model_corr)))
# one of the models doesn't include Near_Artery_or_Feedr
# now we hame 5 models:
# step_forward, step_both, step_backward_simple, model_corr, full

# comparison of models:
# adjusted R-squared
print("Adj. R-squared")
print("Model 1. Predictors with correlation > 0.3: ")
summary(model_corr)$adj.r.squared
print("Model 2. Backward AIC: ")
summary(step_backward)$adj.r.squared
print("Model 3. Forward AIC: ")
summary(step_forward)$adj.r.squared
# RMSE
RMSE <- function(model, data){
      y_true <- data$SalePrice
      y_pred <- predict(model, subset(data, select = -c(SalePrice)))
      return (sqrt(mean( (y_pred - y_true)^2 )))
}

print("RMSE")
print("Model 1. Predictors with correlation > 0.3: ")
RMSE(model_corr, data_MS)
print("Model 2. Backward AIC: ")
RMSE(step_backward, data_MS)
print("Model 3. Forward AIC: ")
RMSE(step_forward, data_MS)

# Conclusion: model step_forward has the best R^2 and RMSE score.
summary(step_forward)
y <- data$SalePrice
data <- data[features_forward[-1]]
model_forward <- lm(y ~ ., data, )
