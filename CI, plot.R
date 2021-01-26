# 2
alpha = fitted.values(model)
sigma <- sigma(model)
# CI for alpha_i (i =1, 2, ..., k)
gamma <- 0.05
t = abs(qt(gamma/2, n - k - 1))
X_mean = colMeans(X)

CI_dev_alpha = seq(0, 0, len=k)
for (i in 1:k){
  CI_dev_alpha[i] = t * sigma * sqrt(1/ sum((X[, i] - X_mean[i])^2))
}

# Assuming errors are normal, compute CI for mu_h based on each observation
i = 1 # we train univariate linear regression, i - index of feature
X_SS = sum((X[, i] - X_mean[i])^2)
sigma_y_hat = seq(0, 0, n)

for (h in 1:n){
    sigma_y_hat[h] = sigma*sqrt(1/n + (X[h, i] - X_mean[i])^2 / X_SS)
}

# for prediction intervals
sigma_y_hat_limits = seq(0, 0, n)
for (h in 1:n){
  sigma_y_hat_limits[h] = sigma*sqrt(1 + 1/n + (X[h, i] - X_mean[i])^2 / X_SS)
}

# plot of the regression line with CI and PI
temp_var <- predict(model, interval="prediction")
new_df <- cbind(X, temp_var, y_lower)
ggplot(new_df, aes(OverallQual, SalePrice))+
  geom_point() +
  geom_line(aes(y=lwr), linetype="dashed", color="black")+
  geom_line(aes(y=upr), linetype="dashed", color="black")+
  geom_smooth(method=lm, se=TRUE) + scale_x_discrete(name ="OverallQual", limits=xtics)