# Adequacy of model

# R^2
r_squared = summary(model)$r.squared
adj_r_squared = summary(model)$adj.r.squared

# loss function (MSE of residuals)
MSE = mean(residuals(model)^2)

# test if sigma is good estimation for variance of y's population
sigma_test = sd(test$SalePrice)
if (sigma > sigma_test){
  F <- (sigma/sigma_test)^2
  df1 <- n - k - 1
  df2 <- nrow(test) - 1
} else {
  F <- (sigma_test/sigma)^2
  df2 <- n - k - 1
  df1 <- nrow(test) - 1
}

alpha = 0.95
Fq <- df(alpha, df1, df2)
# accept hypothesis of adequacy when F < Fq
# we have F = 2.053887, Fq = 4.100493. Therefore, we adequately estimate 
# a variance of y's population.
  

# residual plots
res = residuals(model)
data = cbind(res / sigma, fitted.values(model), X[, 1])

# 1
# https://plotly.com/ggplot2/geom_density/
set.seed(0)
x = rnorm(length(res))
x = x[order(x)]
y = dnorm(x)
data1 = cbind(data, x, y)
colnames(data1) <- c("res", "x_norm", "y_dnorm", "y_fitted", "OverallQual")
ggplot(data1)  + geom_histogram(aes(x = res, y = ..density..), bins = 40, alpha = 0.5) 
              + geom_line(aes(x = x_norm, y = y_dnorm)) 
              + labs(title = "Histogram of standartied Res vs normal df")

# 2
ggplot(data, aes(x = y_fitted)) + geom_point(aes(y = res)) 
+ labs(title = "Fitted Y vs Res")

#3
ggplot(data, aes(x = OverallQual)) + geom_point(aes(y = res))
+ labs(title = "X_1 (OverallQual) vs Res")

# 4 
# we have time of !!!
data = data.frame(residuals(model), X["YearBuilt"])
colnames(data) <- c("res", "t")
ggplot(data) + geom_point(aes(x = t, y = res)) 
