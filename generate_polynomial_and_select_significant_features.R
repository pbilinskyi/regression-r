# generate polynomial features

# data - without SalePrice
y <- data$SalePrice
data <- subset(data, select = -c(SalePrice))

# features, wrom which we want to generate polymomials:
# do not add categorical features to this list!!!
features_names <- c("OverallQual", "YearBuilt", "GarageRating", "TotalBsmtSF",
                    "GrLivArea", "LotFrontage", "LotArea", "BsmtQuartersQ")
for (i in 1:length(features_names)){
  feature_name <- features_names[i]
  poly_feature_name <- paste(feature_name, "^2")
  data[poly_feature_name] <- data[feature_name]^2
}

model <- lm(y ~ ., data = data)

# compute adjucted p-values
# from page 28 of http://www.machinelearning.ru/wiki/images/5/53/Psad_linreg_2017.pdf 
# beta  <- coef(model)
# Vbeta <- vcov(model)
# D     <- diag(1 / sqrt(diag(Vbeta)))
# t     <- D %*% beta
# Cor   <- D %*% Vbeta %*% t(D)
# library(mvtnorm)
# model.df  <- nrow(X) - length(beta)
# p_adj <- sapply(abs(t), function(x) 1-pmvt(-rep(x, length(beta)),
#                                             rep(x, length(beta)),
#                                            corr = Cor, df = model.df))
# print("p_adj for selecting coefficients:")
# print(p_adj)
# 
library(multcomp)
model.mc <- glht(m, linfct = diag(length(coef(m))))
summary(model.mc)

final_features_list <- c("")
data <- data[final_features_list]
final_model <- lm()