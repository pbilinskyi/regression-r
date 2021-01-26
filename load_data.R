# 1
data <- read.csv("preprocessed_train.csv")
index_col <- data[,1]
data <- data[,-1]
rownames(data) <- index_col
#split into train and test
smp_size = floor(0.75*nrow(data))
set.seed(0)
train_ind = sample(seq_len(nrow(data)), smp_size)
train = data[train_ind,]
test = data[-train_ind,]


features_names = c("OverallQual", "SalePrice")
n <- dim(train)[1]
k <- length(features_names) - 1

model <- lm(formula = SalePrice ~ OverallQual, data = features_names)
print(summary(model))
# F-statistic:  1349 on 1 and 1453 DF,  p-value: < 2.2e-16 
# therefore, we reject null hypothesis (H_0 - all coefficients are zero)
