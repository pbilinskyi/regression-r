# library(tidyverse)

train = read.csv("preprocessed_train.csv")
train <- select(train, OverallQual, GrLivArea, YearBuilt, TotalBsmtSF, SalePrice)
train

# plotting scatterplots
ggplot(data = train) + 
  geom_point(mapping = aes(x = OverallQual, y = SalePrice), color = "blue")

ggplot(data = train) + 
  geom_point(mapping = aes(x = OverallQual, y = YearBuilt), color = "blue")

ggplot(data = train) + 
  geom_point(mapping = aes(x = OverallQual, y = TotalBsmtSF), color = "blue")

ggplot(data = train) + 
  geom_point(mapping = aes(x = OverallQual, y = GrLivArea), color = "blue")

# histograms
ggplot(data = train) + 
  geom_histogram(mapping = aes(x = SalePrice), bins = 1000, binwidth = 1000)

plt2 = ggplot(data = train) + 
  geom_histogram(mapping = aes(x = OverallQual), bins = 10000)

plt1
