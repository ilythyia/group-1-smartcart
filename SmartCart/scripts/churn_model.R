library(caret)
library(randomForest)

data <- read.csv("data/smartcart_transactions_clean.csv")

data$churn <- as.factor(data$churn)

set.seed(123)

sample_data <- data[sample(nrow(data), 50000), ]

model <- train(
  churn ~ age +
    product_price +
    quantity +
    total_purchase_amount +
    returns,
  data = sample_data,
  method = "rf",
  ntree = 100
)

saveRDS(model, "models/churn_model.rds")
