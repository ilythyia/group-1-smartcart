library(tidyverse)
library(cluster)

data <- read.csv("data/smartcart_transactions_clean.csv")

cluster_data <- data %>%
  select(age,
         product_price,
         quantity,
         total_purchase_amount)

cluster_data <- scale(cluster_data)

set.seed(123)

kmodel <- kmeans(cluster_data,
                 centers = 3)

saveRDS(kmodel,
        "models/kmeans_model.rds")