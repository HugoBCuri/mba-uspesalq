input_data <- read.csv("./data/training.csv", header = T, sep = ",")

set.seed(123)

input_data <- input_data %>% select(-c("pl_alternativo", "ativo_circulante", "ativo_nao_circulante", "passivo_circulante", "passivo_nao_circulante", "passivo_total", "receitas_financeiras", "resultado_financeiro"))

input_data <- input_data %>% mutate(output = close_dividends_tres_meses / close_dividends)

head(input_data)

sample_size <- as.integer(nrow(input_data) * 0.8)
train_indices <- sample(1:nrow(input_data), sample_size)

#train_data <- input_data[train_indices,]
#test_data <- input_data[-train_indices,]
train_data <- input_data %>% filter(as.Date(dt_fim_exerc) <= as.Date("2019-12-31"))
test_data <- input_data %>% filter(as.Date(dt_fim_exerc) > as.Date("2019-12-31"))

unique(test_data$dt_fim_exerc)
nrow(test_data %>%  na.omit)
nrow(test_data)

ctrl <- trainControl(
  method = "cv",  # Cross-validation method
  number = 6,     # Number of folds
  verboseIter = TRUE
)

rf_model <- randomForest(
  output ~ close +
    close_dividends + 
    lucro_liquido + 
    roe + 
    roi + 
    popl + 
    poat + 
    roa + 
    gaf + 
    ebit + 
    ebitda,
  ntree = 500,
  data = train_data, 
  na.action = na.omit,
  replace = TRUE,
  trControl = ctrl,
)
rf_model$importance
mean(rf_model$rsq)
sqrt(mean(rf_model$mse))

sum((train_data$output - train_data$prediction)^2, na.rm = T) / sum((train_data$output - mean(train_data$output))^2, na.rm = T)


rf_model$rsq
hist(rf_model$mse)

train_data$prediction <- predict(rf_model, newdata = train_data)
train_data <- train_data %>% mutate(residual = prediction - output)
hist(train_data$residual, breaks = 100)

shapiro.test(train_data$residual)

test_data$prediction <- predict(rf_model, newdata = test_data)

test_data <- test_data %>% mutate(residual = prediction - output)
summary(test_data$residual)
hist(test_data$residual, breaks = 100)
shapiro.test(test_data$residual)

View(test_data %>% arrange(prediction))
View(test_data)


## MSE test
mse <- test_data$residual ^ 2

summary(rf_model)
nrow(mse)
r2 <- sum((test_data$output - test_data$prediction)^2, na.rm = T) / sum((test_data$output - mean(test_data$output))^2, na.rm = T)
r2

r2 <- sum((train_data$output - train_data$prediction)^2, na.rm = T) / sum((train_data$output - mean(train_data$output))^2, na.rm = T)
r2

sum((input_data$output - predict(rf_model, newdata = input_data))^2, na.rm = T) / sum((input_data$output - mean(input_data$output))^2, na.rm = T)
input_data

rmse <- sqrt(mse)

hist(rmse, breaks = 100)

1 - (mean(mse, na.rm = T) / var(mse, na.rm = T))




## R quadrado
r2 


