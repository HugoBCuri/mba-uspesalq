price <- read.csv("./data/price.csv", header = T, sep = ",")

t <- rbind(train_data, test_data) %>% arrange(dt_fim_exerc) %>% select(dt_fim_exerc) %>% distinct()

backtest <- function() {
  n <- 20
  ratio <- 1 / n

  all_data <- rbind(train_data, test_data) %>% na.omit %>% 
    mutate(dt_proxima = as.character(as.Date(dt_fim_exerc) + years(1))) 
  #all_data <- test_data %>% na.omit %>% 
  #  mutate(dt_proxima = as.character(as.Date(dt_fim_exerc) + years(1))) 
  
  all_data <- merge(all_data, all_data, by.x = c("ticker", "dt_proxima"), by.y = c("ticker", "dt_fim_exerc"), all.y = FALSE) %>% 
    distinct(dt_fim_exerc, ticker, .keep_all = TRUE)

  return_acc <- 1
  df <- data.frame(year = character(), return_acc = numeric(), return_year = numeric())
  
  for (date_t in 1:nrow(t)) {
    # print(t[date_t,])
    stocks <- all_data %>% 
      filter(dt_fim_exerc == as.character(t[date_t,])) %>% 
      arrange(desc(prediction.x)) %>% 
      head(n) %>%
      select(ticker, close_dividends.x, close_dividends.y, close.x, close.y) %>% 
      mutate(return_close_dividends = close_dividends.y / close_dividends.x)

    mean_return_year <- mean(stocks$return_close_dividends)
    if (!is.nan(mean_return_year)) {
      print(mean_return_year)
      return_acc <- return_acc * mean_return_year
      print(t[date_t,])
      df <- rbind(df, date = c(t[date_t,], return_acc, mean_return_year))
    }
  }

  return (df)
}

result_rf <- backtest()
summary(result_rf)

unique(train_data$dt_cotacao)
result_ibov <- ibov %>% filter(date == as.Date("2010-03-31") | date == as.Date("2011-03-31") | date == as.Date("2012-03-30") | date == as.Date("2013-03-28") | date == as.Date("2014-03-31") | date == as.Date("2015-03-31") | date == as.Date("2016-03-31") | date == as.Date("2017-03-31") | date == as.Date("2018-03-29") | date == as.Date("2019-03-31") | date == as.Date("2020-03-31") | date == as.Date("2021-03-31") | date == as.Date("2022-03-31")) %>% #
  mutate(return_close = close / lag(close)) %>% 
  mutate(return_close_acc = ifelse(is.na(lag(return_close)), return_close, lag(return_close) * return_close)) %>% 
  na.omit()

result_rf$ibov_return_close <- result_ibov$return_close
result_rf$ibov_return_close_acc <- result_ibov$return_close_acc
result_rf
result_rf <- result_rf %>% mutate(return_year = X.1.2028544346476..1, return_acc = X.1.2028544346476., year = X.2011.12.31.) %>% select(c("year", "return_year", "return_acc", "ibov_return_close", "ibov_return_close_acc"))
sum(result_rf$return_year) / nrow(result_rf)

result_rf <- result_rf %>% mutate(
  return_acc = as.numeric(return_acc),
  return_year = as.Date(return_year),
  ibov_return_close_acc = as.numeric(ibov_return_close_acc)
)

pivot_longer(result_rf, cols = c(3, 5), names_to = , values_to = "")

ggplot(result_rf, aes(x = year, y = return_acc, group = 1)) +
  geom_line() +
  labs(title = "Compound Growth of Investments",
       x = "Time",
       y = "Amount",
       color = "Investment") +
  theme_minimal()
