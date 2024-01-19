path <- "./data/price.csv"
print(path)
b3_data <- read.csv(path, header = T, sep = ",")
summary(b3_data)

b3_data <- b3_data %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  mutate(ticker = as.factor(sub("\\.SA$", "", ticker)))

cnpj_ticker_data <- read.csv("./data/cnpj_ticker.csv", header = T, sep = ";", colClasses = c(cnpj = "character"))

b3_cnpj_data <- merge(b3_data, cnpj_ticker_data, by = "ticker", all.x = TRUE) %>% 
  mutate(cnpj_cia = cnpj) %>% 
  select(-c("cnpj"))


View(b3_cnpj_data)
head(b3_cnpj_data)

VALE3 <- b3_cnpj_data %>% filter(ticker == "ITSA3") %>% arrange(Date)

head(VALE3)
ggplot() +
  geom_line(data = VALE3, aes(x = Date, y = Close, color = "SemDiv"), linetype = "solid") +
  geom_line(data = VALE3, aes(x = Date, y = CloseDividends, color = "ComDiv"), linetype = "dashed") +
  labs(title = "Stock Prices Over Time",
       x = "Date",
       y = "Closing Price",
       color = "Stock Ticker") +
  scale_color_manual(values = c("SemDiv" = "blue", "ComDiv" = "red"))  # Customize colors if needed

head(data_roi)
unique(data_roi$dt_fim_exerc)


data_roi <- data_roi %>% mutate(
  dt_cotacao = case_when(
    dt_fim_exerc == "2011-12-31" ~ as.Date("2012-03-30"),
    dt_fim_exerc == "2012-12-31" ~ as.Date("2013-03-28"),
    dt_fim_exerc == "2013-12-31" ~ as.Date("2014-03-31"),
    dt_fim_exerc == "2014-12-31" ~ as.Date("2015-03-31"),
    dt_fim_exerc == "2015-12-31" ~ as.Date("2016-03-31"),
    dt_fim_exerc == "2016-12-31" ~ as.Date("2017-03-31"),
    dt_fim_exerc == "2017-12-31" ~ as.Date("2018-03-29"),
    dt_fim_exerc == "2018-12-31" ~ as.Date("2019-03-29"),
    dt_fim_exerc == "2019-12-31" ~ as.Date("2020-03-31"),
    dt_fim_exerc == "2020-12-31" ~ as.Date("2021-03-31"),
    dt_fim_exerc == "2021-12-31" ~ as.Date("2022-03-31"),
    dt_fim_exerc == "2022-12-31" ~ as.Date("2023-03-31"),
    .default = NA
  )
) %>% mutate(
  dt_cotacao_tres_meses = case_when(
    dt_fim_exerc == "2011-12-31" ~ as.Date("2012-07-02"),
    dt_fim_exerc == "2012-12-31" ~ as.Date("2013-07-01"),
    dt_fim_exerc == "2013-12-31" ~ as.Date("2014-07-02"),
    dt_fim_exerc == "2014-12-31" ~ as.Date("2015-07-01"),
    dt_fim_exerc == "2015-12-31" ~ as.Date("2016-07-01"),
    dt_fim_exerc == "2016-12-31" ~ as.Date("2017-07-03"),
    dt_fim_exerc == "2017-12-31" ~ as.Date("2018-07-02"),
    dt_fim_exerc == "2018-12-31" ~ as.Date("2019-07-01"),
    dt_fim_exerc == "2019-12-31" ~ as.Date("2020-07-01"),
    dt_fim_exerc == "2020-12-31" ~ as.Date("2021-07-01"),
    dt_fim_exerc == "2021-12-31" ~ as.Date("2022-07-01"),
    dt_fim_exerc == "2022-12-31" ~ as.Date("2023-07-03"),
    .default = NA
  )
)

merged_data <- merge(data_roi, b3_cnpj_data, by.x = c("cnpj_cia", "dt_cotacao"), by.y = c("cnpj_cia", "Date"))
merged_data <- merge(merged_data, data_ebitda, by.x = c("denom_cia", "dt_fim_exerc"), by.y = c("denom_cia.x", "dt_fim_exerc"))
merged_data <- merge(merged_data, b3_cnpj_data, by.x = c("cnpj_cia", "dt_cotacao_tres_meses"), by.y = c("cnpj_cia", "Date"))

head(merged_data)
merged_data <- merged_data %>% distinct(
  denom_cia,
  dt_ini_exerc.x, 
  dt_fim_exerc, 
  cnpj_cia, 
  dt_cotacao, 
  cd_cvm.x, 
  lucro_liquido, 
  pl, 
  ativo_circulante, 
  ativo_nao_circulante, 
  ativo_total, 
  passivo_circulante, 
  passivo_nao_circulante, 
  passivo_total, 
  ROE, 
  ROI, 
  POPL, 
  POAT, 
  ROA, 
  GAF, 
  pl_alternativo, 
  ticker.x,
  Close.x,
  CloseDividends.x,
  dividendsAcc.x,
  Close.y,
  CloseDividends.y,
  dividendsAcc.y,
  ebit, 
  ebitda, 
  receitas_financeiras, 
  resultado_financeiro, 
  depreciacao,
) %>% 
  mutate(
    dt_ini_exerc = dt_ini_exerc.x, 
    cd_cvm = cd_cvm.x,
    ticker = ticker.x,
    Close = Close.x,
    CloseDividends = CloseDividends.x,
    dividendsAcc = dividendsAcc.x,
    Close_tres_meses = Close.y,
    CloseDividends_tres_meses = CloseDividends.y,
    dividendsAcc_tres_meses = dividendsAcc.y,
  ) %>% 
  clean_names() %>% 
  select(-c("dt_ini_exerc_x", "cd_cvm_x", "ticker_x", "close_x", "close_dividends_x", "dividends_acc_x", "close_y", "close_dividends_y", "dividends_acc_y")) 

export_data <- merged_data %>% distinct(cd_cvm, dt_fim_exerc, ticker, .keep_all = T)

write.csv(export_data, file = "./export/training.csv", row.names = TRUE, na = "NA")


