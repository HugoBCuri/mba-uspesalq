## Valuetion, ASSAF NETO 
## Resultado líquido do exercício + tibutos s/ Lucro (IR/CSLL) + Despesas Financeiras + depreciação = EBITDA

## EBITDA =
#  Resultado Antes do Resultado Financeiro e dos Tributos 	3.05
# +
#  Despesas Financeiras 3.06.02

buildPatrimonioLiquido <- function(years) {
  df <- data.frame()
  
  for (year in years) {
    BPA <- readCVMFile(year, "BPA") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC))
    
    BPP <- readCVMFile(year, "BPP") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC))

    data <- rbind(distinct(BPA), distinct(BPP)) %>%
      select(DENOM_CIA, CD_CVM, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_FIM_EXERC) %>% 
      distinct(DENOM_CIA, CD_CVM, DT_FIM_EXERC, DT_REFER, DS_CONTA, .keep_all = T) %>% 
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "CD_CVM", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()

    data_pl <- data %>% 
      mutate(pl = (ativo_circulante_1_01 + ativo_nao_circulante_1_02) - (passivo_circulante_2_01 + passivo_nao_circulante_2_02)) %>% 
      mutate(ativo_circulante = ativo_circulante_1_01) %>% 
      mutate(ativo_nao_circulante = ativo_nao_circulante_1_02) %>% 
      mutate(passivo_circulante = passivo_circulante_2_01) %>% 
      mutate(passivo_nao_circulante = passivo_nao_circulante_2_02) %>% 
      mutate(ativo_total = ativo_total_1) %>% 
      mutate(passivo_total = passivo_total_2 - patrimonio_liquido_consolidado_2_08) %>% 
      mutate(pl_alternativo = patrimonio_liquido_consolidado_2_08) %>% 
      mutate(pl = case_when(
        is.na(pl) ~ pl_alternativo,
        .default = pl
      )) %>% 
      select(denom_cia, cd_cvm, dt_fim_exerc, dt_refer, pl, ativo_circulante, ativo_nao_circulante, passivo_circulante, passivo_nao_circulante, ativo_total, passivo_total, pl_alternativo) %>% 
      filter(dt_fim_exerc == dt_refer)
    
    df <- rbind(df, data_pl)
  }
  
  
  return (df)
}

data_pl <- buildPatrimonioLiquido(c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))

View(data_pl %>% filter(!is.na(pl_alternativo)))

x <- data_pl %>% arrange(desc(dt_fim_exerc)) %>% filter(denom_cia == "BRADESPAR S.A.") %>% filter(dt_fim_exerc == dt_refer)
View(x)



ggplot(x, aes(x = dt_fim_exerc, y = pl)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Graph Example", x = "Categories", y = "Values")

