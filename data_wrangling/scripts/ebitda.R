## Valuetion, ASSAF NETO 
## Resultado líquido do exercício + tibutos s/ Lucro (IR/CSLL) + Despesas Financeiras + depreciação = EBITDA

## EBITDA =
#  Resultado Antes do Resultado Financeiro e dos Tributos 	3.05
# +
#  Despesas Financeiras 3.06.02


buildEbitda <- function(years) {
  df <- data.frame()
  
  for (year in years) {
    DRE <- readCVMFile(year, "DRE") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    data <- DRE %>%
      select(DENOM_CIA, CD_CVM, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
      unique() %>% 
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "CD_CVM", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()

    data_ebtida <- data %>% 
      mutate(lucro_liquido = case_when(
        !is.na(resultado_liquido_das_operacoes_continuadas_3_09) ~ resultado_liquido_das_operacoes_continuadas_3_09, 
        !is.na(resultado_liquido_das_operacoes_continuadas_3_07) ~ resultado_liquido_das_operacoes_continuadas_3_07,
        !is.na(resultado_liquido_das_operacoes_continuadas_3_11) ~ resultado_liquido_das_operacoes_continuadas_3_11,
        !is.na(lucro_prejuizo_consolidado_do_periodo_3_09) ~ lucro_prejuizo_consolidado_do_periodo_3_09,
        .default = NA
      )) %>% 
      mutate(imposto_de_renda_e_contribuicao_social_sobre_o_lucro = case_when(
        !is.na(imposto_de_renda_e_contribuicao_social_sobre_o_lucro_3_08) ~ imposto_de_renda_e_contribuicao_social_sobre_o_lucro_3_08, 
        !is.na(imposto_de_renda_e_contribuicao_social_sobre_o_lucro_3_08) ~ imposto_de_renda_e_contribuicao_social_sobre_o_lucro_3_06,
        !is.na(imposto_de_renda_e_contribuicao_social_sobre_o_lucro_3_08) ~ imposto_de_renda_e_contribuicao_social_sobre_o_lucro_3_10,
        .default = NA
      )) %>% 
      mutate(resultado_financeiro = case_when(
        !is.na(resultado_financeiro_3_06) ~ resultado_financeiro_3_06, 
        !is.na(resultado_financeiro_3_08) ~ resultado_financeiro_3_08,
        .default = NA
      )) %>% 
      mutate(despesas_financeiras = case_when(
        !is.na(despesas_financeiras_3_06_02) ~ despesas_financeiras_3_06_02,
        .default = NA
      )) %>% 
      mutate(receitas_financeiras = case_when(
        !is.na(receitas_financeiras_3_06_01) ~ receitas_financeiras_3_06_01,
        .default = NA
      )) %>% 
      mutate(ebit = lucro_liquido + imposto_de_renda_e_contribuicao_social_sobre_o_lucro - despesas_financeiras) %>% 
      select(denom_cia, cd_cvm, dt_ini_exerc, dt_fim_exerc, dt_refer, ebit, receitas_financeiras, resultado_financeiro)

    df <- rbind(df, data_ebtida)
  }

  return (df)
}

data_ebitda <- buildEbitda(c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))
data_ebitda <- buildEbitda(c("2018"))

summary(data_ebitda)
View(data_ebitda)

buildDepreciacao <- function(years) {
  df <- data.frame()
  
  for (year in years) {
    DVA <- readCVMFile(year, "DVA") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))

    data <- DVA %>%
      select(DENOM_CIA, CD_CVM, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
      unique() %>% 
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "CD_CVM", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()

    data_depreciacao <- data %>% 
      mutate(depreciacao = ifelse(is.na(depreciacao_amortizacao_e_exaustao_7_05_01), depreciacao_amortizacao_e_exaustao_7_04_01, depreciacao_amortizacao_e_exaustao_7_05_01)) %>%
      select(denom_cia, cd_cvm, dt_ini_exerc, dt_fim_exerc, dt_refer, depreciacao)
    
    df <- rbind(df, data_depreciacao)
  }

  return (df)
}


data_depreciacao <- buildDepreciacao(c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))
data_ebitda

summary(data_ebitda)
data_ebitda <- merge(data_ebitda, data_depreciacao, by = c("cd_cvm", "dt_ini_exerc", "dt_fim_exerc"), all = FALSE)
data_ebitda <- data_ebitda %>% mutate(ebitda = ebit - receitas_financeiras - depreciacao)


ggplot(data_ebitda %>% filter(denom_cia.y == "REDE D'OR SÃO LUIZ S.A."), aes(x = dt_fim_exerc, y = ebitda)) + 
  geom_bar(stat = "identity", color = "black")


ggplot(
  data_depreciacao %>% 
    arrange(desc(dt_fim_exerc)) %>% 
    filter(denom_cia == "MOVIDA PARTICIPACOES SA"), 
  aes(x = dt_fim_exerc, y = new_depreciacao)
) 
+ labs(title = "Depreciação das empresas", x = "Data fim", y = "Depreciação acumulada")

