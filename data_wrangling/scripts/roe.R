## Valuetion, ASSAF NETO 
## Resultado líquido do exercício + tibutos s/ Lucro (IR/CSLL) + Despesas Financeiras + depreciação = EBITDA

buildLucroLiquido <- function(years) {
  df <- data.frame()
  
  for (year in years) {
    DRE <- readCVMFile(year, "DRE") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    data <- DRE %>%
      select(CNPJ_CIA, DENOM_CIA, CD_CVM, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
      unique() %>% 
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("CNPJ_CIA", "DENOM_CIA", "CD_CVM", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()
    
    data_roe <- data %>% 
      # filter(isAnnualized(dt_ini_exerc, dt_fim_exerc)) %>% 
      # mutate(lucro_liquido = resultado_liquido_das_operacoes_continuadas_3_09) %>% 
      mutate(lucro_liquido = case_when(
        !is.na(resultado_liquido_das_operacoes_continuadas_3_09) ~ resultado_liquido_das_operacoes_continuadas_3_09, 
        !is.na(resultado_liquido_das_operacoes_continuadas_3_07) ~ resultado_liquido_das_operacoes_continuadas_3_07,
        !is.na(resultado_liquido_das_operacoes_continuadas_3_11) ~ resultado_liquido_das_operacoes_continuadas_3_11,
        !is.na(lucro_prejuizo_consolidado_do_periodo_3_09) ~ lucro_prejuizo_consolidado_do_periodo_3_09,
        .default = NA
      )) %>% 
      # mutate(lucro_operacional = resultado_antes_do_resultado_financeiro_e_dos_tributos_3_05) %>% 
      mutate(lucro_operacional = case_when(
        !is.na(resultado_antes_dos_tributos_sobre_o_lucro_3_05) ~ resultado_antes_dos_tributos_sobre_o_lucro_3_05,
        !is.na(resultado_antes_do_resultado_financeiro_e_dos_tributos_3_05) ~ resultado_antes_do_resultado_financeiro_e_dos_tributos_3_05,
        !is.na(resultado_antes_do_resultado_financeiro_e_dos_tributos_3_07) ~ resultado_antes_do_resultado_financeiro_e_dos_tributos_3_07,
        .default = NA
      )) %>% 
      mutate(cnpj_cia = gsub("[^0-9]", "", cnpj_cia)) %>% 
      select(denom_cia, cnpj_cia, cd_cvm, dt_ini_exerc, dt_fim_exerc, dt_refer, lucro_liquido, lucro_operacional)
    
    df <- rbind(df, data_roe)
  }
  
  
  return (df)
}


buildROI <- function(years) {
  patrimonio_liquido <- buildPatrimonioLiquido(years)
  lucro_liquido <- buildLucroLiquido(years)
  
  df <- inner_join(lucro_liquido, patrimonio_liquido, by = c("cd_cvm", "dt_fim_exerc")) %>% 
    mutate(pl_final = case_when(
      !is.na(pl) ~ pl,
      !is.na(pl_alternativo) ~ pl_alternativo,
      .default = NA
    )) %>% 
    mutate(ativo_total_final = case_when(
      !is.na(ativo_circulante) & !is.na(ativo_nao_circulante) ~ ativo_circulante + ativo_nao_circulante,
      !is.na(ativo_total) ~ ativo_total,
      .default = NA
    )) %>% 
    mutate(passivo_total_final = case_when(
      !is.na(passivo_circulante) & !is.na(passivo_nao_circulante) ~ passivo_circulante + passivo_nao_circulante,
      !is.na(passivo_total) ~ passivo_total,
      .default = NA
    )) %>% 
    mutate(denom_cia = denom_cia.x, dt_refer = dt_refer.x) %>% 
    mutate(ROE = lucro_liquido / pl_final) %>% 
    mutate(ROI = lucro_liquido / (passivo_total_final + pl_final)) %>% 
    mutate(POPL = passivo_total_final / pl_final) %>% 
    mutate(POAT = passivo_total_final / ativo_total_final) %>% 
    mutate(ROA = lucro_operacional / ativo_total_final) %>% 
    mutate(GAF = ROE / ROI) %>% 
    select(denom_cia, cnpj_cia, cd_cvm, dt_ini_exerc, dt_fim_exerc, dt_refer, lucro_liquido, pl, ativo_circulante, ativo_nao_circulante, passivo_circulante, passivo_nao_circulante, passivo_total, ativo_total, ROE, ROI, POPL, POAT, ROA, GAF, pl_alternativo)
  
  return (df)
}

data_roi <- buildROI(c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))
data_roi <- buildROI(c("2011"))
View(data_roi)

summary(data_roi$ROA)
nrow(data_roi) # 1621

data_roi_na_ll <- data_roi %>%  filter(is.na(lucro_liquido))
View(data_roi_na_ll)





ggplot(x, aes(x = dt_fim_exerc, y = pl)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Graph Example", x = "Categories", y = "Values")

