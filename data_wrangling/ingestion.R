files1 <-  c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")

readCVMFile <- function(year, report) {
  path <- paste("./data_dfp/", year, "/dfp_cia_aberta_", report, "_con_", year, ".csv", sep = "")
  print(path)
  data <- read.csv(path, header = T, sep = ";", fileEncoding = 'Windows-1252')
  return (data)
}

applyPivotWider <- function(data_to_apply) {
  if ("DT_INI_EXERC" %in% colnames(data_to_apply)) {
    result <- data_to_apply %>%
      select(DENOM_CIA, CD_CVM, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
      distinct() %>% 
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "CD_CVM", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()
    
    return (result)  
  }
  
  result <- data_to_apply %>%
    select(DENOM_CIA, CD_CVM, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_FIM_EXERC) %>% 
    distinct() %>% 
    pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "CD_CVM", "DT_FIM_EXERC", "DT_REFER")) %>%
    clean_names()
  
  return (result)
  
}
  
readItr <- function(years) {
  value <- NULL
  for (year in years) {
    ## Sem DT_INI_EXERC: BPÀ, BPP, DRE
    BPA <- readCVMFile(year, "BPA") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC))
    BPP <- readCVMFile(year, "BPP") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC))


    ## Com DT_INIC_EXERC: DFC_MD, DFC_MI, DRA, DVA
    DFC_MD <- readCVMFile(year, "DFC_MD") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    DFC_MI <- readCVMFile(year, "DFC_MI") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    #DMPL <- readCVMFile(year, "DMPL") %>% 
      #mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      #mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    DRA <- readCVMFile(year, "DRA") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    DRE <- readCVMFile(year, "DRE") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))
    
    DVA <- readCVMFile(year, "DVA") %>% 
      mutate(DT_FIM_EXERC = as.Date(DT_FIM_EXERC)) %>% 
      mutate(DT_INI_EXERC = as.Date(DT_INI_EXERC))

    BPA_BPP_DRE <- BPA %>% 
      rbind(BPP) %>% 
      mutate(DT_INI_EXERC = NA, .after = ORDEM_EXERC)
    
    MD_MI_DRA_DVA <- DFC_MD %>%  
      rbind(DFC_MI) %>% 
      rbind(DRA) %>% 
      rbind(DVA) %>% 
      rbind(DRE)
    
    without_dt_ini <- BPA_BPP_DRE %>%
      select(DENOM_CIA, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
      unique() %>%
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()

    with_dt_ini <- MD_MI_DRA_DVA  %>%
      select(DENOM_CIA, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
      unique() %>%
      pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
      clean_names()

    result <- with_dt_ini %>% 
      left_join(without_dt_ini, by = c("denom_cia", "dt_fim_exerc", "dt_refer"))
    return (result)
  }

  return (result)
}

data_after_read <- readItr(files1)
View(data_after_read)

all_columns <- colnames(data_after_read)

findName <- function(name) {
  return (all_columns[
    !is.na(str_match(all_columns, name))
  ])
}
intersect(findName('lucro'), findName('liquido'))

summary(data_after_read$lucro_liquido_6_01_01_01)
nrow(data_after_read)
data_after_read %>% 
  filter(denom_cia == "BCO SANTANDER (BRASIL) S.A.") %>% 
  select(denom_cia, dt_ini_exerc, dt_fim_exerc, lucro_liquido_do_periodo_6_01_01_01)
data_after_read$denom_cia
 
summary(data_after_read$DT_INI_EXERC)
unique(data_after_read$DT_INI_EXERC)

data_final <- data_after_read %>%
  select(DENOM_CIA, CD_CONTA, DS_CONTA, DT_REFER, VL_CONTA, DT_INI_EXERC, DT_FIM_EXERC) %>% 
  unique() %>%
  pivot_wider(names_from = c("DS_CONTA", "CD_CONTA"), values_from = VL_CONTA, id_cols = c("DENOM_CIA", "DT_INI_EXERC", "DT_FIM_EXERC", "DT_REFER")) %>%
  clean_names()

# Problema  com DT_INI_EXERC, NAs gerados no pivot_wider, não está juntando o DRE com BP
# data_final <- 

View(data_final)
ncol(data_final)

duplicates <- data_after_read %>%
  filter(VL_CONTA > 0) %>% 
  dplyr::group_by(DENOM_CIA, DT_INI_EXERC, DT_FIM_EXERC, DS_CONTA, CD_CONTA) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

data_after_read %>% 
  filter(DENOM_CIA == 'AES ELPA S.A.' & DS_CONTA == 'Contas a Receber' & CD_CONTA == '1.01.03') %>% 
  arrange(DT_FIM_EXERC)


write.xlsx(data_after_read, 'C:\Users\hugob\Desktop\MBA\0. Analises\TCC\export.xlsx', colNames = T)

data_after_read$resultado_antes_dos_tributos_sobre_o_lucro_3_05


