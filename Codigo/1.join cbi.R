# Joining CBI + closing firms

library(readr)
library(tidyverse)
library(xlsx)
library(cowplot)

# Closing info 2014 - 2019 ----

setwd("C:/Users/Q32223/Desktop/TFM/BAJAS")

list.files(pattern = "1[4-9]")

bajas<- NULL

for (file in list.files(pattern = "1[4-9]"))   {
  baja_any <- read_fwf(file, fwf_widths(c(10,56,6,3,4,10), c("nif", "razon", "trab", "prov", "cnae", "situacion")))
  baja_any <- baja_any %>% filter(str_detect(situacion, "B")) %>% 
    mutate(any = paste("20",str_extract(file, "\\d+"), sep = ""))
  
  bajas <- rbind(bajas, baja_any)
}

 
  bajas <- bajas %>% mutate(nif = str_remove(nif, "A|B"))
  closing_cifs <- bajas %>% pull(nif) %>% as.data.frame()
  names(closing_cifs) <- c("cif")
  

# CBI 2014 - 2019 ----

setwd("C:/Users/Q32223/Desktop/Bases de datos/CBI/CSVS")

list.files(pattern = "20[1-2][4-9]|201[4-9]|2020|2021_eng.stata.csv$")

CBI <- NULL

for (filename in list.files(pattern = "20[1-2][4-9]|201[4-9]|2020|202_eng.stata.csv$")) {
  
  # Reading file
  CBI_file <- read_delim(filename,delim = ";", 
                         escape_double = FALSE, 
                         trim_ws = TRUE, 
                         col_names = TRUE,
                         col_select = c("cif",
                                        "any", 
                                        "anyconst",
                                        "id",
                                        "peco", 
                                        "calidad",
                                        "alta_sitc",
                                        "tamest",
                                        "esta",
                                        "gsec09",
                                        "c200075_Current", #Valor produccion
                                        "c200075_Previous",
                                        "c200001_Current", #Importe neto cifra de negocios
                                        "c200001_Previous",
                                        "c200030_Current", #REB
                                        "c200030_Previous", 
                                        "c200134_Current", #ACTIVO CORRIENTE
                                        "c200134_Previous", 
                                        "c200129_Current",#Efectivo
                                        "c200129_Previous",
                                        "c200115_Current",#ACTIVO NO CORRIENTE
                                        "c200115_Previous",
                                        "c200145_Current", #PATRIMONIO NETO
                                        "c200145_Previous", 
                                        "c200158_Current", #PASIVO NO CORRIENTE
                                        "c200158_Previous", 
                                        "c200180_Current", #PASIVO CORRIENTE
                                        "c200180_Previous",
                                        "c200135_Current",# ACTIVO = PASIVO
                                        "c200135_Previous",
                                        "c209237_Current", #periodo medio pago proveedores
                                        "c209237_Previous",
                                        "c209234_Current", #periodo medio cobro clientes 
                                        "c209234_Previous",
                                        "c200378_Current", #REB/PROD
                                        "c200378_Previous",
                                        "c200377_Current", #VAB/PROD
                                        "c200377_Previous",
                                        "c290041_Current", #Gastos financieros
                                        "c290041_Previous",
                                        "c290070_Current",#RESULTADO DEL EJERCICIO
                                        "c290070_Previous",
                                        "c269212_Current", #R1
                                        "c269212_Previous",
                                        "c269206_Current", #R2
                                        "c269206_Previous",
                                        "c260213_Current", #R3
                                        "c260213_Previous", 
                                        "c269214_Current", #R4
                                        "c269214_Previous",
                                        "c200275_Current", #E1
                                        "c200275_Previous",
                                        "c209223_Current", #E2
                                        "c209223_Previous",
                                        "c200224_Current", #E3
                                        "c200224_Previous",
                                        "c200085_Current", #gastos personal
                                        "c200085_Previous",
                                        "c200082_Current", #empleo fijo
                                        "c200082_Previous", 
                                        "c200084_Current", #empleo medio
                                        "c200084_Previous")) 
  
  
  # Appending 
  
  CBI <- rbind(CBI, CBI_file)
  
}

# Changing the names to more interpretable ones

names(CBI) <- c("cif",
                "any", 
                "anyconst",
                "id",
                "peco", 
                "calidad",
                "alta_sitc",
                "tamest",
                "esta",
                "gsec09",
                "prod_c",
                "prod_p",
                "turnover_c",
                "turnover_p",
                "gop_c",
                "gop_p",
                "cur_assets_c",
                "cur_assets_p",
                "cash_c",
                "cash_p",
                "ncur_assets_c",
                "ncur_assets_p",
                "equity_c",
                "equity_p",
                "ncur_liab_c",
                "ncur_liab_p",
                "cur_liab_c",
                "cur_liab_p",
                "assets_liab_c",
                "assets_liab_p",
                "avg_sup_per_c",
                "avg_sup_per_p",
                "avg_cus_per_c",
                "avg_cus_per_p",
                "gop_output_c",
                "gop_output_p",
                "gva_output_c",
                "gva_output_p",
                "fin_exp_c",
                "fin_exp_p",
                "profit_c",
                "profit_p",
                "r1_c",
                "r1_p",
                "r2_c",
                "r2_p",
                "r3_c",
                "r3_p",
                "r4_c",
                "r4_p",
                "e1_c",
                "e1_p",
                "e2_c",
                "e2_p",
                "e3_c",
                "e3_p",
                "person_cost_c",
                "person_cost_p",
                "perm_empl_c",
                "perm_empl_p",
                "avg_empl_c",
                "avg_empl_p")


# Filtering for quality checks
CBI <- CBI %>% filter(peco == "S" & calidad == "S")
CBI <- CBI %>% mutate(age = any - anyconst)

# Remving letter from cif for better join
CBI <- CBI %>% mutate(cif = str_remove(cif, "A|B"))

# Creation of additional ratios for the present and previous periods
CBI <- CBI %>% mutate(
  #Current ratio: current assets/ current liabilities
  current_ratio_c = (cur_assets_c/cur_liab_c)*100,
  current_ratio_p = (cur_assets_p/cur_liab_p)*100, 
  
  #Debt to equity ratio: total liabilities / equity 
  debt_equity_c= ((cur_liab_c + ncur_liab_c)/equity_c)*100,
  debt_equity_p= ((cur_liab_p + ncur_liab_p)/equity_p)*100,
  
  #Debt quality ratio: current liabilities / total liabilities
  debt_quality_c= (cur_liab_c/(cur_liab_c + ncur_liab_c))*100,
  debt_quality_p= (cur_liab_p/(cur_liab_p + ncur_liab_p))*100,
  
  #Equity to assets ratio: equity/total assets
  equity_to_assets_c = (equity_c/assets_liab_c)*100,
  equity_to_assets_p = (equity_p/assets_liab_p)*100,
  
  #Liquidity ratio: cash and equivalents / current liabilities 
  liquidity_c = (cash_c/cur_liab_c)*100,
  liquidity_p = (cash_p/cur_liab_p)*100,
  
  #Financial expenses to turnover ratio: financial expenses / turnover
  finexp_turn_c = (fin_exp_c/turnover_c)*100,
  finexp_turn_p =( fin_exp_p/turnover_p)*100,
  
  #Fixed employment ratio: fixed employment / total employment 
  fixed_total_empl_c = (perm_empl_c/avg_empl_c)*100,
  fixed_total_emp_p = (perm_empl_p/avg_empl_p)*100,
  
  #Profit to assets ratio: net profit / total assets
  profit_assets_c = (profit_c/assets_liab_c)*100,
  profit_assets_p = (profit_p/assets_liab_p)*100,
  
  #Turnover to fixed assets ratio: turnover / non current assets
  rofa_c = (turnover_c/ncur_assets_c)*100,
  rofa_p = (turnover_c/ncur_assets_p)*100,
  
  #Employment change: average total employment current - previous
  empl_change = avg_empl_c - avg_empl_p)

# Aggrupation of activity sector (CNAE) to big sector classification
CBI <- CBI %>% mutate(sector = case_when(
  gsec09 %in% c("G", "I") ~ "Retail and food services",
  gsec09 %in% c("F", "L") ~ "Construction and real state",
  gsec09 %in% c("D", "E") ~ "Energy",
  gsec09 %in% c("C") ~ "Industry",
  gsec09 %in% c("G", "I") ~ "Retail and food services",
  gsec09 %in% c("J") ~ "Information and communication",
  gsec09 %in% c("H") ~ "Transport",
  TRUE ~ "Rest"
))

summary(CBI$sector)


# Merging CBI + closing data ----

length(intersect(bajas$nif, CBI$cif))

CBI$any <- as.character(CBI$any)

bajas_cbi <- left_join(CBI, bajas, by = c("cif" = "nif", "any" = "any"))
bajas_cbi %>% filter(!is.na(situacion)) %>% count()# 93101 matches

## Fixing age ----

bajas_cbi<- bajas_cbi %>% filter(age >=0)
summary(bajas_cbi$age)

## Remove non necessary vars
bajas_cbi <- bajas_cbi %>% select(-razon, -trab, -prov, -cnae, -anyconst,
                                  -peco,-calidad,-gsec09, -alta_sitc, -esta, -cif)


## Summaries for all companies -----

setwd("C:/Users/Q32223/Desktop/TFM/Codigo")

# Save the data
save(bajas_cbi, file = "../Datos/bajas_cbi.RData")


##### Generate anonymized version of the data ################

#Paso 1: nos quedamos con una muestra de 100000 observacioens

DATOS_RANDOM <- sample_n(bajas_cbi, size= 100000)

#Paso 2: HACEMOS SUFFLE 
DATOS_RANDOM <- DATOS_RANDOM %>% mutate(across(everything(),~sample(.)))





