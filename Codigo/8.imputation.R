## Data imputation
#load("../Datos/matched_sample.RData")

matched_sample_knn$distress_one <- as.factor(matched_sample_knn$distress_one)
  

sapply(matched_sample_knn, function(x) sum(is.infinite(x)/nrow(matched_sample_knn)))
sapply(matched_sample_knn, function(x) sum(is.na(x)/nrow(matched_sample_knn)))

matched_sample_knn[matched_sample_knn == -Inf] <- NA
matched_sample_knn[matched_sample_knn == Inf] <- NA

# Deal with missings

# Remove variables with more than 15% missing
matched_sample_knn <- matched_sample_knn %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)*100 >=15)

# Select the variables for the model
model_sample <- matched_sample_knn %>% select(any, distress_one, age , sector , tamest,
                                               r1_p , r3_p , r4_p, 
                                               e1_p , e2_p , e3_p , 
                                               current_ratio_p , 
                                               debt_equity_p , 
                                               debt_quality_p,
                                               equity_to_assets_p , 
                                               liquidity_p , 
                                               profit_assets_p , 
                                               debt_quality_p,
                                               empl_change , 
                                               distress2_p , distress3_p , distress4_p)

# Check missings in resulting data
plot_missing(model_sample)

# Automatic imputation of NAs with mice ----

m = 5 # number of multiple imputations
mice_mod <- mice(model_sample, m=m, method='rf')
model_sample <- complete(mice_mod, action=m)

## Check there are no missings 
plot_missing(model_sample)

# Rename the definitive variables ----

names(model_sample)<- c("year",
                        "distressed",
                        "age",
                        "sector",
                        "size",
                        "roa_p",
                        "roe_p",
                        "leverage_p",
                        "endebtness1_p",
                        "endebtness2_p",
                        "interest_charge_p",
                        "current_ratio_p",
                        "debt_equity_p",
                        "debt_quality_p",
                        "equity_to_assets_p",
                        "liquidity_p",
                        "profit_assets_p",
                        "empl_change",
                        "distress2_p",
                        "distress3_p",
                        "distress4_p")

# Assign variables proper format 

model_sample$year <- as.numeric(format(model_sample$year, format = "%Y"))

model_sample <- model_sample %>% arrange(year)

model_sample$sector <- as.factor(model_sample$sector)
model_sample$size <- as.factor(model_sample$size)
model_sample$distress2_p <- as.factor(model_sample$distress2_p)
model_sample$distress3_p <- as.factor(model_sample$distress3_p)
model_sample$distress4_p <- as.factor(model_sample$distress4_p)

# Check correlation ----

# Correlation matrix for numeric variables
df_numeric <- model_sample %>% select_if(is.numeric)

# Create correlation matrix
cor_matrix <- cor(df_numeric) 

# Plot correlation matrix 
corrplot(cor_matrix, method = "circle", col.names = names(cor_matrix))

# Remove almost perfectly correlated variable
model_sample <- model_sample %>% select(-endebtness1_p)

#Save the clean data for modeling ----
save(model_sample, file = "../Datos/imputed_data.RData")

distress_imputed <- model_sample %>% filter(distressed==1 & year ==2019)
distress_imputed <- distress_imputed %>% select(-year)

#############################################################################################################

# Extract sample for 2019
library(DataExplorer)
full_2019 <- full_sample %>% filter(any == 2019)


full_2019 <- full_2019 %>% filter(!(is.na(distress_one)))

full_2019 %>% summarise(empresas = n(),
                        emp_distr_t = sum(distress1_c, na.rm = T),
                        emp_distr_t_1 = sum(distress1_p, na.rm = T),
                        emp_dist_tyt_1 = sum(distress_two, na.rm = T),
                        emp_dist_t_no_t_1 = sum(distress_one, na.rm = T),
                        never_distress = sum(no_distress, na.rm = T),
                        P = emp_dist_tyt_1/emp_distr_t,
                        Pno = emp_dist_t_no_t_1/emp_distr_t) %>% 
  as.data.frame()


full_2019 <- full_2019 %>% select(any, distress_one, age , sector , tamest,
                                  r1_p , r3_p , r4_p, 
                                  e1_p , e2_p , e3_p , 
                                  current_ratio_p , 
                                  debt_equity_p , 
                                  debt_quality_p,
                                  equity_to_assets_p , 
                                  liquidity_p , 
                                  profit_assets_p , 
                                  debt_quality_p,
                                  empl_change , 
                                  distress2_p , distress3_p , distress4_p)

names(full_2019)<- c("year",
                     "distressed",
                     "age",
                     "sector",
                     "size",
                     "roa_p",
                     "roe_p",
                     "leverage_p",
                     "endebtness1_p",
                     "endebtness2_p",
                     "interest_charge_p",
                     "current_ratio_p",
                     "debt_equity_p",
                     "debt_quality_p",
                     "equity_to_assets_p",
                     "liquidity_p",
                     "profit_assets_p",
                     "empl_change",
                     "distress2_p",
                     "distress3_p",
                     "distress4_p")

full_2019[full_2019 == -Inf] <- NA
full_2019[full_2019 == Inf] <- NA

plot_missing(full_2019)


m = 5 # number of multiple imputations
mice_mod <- mice(full_2019, m=m, method='rf')
full_2019 <- complete(mice_mod, action=m)

# Assign variables proper format 
full_2019$year <- as.numeric(format(full_2019$year, format = "%Y"))

full_2019$sector <- as.factor(full_2019$sector)
full_2019$size <- as.factor(full_2019$size)
full_2019$distress2_p <- as.factor(full_2019$distress2_p)
full_2019$distress3_p <- as.factor(full_2019$distress3_p)
full_2019$distress4_p <- as.factor(full_2019$distress4_p)


full_2019 <- full_2019 %>% select(-year, -endebtness1_p)

save(full_2019,file = "../Datos/full_2019.RData")

# Join full sample (1 and 0)

no_distress_full_2019 <- full_2019 %>% filter(distressed == 0)

full_2019_imputed <- rbind(distress_imputed, no_distress_full_2019)

save(full_2019_imputed,file = "../Datos/full_2019_imputed.RData")


