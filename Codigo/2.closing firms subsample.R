# 2. Closing firms subsample 

load("../Datos/bajas_cbi.RData")
bajas_cbi <- bajas_cbi %>% 
  mutate(age_strata = cut(age,c(0,5,10,20,50,100, Inf), include.lowest = T))

#Select closing firms
cerradas <- bajas_cbi %>%  filter(!(is.na(situacion)))

## Filtering reactivations -----
reactivaciones <- cerradas %>%
  arrange(any) %>% 
  group_by(id) %>% summarise(veces = n(),
                             first = min(any),
                             last = max(any),
                             baja = any[!is.na(situacion)]) %>% 
  mutate(reaparece = ifelse(last>baja, "si", "no"))

table(reactivaciones$reaparece) # around 1300 firms reappear

### Looking for those not reappearing -----
cierres <- reactivaciones %>% 
  filter(reaparece =="no")

no_reap <- cierres %>% pull(id)

# Subsample of companies which close and do not reactivate 
# these companies appear across several years,that is why there are much more companies in 2014 than in 2019
cierres <- cerradas %>% filter(id %in% no_reap)
n_distinct(cierres$id)

# Subsample of micro firms which close and do not reappear ----
micro_sample_closing <- cierres %>% filter(tamest == 4)

## Keep only the year they closed
micro_sample_closing_year <- micro_sample_closing %>% 
  group_by(id) %>% 
  arrange(desc(any)) %>% 
  dplyr::slice(1)


# Check proportion of missings and infinites 
sapply(micro_sample_closing_year, function(x) sum(is.infinite(x)/nrow(micro_sample_closing_year)))
sapply(micro_sample_closing_year, function(x) sum(is.na(x)/nrow(micro_sample_closing_year)))

# Remove variables with more than 15% NA
micro_sample_closing_year <- micro_sample_closing_year %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)*100 >=15)

# Remove variables from the year they close (keep previous)
micro_sample_closing_year <- micro_sample_closing_year %>% select(!ends_with("_c"))

# Remove those which close from different reasons like merges
micro_sample_closing_year <-  micro_sample_closing_year %>% 
  filter(!(situacion %in% c("B4","B3","B2","B1")))

n_distinct(micro_sample_closing_year$id)#Final number of closed firms 

#Save the data 
save(micro_sample_closing_year, file = "../Datos/closing firms.RData")

micro_sample_closing_year %>% filter(is.na(situacion))


micro_sample_closing_year %>% summary()

rm(cerradas)
rm(cierres)
rm(reactivaciones)
rm(micro_sample_closing)
