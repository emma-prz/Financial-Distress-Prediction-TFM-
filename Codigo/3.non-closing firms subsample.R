library(tidyverse)

# 3. Non-closing firms sample
#load("../Datos/bajas_cbi.RData")
#load("../Datos/closing firms.RData")

# Non-closing sample 

# Removing possible "zombies" and selecting microfirms
micro_bajas_cbi <- bajas_cbi %>% filter(tamest == 4)

micro_bajas_cbi <- micro_bajas_cbi %>% 
  filter(turnover_c !=0 & assets_liab_c !=0 & turnover_p !=0 & assets_liab_p !=0)


# Removing those firms appearing in the closing sample
closing_ids <- bajas_cbi %>% filter(!is.na(situacion)) %>% pull(id) %>% as.data.frame()

names(closing_ids) <- c("id")
non_closing_micro <- anti_join(micro_bajas_cbi, closing_ids, by = "id")


# Select those alive between 2014 and 2019
vivas <- non_closing_micro %>%
  arrange(any) %>% 
  group_by(id) %>% summarise(veces = n(),
                             first = min(any),
                             last = max(any)) %>% 
  filter(last >= 2019 & first == 2014) %>% pull(id) %>% as.data.frame()

names(vivas) <- c("id")

non_closing_micro <- inner_join(non_closing_micro, vivas, by = "id")

#Remove covid year 
non_closing_micro <- non_closing_micro %>% filter(any != 2020)

# Discard variables with more than 15% missing values
non_closing_micro <- non_closing_micro %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)*100 >=15)

# Check resulting proportion of missings and infinites
sapply(non_closing_micro, function(x) sum(is.infinite(x)/nrow(non_closing_micro)))
sapply(non_closing_micro, function(x) sum(is.na(x)/nrow(non_closing_micro)))

# Keep variables from the previous year
non_closing_micro <- non_closing_micro %>% select(!ends_with("_c"))

unique(non_closing_micro$any)
n_distinct(non_closing_micro$id)


# Save the data
save(non_closing_micro, file = "../Datos/non-closing firms.RData")

rm(vivas)
rm(closing_ids)

