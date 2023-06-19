# Full sample generation 

#load("../Datos/bajas_cbi.RData")

# Filter years
full_sample <- bajas_cbi %>% filter(any %in% c("2014", "2015", "2016", "2017", "2018", "2019"))

# Remove zombies 
full_sample <- full_sample %>% 
  filter(!(turnover_p == 0 & assets_liab_p ==0))

full_sample <- full_sample %>% 
  filter(!(turnover_p == 0 & assets_liab_p ==0 & turnover_c == 0 & assets_liab_c == 0))

# Calculate the distress ratios for the whole sample 
  #for current period
full_sample <- full_sample %>% mutate(neg_prof_c = ifelse(profit_assets_c <= 0, 1, 0),
                                      low_liquidity_c = ifelse(liquidity_c <= 5, 1, 0),
                                      low_equity_c = ifelse(equity_to_assets_c <= 3, 1, 0))

  #for previous period 
full_sample <- full_sample %>% mutate(neg_prof_p = ifelse(profit_assets_p <= 0, 1, 0),
                                      low_liquidity_p = ifelse(liquidity_p <= 5, 1, 0),
                                      low_equity_p = ifelse(equity_to_assets_p <= 3, 1, 0))


# Calculate the 4 distress measures
full_sample <- full_sample %>% mutate(distress1_c = ifelse(low_equity_c == 1 & low_liquidity_c == 1 & neg_prof_c == 1, 1, 0),
                                      distress2_c = ifelse(low_liquidity_c == 1 & neg_prof_c == 1, 1, 0),
                                      distress3_c = ifelse(low_equity_c == 1 & neg_prof_c == 1, 1, 0),
                                      distress4_c = ifelse(low_equity_c == 1 & low_liquidity_c == 1, 1, 0))

full_sample <- full_sample %>% mutate(distress1_p = ifelse(low_equity_p == 1 & low_liquidity_p == 1 & neg_prof_p == 1, 1, 0),
                                      distress2_p = ifelse(low_liquidity_p == 1 & neg_prof_p == 1, 1, 0),
                                      distress3_p = ifelse(low_equity_p == 1 & neg_prof_p == 1, 1, 0),
                                      distress4_p = ifelse(low_equity_p == 1 & low_liquidity_p == 1, 1, 0))


save(full_sample, file = "../Datos/full sample.RData")

