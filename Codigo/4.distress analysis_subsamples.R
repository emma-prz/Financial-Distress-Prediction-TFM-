# 4. Analysis


#load("../Datos/non-closing firms.RData")
#load("../Datos/closing firms.RData")


# Check the thresholds ----

closing_ratios <- micro_sample_closing_year %>% ungroup() %>% select(liquidity_p, equity_to_assets_p, profit_assets_p) 

survivals_ratios <- non_closing_micro %>% ungroup() %>% select(liquidity_p, equity_to_assets_p, profit_assets_p) 


# CLOSED FIRMS
# set the ratios and thresholds to check
ratios <- c("liquidity_p", "equity_to_assets_p", "profit_assets_p")
thresholds <- as.numeric(seq(-100, 150, by = 1))

# create a matrix to store the proportions below thresholds
prop_below_threshold_closed <- matrix(NA, nrow = length(thresholds), ncol = length(ratios),
                                      dimnames = list(as.character(thresholds), ratios))

# loop through each ratio and threshold, and calculate the proportion below the threshold
for (i in seq_along(ratios)) {
  for (j in seq_along(thresholds)) {
    prop_below_threshold_closed[as.character(thresholds[j]), i] <- mean(as.numeric(closing_ratios[[ratios[i]]]) <= thresholds[j], na.rm = TRUE)
  }
}

# convert the matrix to a data frame
prop_below_threshold_closed <- as.data.frame(prop_below_threshold_closed) %>% rownames_to_column()
prop_below_threshold_closed$threshold <- as.numeric(prop_below_threshold_closed$rowname)

# print the result
prop_below_threshold_closed


# SURVIVALS 
# create a matrix to store the proportions below thresholds
prop_below_threshold_alive <- matrix(NA, nrow = length(thresholds), ncol = length(ratios),
                                     dimnames = list(as.character(thresholds), ratios))

# loop through each ratio and threshold, and calculate the proportion below the threshold
for (i in seq_along(ratios)) {
  for (j in seq_along(thresholds)) {
    prop_below_threshold_alive[as.character(thresholds[j]), i] <- mean(as.numeric(non_closing_micro[[ratios[i]]]) <= thresholds[j], na.rm = TRUE)
  }
}

# convert the matrix to a data frame
prop_below_threshold_alive <- as.data.frame(prop_below_threshold_alive) %>% rownames_to_column()
prop_below_threshold_alive$threshold <- as.numeric(prop_below_threshold_alive$rowname)

# print the result
prop_below_threshold_alive

liquidity <- cbind(prop_below_threshold_alive$threshold, prop_below_threshold_alive$liquidity_p, prop_below_threshold_closed$liquidity_p) %>% as.data.frame()
colnames(liquidity) <- c("threshold", "proportion_alive", "proportion_close")
liquidity <- liquidity %>% mutate(diff = proportion_close - proportion_alive) %>% 
  arrange(desc(diff))


equity <- cbind(prop_below_threshold_alive$threshold,prop_below_threshold_alive$equity_to_assets_p, prop_below_threshold_closed$equity_to_assets_p) %>% as.data.frame()
colnames(equity) <- c("threshold", "proportion_alive", "proportion_close")
equity <- equity %>% mutate(diff = proportion_close - proportion_alive) %>% 
  arrange(desc(diff))


profit <- cbind(prop_below_threshold_alive$threshold,prop_below_threshold_alive$profit_assets_p, prop_below_threshold_closed$profit_assets_p) %>% as.data.frame()
colnames(profit) <- c("threshold", "proportion_alive", "proportion_close")
profit <- profit %>% mutate(diff = proportion_close - proportion_alive) %>% 
  arrange(desc(diff))

# Graphs to see the differences -----

p1 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = liquidity_p*100,  color = "Alive"), size = 1.5) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = liquidity_p*100, color = "Closed"), size = 1.5) +
  #geom_vline(xintercept = 5, alpha = 0.4, linetype = "dashed", color = "blue")+
  scale_color_manual(name = "Status", values = c("Alive" = "#87E066", "Closed" = "#F57217")) +
  labs(x = "Threshold", y = "Proportion below threshold (%)", title = "Liquidity ratio")+
  theme_light() +
  theme(legend.text = element_text(size = 14),
        text = element_text(size = 13, family = "serif"),
        legend.position = "bottom",   
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.direction = "horizontal",
        plot.title.position = "panel")


p2 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = equity_to_assets_p*100, color = "Alive"), size = 1.5) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = equity_to_assets_p*100, color = "Closed"), size = 1.5) +
  #geom_vline(xintercept = 3, alpha = 0.4,color = "blue", linetype = "dashed")+
  scale_color_manual(name = "Status", values =  c("Alive" = "#87E066", "Closed" = "#F57217")) +
  labs(x = "Threshold", y = "Proportion below threshold (%)", title = "Equity to assets ratio")+
  theme(axis.ticks.x = ) +
  theme_light() +
  theme(legend.text = element_text(size = 14),
        text = element_text(size = 13, family = "serif"),
        legend.position = "bottom",   
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.direction = "horizontal",
        plot.title.position = "panel")


p3 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = profit_assets_p*100, color = "Alive"), size = 1.5) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = profit_assets_p*100, color = "Closed"), size = 1.5) +
  scale_color_manual(name = "Status", values = c("Alive" = "#87E066", "Closed" = "#F57217")) +
  #geom_vline(xintercept = 0, alpha = 0.2, linetype = "dashed", color = "blue")+
  labs(x = "Threshold", y = "Proportion below threshold (%)", title = "Profit to assets ratio") +
  theme_light() +
  theme(legend.text = element_text(size = 14),
        text = element_text(size = 13, family = "serif"),
        legend.position = "bottom",   
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.direction = "horizontal",
        plot.title.position = "panel")



# Closing firms ----
###  Check proportions distress ratios

micro_sample_closing_year <- micro_sample_closing_year %>% mutate(neg_prof = ifelse(profit_assets_p <= 0, 1, 0),
                                                                  neg_e2= ifelse(e2_p <= 0, 1, 0), 
                                                                  low_liquidity = ifelse(liquidity_p <= 5, 1, 0),
                                                                  low_equity = ifelse(equity_to_assets_p <= 3, 1, 0),
                                                                  low_roe = ifelse(r3_p <= 1, 1, 0),
                                                                  low_roa = ifelse(r1_p <= 1, 1, 0),
                                                                  neg_apalanc = ifelse(r4_p < 0, 1, 0),
                                                                  low_current = ifelse(current_ratio_p < 1, 1, 0),
                                                                  high_debt = ifelse(debt_equity_p >= 2, 1, 0),
                                                                  empl_reduction = ifelse(empl_change < 0, 1, 0))



summary_table <- micro_sample_closing_year %>% summary()

summary_df <- as.data.frame.matrix(summary_table)

#write.xlsx(as.data.frame(summary_df),
           #"C:/Users/Q32223/Desktop/TFM/resumen var contables cierres.xlsx",
           #row.names = FALSE)


### Distress measure

micro_sample_closing_year <- micro_sample_closing_year %>% 
  mutate(distress1 = ifelse(low_equity == 1 & low_liquidity == 1 & neg_prof == 1, 1, 0),
         distress2 = ifelse(low_liquidity == 1 & neg_prof == 1, 1, 0),
         distress3 = ifelse(low_equity == 1 & neg_prof == 1, 1, 0),
         distress4 = ifelse(low_equity == 1 & low_liquidity == 1, 1, 0),
         distress5 = ifelse(low_equity == 1 & neg_e2 == 1 & neg_prof == 1, 1, 0))


micro_sample_closing_year %>%  select(starts_with("distress")) %>% summary()



# Non-closing firms ----

### Check proportions distress ratios

non_closing_micro <- non_closing_micro %>%  mutate(neg_prof = ifelse(profit_assets_p <= 0, 1, 0),
                                                   neg_e2= ifelse(e2_p <= 0, 1, 0),
                                                   low_liquidity = ifelse(liquidity_p <= 5, 1, 0),
                                                   low_equity = ifelse(equity_to_assets_p <= 3, 1, 0),
                                                   low_roe = ifelse(r3_p <= 1, 1, 0),
                                                   low_roa = ifelse(r1_p <= 1, 1, 0),
                                                   neg_apalanc = ifelse(r4_p < 0, 1, 0),
                                                   low_current = ifelse(current_ratio_p < 1, 1, 0),
                                                   high_debt = ifelse(debt_equity_p >= 2, 1, 0),
                                                   empl_reduction = ifelse(empl_change < 0, 1, 0))



non_closing_micro %>% summary()

### Distress measure

non_closing_micro <- non_closing_micro %>% 
  mutate(distress1 = ifelse(low_equity == 1 & low_liquidity == 1 & neg_prof == 1, 1, 0),
         distress2 = ifelse(low_liquidity == 1 & neg_prof == 1, 1, 0),
         distress3 = ifelse(low_equity == 1 & neg_prof == 1, 1, 0),
         distress4 = ifelse(low_equity == 1 & low_liquidity == 1, 1, 0),
         distress5 = ifelse(low_equity == 1 & neg_e2 == 1 & neg_prof == 1, 1, 0))


non_closing_micro %>%  select(starts_with("distress")) %>% summary()


# Compare distress proportions in both samples ---- 
selected_vars <- c( "neg_prof", "low_liquidity", "low_equity","neg_e2","distress1","distress2","distress3","distress4","distress5")


closing <- map_df(micro_sample_closing_year[,selected_vars], function(x) mean(x, na.rm = TRUE)) %>% 
  pivot_longer(everything(), names_to = "distress", values_to = "prop_closing")

non_closing <- map_df(non_closing_micro[,selected_vars], function(x) mean(x, na.rm = TRUE)) %>% 
  pivot_longer(everything(), names_to = "distress", values_to = "prop_alive")

left_join(non_closing, closing, by = "distress") %>% mutate(div = (prop_closing/prop_alive)*100)

rm(p1)
rm(p2)
rm(p3)
