# Check distress in the full sample of companies

load("../Datos/full sample.RData")

# Chek proportion of distressed firms in the full sample across years -----

n_distinct(full_sample$id)

full_sample_distress <- full_sample %>% select(id, any, starts_with("distress"), low_equity_c, neg_prof_c, low_liquidity_c)

summary_dist_full <- full_sample_distress %>% group_by(any) %>% summarise(prop_neg_prof = mean(neg_prof_c, na.rm = TRUE),
                                                                          prop_low_equity = mean(low_equity_c, na.rm = TRUE),
                                                                          prop_low_liq = mean(low_liquidity_c, na.rm = TRUE),
                                                                          prop_d1 = mean(distress1_c, na.rm = TRUE),
                                                                          prop_d2 = mean(distress2_c, na.rm = TRUE),
                                                                          prop_d3 = mean(distress3_c, na.rm = TRUE),
                                                                          prop_d4 = mean(distress4_c, na.rm = TRUE),
                                                                          firms = n_distinct(id))

#write.xlsx(summary_dist_full, "distress proprotions.xlsx", sheetName = "full sample - no zombies", append = TRUE)

summary_dist_full_pivot <- summary_dist_full %>% select(-firms) %>% pivot_longer(-any, names_to = "distress_measure", values_to = "proportion")

ggprops <- ggplot(data = summary_dist_full_pivot, aes(x = any, y = (proportion)*100, group = distress_measure)) +
  geom_point(aes(color = distress_measure), size = 3, alpha = 0.6) + 
  geom_line(aes(color = distress_measure)) + 
  scale_color_brewer(guide = guide_legend(title = "Distress measure"), 
                     palette = "Set1", 
                     limits = c("prop_neg_prof",
                                "prop_low_liq",
                                "prop_low_equity",
                                "prop_d2",
                                "prop_d3",
                                "prop_d4",
                                "prop_d1"),
                     labels = c("R1. Negative profit",
                                "R2. Liquidity ratio <= 5%",
                                "R3. Equity to assets <= 3%",
                                "R1 & R2",
                                "R1 & R3",
                                "R2 & R3",
                                "R1 & R2 & R3")) +
  scale_y_continuous(limits = c(0, 50), expand = c(0,0)) +
  labs(y = "Proportion in distress (%)", x = element_blank()) +
  theme_light() +
  theme(legend.text = element_text(size = 10),
        text = element_text(size = 14, family = "serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title.align = 0.5)

ggsave("../Graficos/prop_distressed_by_year.png", plot = ggprops, height = 6, width = 6)

# proportion of distressed in just 1 year , 2 consecutive years or never -------

full_sample <- full_sample %>% mutate(distress_one = ifelse(distress1_c ==1 & distress1_p ==0, 1, 0),
                                      distress_two = ifelse(distress1_c ==1 & distress1_p ==1, 1, 0),
                                      no_distress = ifelse(distress1_c ==0 & distress1_p == 0, 1, 0),
                                      recovered = ifelse(distress1_p ==1 & distress1_c == 0, 1, 0))

full_sample %>% group_by(any) %>% summarise(distr_prop = mean(distress_one, na.rm = TRUE))



#drop companies with missing values in the distress measure for current and previous year
full_sample <- full_sample %>% filter(!(is.na(distress1_c) | is.na(distress1_p)))

#check resulting distribution
P_1 <- full_sample %>% group_by(any) %>% summarise(total_firms = n(),
                                                   total_distressed = sum(distress1_c, na.rm = T),
                                                   distressed_previous = sum(distress1_p, na.rm = T),
                                                   distressed_both = sum(distress_two, na.rm = T),
                                                   new_distressed = sum(distress_one, na.rm = T),
                                                   never_distress = sum(no_distress, na.rm = T),
                                                   recovered = sum(recovered, na.rm = T),
                                                   P = distressed_both/total_distressed,
                                                   Pno = new_distressed/total_distressed) %>% 
  as.data.frame()


write.xlsx(P_1, "../Salidas/sample composition.xlsx", sheetName = "proportion distressed", row.names = F)

save(full_sample, file = "../Datos/full sample.RData")

