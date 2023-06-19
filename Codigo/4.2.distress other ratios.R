# Graphs for thresholds 

closing_ratios <- micro_sample_closing_year %>% ungroup() %>% select(r1_p, r3_p, e1_p, e2_p, current_ratio_p) 

survivals_ratios <- non_closing_micro %>% ungroup() %>% select(r1_p, r3_p, e1_p, e2_p, current_ratio_p) 


# CLOSED FIRMS
# set the ratios and thresholds to check
ratios <- c("r1_p", "r3_p", "e1_p", "e2_p", "current_ratio_p")
thresholds <- as.numeric(seq(-10, 10, by = 0.1))

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


r1_p <- cbind(prop_below_threshold_alive$threshold, prop_below_threshold_alive$r1_p, prop_below_threshold_closed$r1_p) %>% as.data.frame()
colnames(r1_p) <- c("threshold", "proportion_alive", "proportion_close")
r1_p <- r1_p %>% mutate(diff = proportion_close - proportion_alive)


r3_p <- cbind(prop_below_threshold_alive$threshold,prop_below_threshold_alive$r3_p, prop_below_threshold_closed$r3_p) %>% as.data.frame()
colnames(r3_p) <- c("threshold", "proportion_alive", "proportion_close")
r3_p <- r3_p %>% mutate(diff = proportion_close - proportion_alive)


e1_p <- cbind(prop_below_threshold_alive$threshold, prop_below_threshold_alive$e1_p, prop_below_threshold_closed$e1_p) %>% as.data.frame()
colnames(e1_p) <- c("threshold", "proportion_alive", "proportion_close")
e1_p <- e1_p %>% mutate(diff = proportion_close - proportion_alive)

e2_p <- cbind(prop_below_threshold_alive$threshold, prop_below_threshold_alive$e2_p, prop_below_threshold_closed$e2_p) %>% as.data.frame()
colnames(e2_p) <- c("threshold", "proportion_alive", "proportion_close")
e2_p <- e2_p %>% mutate(diff = proportion_close - proportion_alive)


current_ratio_p <- cbind(prop_below_threshold_alive$threshold, prop_below_threshold_alive$current_ratio_p, prop_below_threshold_closed$current_ratio_p) %>% as.data.frame()
colnames(current_ratio_p) <- c("threshold", "proportion_alive", "proportion_close")
current_ratio_p <- current_ratio_p %>% mutate(diff = proportion_close - proportion_alive)


p4 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = e2_p, color = "Alive"), size = 2) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = e2_p, color = "Closed"), size = 2) +
  scale_color_manual(name = "Status", values = c("Alive" = "#87E066", "Closed" = "#F57217")) +
  geom_vline(xintercept = 0, alpha = 0.2)+
  labs(x = "Threshold", y = "Proportion below threshold", title = "Endebtness ratio") +
  theme_light() +
  theme(legend.text = element_text(size = 11),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title.position = "plot")
