
p1 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = liquidity_p*100,  color = "Alive"), size = 1.2) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = liquidity_p*100, color = "Closed"), size = 1.2) +
  geom_vline(xintercept = 5, alpha = 0.4, linetype = "dashed", color = "blue")+
  scale_color_manual(name = "Status", values = c("Alive" = "#87E066", "Closed" = "#F57217")) +
  scale_x_continuous(limits = c(-100, 100), expand = c(0,0)) +
  labs(x = element_blank(), y = "Proportion below threshold (%)", title = "Liquidity ratio")+
  theme_light() +
  theme(
        text = element_text(size = 10, family = "serif"),
        legend.position = "none",   
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.direction = "horizontal",
        plot.title.position = "panel")



p2 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = equity_to_assets_p*100, color = "Alive"), size = 1.2) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = equity_to_assets_p*100, color = "Closed"), size = 1.2) +
  geom_vline(xintercept = 3, alpha = 0.4,color = "blue", linetype = "dashed")+
  scale_color_manual(name = "Status", values =  c("Alive" = "#87E066", "Closed" = "#F57217")) +
  labs(x = "Threshold", y = element_blank(), title = "Equity to assets ratio")+
  scale_x_continuous(limits = c(-100, 100), expand = c(0,0)) +
  theme_light() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        text = element_text(size = 10, family = "serif"),
        legend.position = "bottom",   
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.direction = "horizontal",
        plot.title.position = "panel")


p3 <- ggplot() + 
  geom_point(data = prop_below_threshold_alive, aes(x = threshold, y = profit_assets_p*100, color = "Alive"), size = 1.2) +
  geom_point( data = prop_below_threshold_closed, aes(x = threshold, y = profit_assets_p*100, color = "Closed"), size = 1.2) +
  scale_color_manual(name = "Status", values = c("Alive" = "#87E066", "Closed" = "#F57217")) +
  scale_x_continuous(limits = c(-100, 100), expand = c(0,0)) +
  geom_vline(xintercept = 0, alpha = 0.2, linetype = "dashed", color = "blue")+
  labs(x = element_blank(), y = element_blank(), title = "Profit to assets ratio") +
  theme_light() +
  theme(
        text = element_text(size = 10, family = "serif"),
        legend.position = "none",   
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.direction = "horizontal",
        plot.title.position = "panel")

combined <- p1 + p2 +p3 + plot_layout(guides= "keep", nrow = 1)

ggsave(combined, file = "../Graficos/combined ratios.png", width = 10, height = 4)

rm(p1)
rm(p2)
rm(p3)
rm(combined)