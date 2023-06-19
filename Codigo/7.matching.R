# Matching to create model sample

#load("../Datos/full sample.RData")

#Remove obs with NA in the target variable 
model_sample <- full_sample %>% filter(!is.na(distress_one))
model_sample <- model_sample %>% filter(distress_one ==1 | (distress_one ==0 & no_distress ==1))

#Check again final composition
model_sample%>% group_by(any) %>% summarise(emp = n(),
                                            distress_new = sum(distress_one),
                                            distress_0 = sum(distress_one==0),
                                            no_distress = sum(no_distress), 
                                            distress_again = sum(distress_two))

# Matching ----

years <-  seq(from = 2014, to = 2019, by = 1)

matched_sample_knn <- NULL

for (year in years){
  
  sample_year <- model_sample %>% filter(any == year)
  
  m.out <- matchit(distress_one ~ age + tamest + sector , data = sample_year,
                     method = "nearest", distance = "mahalanobis", ratio = 5)
  
  matched_data_year_knn <- match.data(m.out)
  
  matched_sample_knn <- rbind(matched_sample_knn, matched_data_year_knn)
  
}

summary(m.out)

matched_sample_knn <- matched_sample_knn %>% select(-weights, -subclass)


save(matched_sample_knn, file = "../Datos/matched_sample.RData")
#load("../Datos/matched_sample.RData")


loveplot <- love.plot(m.out, thresholds = c(m = .6), 
          var.order = "unadjusted", colors = c( "#F57217", "#87E066"),
          title = NULL,
          stars = "raw",size = 4, alpha = 0.6,
          sample.names = c("Unmatched", "Matched"),
          var.names = c(tamest = "Size",
                                 "sector_Retail and food services" = "Sector.Retail and Food Services", 
                                 "sector_Transport" = "Sector.Transport", 
                                 "sector_Energy"="Sector.Energy", 
                                 "sector_Information and communication" = "Sector.Information and Communication", 
                                 "sector_Industry" = "Sector.Industry",
                                 "sector_Rest" = "Sector.Rest",
                                 "sector_Construction and real state" = "Sector.Construction and Real State",
                                  age = "Age")) + 
  theme_light()+
  theme(legend.text = element_text(size = 13),
        text = element_text(size = 14, family = "serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title.align = 0.5)

ggsave(loveplot, filename = "../Graficos/balance2019.png")

plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~age + tamest + sector)

# Check matched sample summary 
sample_summary <- matched_sample_knn %>% group_by(any) %>% summarise(emp = n(),
                                            distress_new = sum(distress_one),
                                            distress_0 = sum(distress_one==0),
                                            no_distress = sum(no_distress==1), 
                                            distress_two_years = sum(distress_two==1), 
                                            prop_target = distress_new/emp)

