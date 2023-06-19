
## Summaries for all companies -----

# Summary of the definitive closing sample ----

final_sample_year <- micro_sample_closing_year %>% group_by(any) %>%
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(final_sample_year),
#           "../Salidas/closing firms.xlsx",
#           sheetName = "final firms by year",
#           row.names = FALSE)


final_sample_age <- micro_sample_closing_year %>% group_by(age_strata) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(final_sample_age),
#           "../Salidas/closing firms.xlsx",
#           sheetName = "final firms by age",
#           rowNames = FALSE,
#           append = TRUE)


final_sample_age_year <- micro_sample_closing_year %>% group_by(any, age_strata) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(final_sample_age_year),
#           "../Salidas/closing firms.xlsx",
#           sheetName = "final firms by age year",
#           rowNames = FALSE,
#           append = TRUE)


final_sample_sector <- micro_sample_closing_year %>% group_by(sector) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(final_sample_sector),
#           "../Salidas/closing firms.xlsx",
#           sheetName = "final firms by sector",
#           rowNames = FALSE,
#           append = TRUE)

final_sample_sector_year <- micro_sample_closing_year %>% group_by(any,sector) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(final_sample_sector_year),
#           "../Salidas/closing firms.xlsx",
#           sheetName = "final firms by sector year",
#           rowNames = FALSE,
#           append = TRUE)


# Summary of the definitive alive sample -----

alive_sample_year <- non_closing_micro %>% group_by(any) %>%
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(alive_sample_year),
#           "../Salidas/alive firms.xlsx",
#           sheetName = "alive firms by year",
#           rowNames = FALSE,
#           append = TRUE)


alive_sample_age <- non_closing_micro %>% group_by(age_strata) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(alive_sample_age),
#           "../Salidas/alive firms.xlsx",
#           sheetName = "alive firms by age",
#           rowNames = FALSE,
#          append = TRUE)


alive_sample_age_year <- non_closing_micro %>% group_by(any, age_strata) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(alive_sample_age_year),
#           "../Salidas/alive firms.xlsx",
#           sheetName = "alive firms by age year",
#           rowNames = FALSE,
#           append = TRUE)


alive_sample_sector <- non_closing_micro %>% group_by(sector) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(alive_sample_sector),
#           "../Salidas/alive firms.xlsx",
#           sheetName = "alive firms by sector",
#           rowNames = FALSE,
#           append = TRUE)

alive_sample_sector_year <- non_closing_micro %>% group_by(any,sector) %>% 
  summarise(emp = n_distinct(id))

#write.xlsx(as.data.frame(alive_sample_sector_year),
#           "../Salidas/alive firms.xlsx",
#           sheetName = "alive firms by sector year",
#           rowNames = FALSE,
#           append = TRUE)

rm(list = ls(pattern = "alive"))
rm(list = ls(pattern = "final"))

