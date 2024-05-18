library(dplyr)
library(ggplot2)
library(tidyr)

fao <- readxl::read_xls("data/FAOSTAT_data_en_5-17-2024.xls")
minifao <- fao %>%
  filter(
    Survey == "Mexico - 2012",
    `Population Age Group` != "All") %>%
  mutate(age = `Population Age Group`, food = `Food Group`) %>%
  select(age, food, Value) 

tab <- minifao %>% 
  group_by(age, food) %>%
  summarize(
    value = round(sum(Value)))  %>%
  pivot_wider(
    id_cols = age, 
    names_from = food, 
    values_from = value, 
    values_fill = 0) 


tabmat <- data.frame(tab[, -1], row.names = tab$age)

library(pheatmap)
pheatmap(t(tabmat))

library(FactoMineR)
library(factoextra)

CA(tabmat)


consotab <- actphys_sedent_decode %>%
  mutate(TV = cut(tv_duree, c(0:5, 10))) %>%
  select(NOIND, POPULATION, TV) %>%
  left_join(
    conso_ca_indiv_decode %>%
      select(NOIND, POPULATION, conso_ca_nb), 
            by = "NOIND") %>%
  drop_na() %>%
  mutate(conso_ca_nb = ifelse(conso_ca_nb >= 5, "5p", conso_ca_nb)) %>%
  count(TV, conso_ca_nb) %>%
  pivot_wider(
    id_cols = TV,
    names_from = conso_ca_nb,
    values_from = n,
    values_fill = 0)

consotabmat <- data.frame(consotab[, -1], row.names = consotab$TV)
CA(consotabmat)




consotab <- actphys_sedent_decode %>%
  mutate(TV = cut(tv_duree, c(0:5, 10))) %>%
  select(NOIND, POPULATION, TV) %>%
  left_join(
    fpq_decode %>%
      select(NOIND, POPULATION, LEG_salade_freq_M), 
    by = "NOIND") %>%
  drop_na() %>%
  mutate(LEG_salade_freq_M = cut(LEG_salade_freq_M , c(0, 10, 20, 25, 35))) %>%
  count(TV, LEG_salade_freq_M) %>%
  pivot_wider(
    id_cols = TV,
    names_from = LEG_salade_freq_M,
    values_from = n,
    values_fill = 0)

consotabmat <- data.frame(consotab[, -1], row.names = consotab$TV)
CA(consotabmat)

writexl::write_xlsx(consotabmat, path = "data/salad_and_tv.xlsx")


