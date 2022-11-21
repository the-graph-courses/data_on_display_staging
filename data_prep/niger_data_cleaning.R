## Niger measles dataset exploration ----
## Joy Vaz
## 2022-04-04

#' Clean Niger dataset and explore data visualisation options for Chapter 3, 
#' This script will later be tidied up for the data prep folder. 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(here)
library(corrplot)
library(reshape2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Import ----

load("ch03_intro_to_data_viz/data/raw/blake_2020/data_reduced_resolution.rda")
View(cases_dpt)

load("C:/Rprojects/niameym_blake_2020/code_data/code_data/nigerm/data/pop.rda")
View(pop)

### Explore ----

head(cases_dpt)
summary(cases_dpt)
table(cases_dpt$year)
table(cases_dpt$department)
max(cases_dpt$week)
histogram(cases_dpt$cases)
pop$district %>% unique()
cases_dpt$cases %>% hist()
11*52


length(unique(pop$district))
length(unique(cases_dpt$department))

### Wrangle ----

cases_dpt$department <- cases_dpt$department %>% as.factor()
cases_rgn <- rename(cases_dpt, region = department)

### Save ----

write_csv(cases_rgn, here("ch03_intro_to_data_viz/data/clean/nigerm_cases_rgn.csv"))


nigerm <- cases_rgn
save(nigerm, file = here("ch03_intro_to_data_viz/data/clean/nigerm_cases_rgn.RData"))

### Visualise ----

plot(seq(0,15*2,by=2),niamey[,1],type='b',log='y',
     main='Niamey Cases', xlab='Week', ylab='Measles cases')


install.packages("ggridges")
library(ggridges)

ggplot(cases_dpt, aes(x = cases, y = factor(year))) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


cases_dpt %>% 
  filter(!cases == 0) %>% 
  ggplot(aes(x = cases, y = factor(year), fill = factor(year))) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

cases_dpt %>% 
  filter(!cases == 0) %>% 
  ggplot(aes(x = log(cases), y = factor(year), fill = factor(year))) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

cases_dpt %>% 
  filter(!cases == 0) %>% 
  ggplot(aes(x = log(cases), y = factor(week), fill = factor(week))) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# log and filter 0 cases
cases_dpt %>% 
  filter(!cases == 0) %>% 
  ggplot(aes(x = log(cases), y = factor(week), fill = ..x..)) +
  scale_fill_viridis_c() +
  geom_density_ridges_gradient() +
  theme_ridges() + 
  theme(legend.position = "none")

# keep 0 cases and no log
cases_dpt %>% 
  #filter(!cases == 0) %>% 
  ggplot(aes(y = cases, x = week)) +
  theme_ridges() + 
  theme(legend.position = "none")

cases_dpt %>%
  ggplot(aes(x = cases, y = factor(year), fill = ..x..)) +
  scale_fill_viridis_c() +
  geom_density_ridges_gradient(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")

cases_dpt %>% 
  filter(!cases == 0) %>% 
  ggplot(aes(x = log(cases), y = factor(year), fill = ..x..)) +
  scale_fill_viridis_c(alpha=0.6) +
  geom_density_ridges_gradient() +
  theme_ridges() + 
  theme(legend.position = "none")

cases_dpt %>% 
  filter(!cases == 0) %>% 
  ggplot(aes(x = log(cases), y = factor(year), fill = ..x..)) +
  scale_fill_viridis_c() +
  geom_density_ridges_gradient() +
  theme_ridges() + 
  theme(legend.position = "none")

