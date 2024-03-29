# HIV and AIDS data cleaning
## Joy Vaz
## 2022-11-05

#' Prepares two datasets for use in the data viz course
#' 
#' description: The total number of persons that are estimated to be infected by HIV, including those without symptoms, those sick from AIDS and those healthy due to treatment of the HIV infection
#' source_url: https://www.gapminder.org/data
#' accessed: November 2022
#' temporal range: 1990 - 2011
#' temporal scale: yearly
#' spatial range: global
#' spatial scale: country
#' raw file: data/raw/hiv_aids/people_living_with_hiv_number_all_ages.csv
#' clean file: data/clean/hiv_prevalence.csv
#' 
#' new_HIV 
#' description: The total number of persons that are estimated to be newly infected with HIV during the given year.
#' source_url: http://www.aidsinfoonline.org (via gapminder)
#' accessed: November 2022
#' temporal range: 1990 - 2011
#' temporal scale: yearly
#' spatial range: global
#' spatial scale: country
#' raw file: data/raw/hiv_aids/newly_hiv_infected_number_all_ages.csv
#' clean file: data/clean/hiv_incidence.csv



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Import data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

total_hiv_raw <- read_csv("data/raw/hiv_aids/people_living_with_hiv_number_all_ages.csv")

new_hiv_raw <- read_csv("data/raw/hiv_aids/newly_hiv_infected_number_all_ages.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Tidy up rows and columns ----

summary(total_hiv_raw) 
summary(new_hiv_raw)

# some years are character but some are numeric?

View(total_hiv_raw)
summary(new_hiv_raw)

# Convert "k" or "M" suffix to thousands and millions (numeric)

total_01 <- total_hiv_raw %>% 
  select(country, `1990`:`2009`) %>% 
  mutate(across(everything(), ~ifelse(grepl('k$',.), 
                                      as.numeric(gsub('k','',.))*1000,
                                      ifelse(grepl('M$',.),
                                             as.numeric(gsub('M','',.))*1000000,.)))) 

new_01 <- new_hiv_raw %>% 
  select(country, `1990`:`2009`) %>% 
  mutate(across(everything(), ~ifelse(grepl('k$',.), 
                                      as.numeric(gsub('k','',.))*1000,
                                      ifelse(grepl('M$',.) ,
                                             as.numeric(gsub('M','',.))*1000000,.))))


### Convert to long form ----

total_02 <- total_01 %>% 
  pivot_longer(!country, names_to = "year", values_to = "total_cases")

new_02 <- new_01 %>% 
  pivot_longer(!country, names_to = "year", values_to = "new_cases")

# Drop years for which there is no data

total_03 <- total_02 %>% drop_na() %>% 
  mutate(country = as.factor(country), 
         year = as.factor(year), 
         total_cases = as.numeric(total_cases))


new_03 <- new_02 %>% drop_na() %>% 
  mutate(country = as.factor(country), 
         year = as.factor(year), 
         new_cases = as.numeric(new_cases))




# Check if every country has data for every year 

total_03 %>% select(year) %>% as.vector() %>% table() %>% 
  as.data.frame() %>% filter(!Freq == length(unique(total_03$country)))

new_03 %>% drop_na() %>% select(year) %>% as.vector() %>% table() %>% 
  as.data.frame() %>% filter(!Freq == length(unique(new_03$country)))

## total_03 %>% select(country) %>% as.vector() %>% table() == 20
## new_03 %>% select(country) %>% as.vector() %>% table() == 20

# Check if every year has data for country year 
total_03 %>% select(country) %>% as.vector() %>% table() %>% 
  as.data.frame() %>% filter(!Freq == length(unique(total_03$year)))

new_03 %>% drop_na() %>% select(country) %>% as.vector() %>% table() %>% 
  as.data.frame() %>% filter(!Freq == length(unique(new_03$year)))

### Join datasets ---

# Check if they have the same countries
setdiff(unique(total_03$country), unique(new_03$country))
setdiff(unique(new_03$country), unique(total_03$country))
## total_03 has 78 more countries than new_03

# Join to create single df with both new and total cases
hiv_all <- left_join(total_02, new_02)

### Save as CSVs ----

write_csv(total_03, "data/clean/hiv_prevalence.csv")
write_csv(new_03, "data/clean/hiv_incidence.csv")
write_csv(hiv_all, "data/clean/hiv_incidence_prevalence.csv")

### Add population ----

# Prevalence dataset

hiv_total <- 
  read_csv(here("data/clean/hiv_prevalence.csv"))

setdiff(hiv_total$country, tidyr::population$country)

pop <- tidyr::population %>% 
  mutate(
    country = case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                        TRUE ~ country),
    country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                        TRUE ~ country),
    country = case_when(country == "Democratic Republic of the Congo" ~ "Congo, Rep.",
                        TRUE ~ country),
    country = case_when(country == "Cabo Verde" ~ "Cape Verde",
                        TRUE ~ country),
    country = case_when(country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                        TRUE ~ country),
    country = case_when(country == "Iran (Islamic Republic of)" ~ "Iran",
                        TRUE ~ country),
    country = case_when(country == "Kyrgyzstan" ~ "Kyrgyz Republic",
                        TRUE ~ country),
    country = case_when(country == "Republic of Korea" ~ "South Korea",
                        TRUE ~ country),
    country = case_when(country == "Lao People's Democratic Republic" ~ "Lao",
                        TRUE ~ country),
    country = case_when(country == "Republic of Moldova" ~ "Moldova",
                        TRUE ~ country),
    country = case_when(country == "Slovakia" ~ "Slovak Republic",
                        TRUE ~ country),
    country = case_when(country == "Swaziland" ~ "Eswatini",
                        TRUE ~ country),
    country = case_when(country == "United Republic of Tanzania" ~ "Tanzania",
                        TRUE ~ country),
    country = case_when(country == "United States of America" ~ "United States",
                        TRUE ~ country),
    country = case_when(country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                        TRUE ~ country),
    country = case_when(country == "Viet Nam" ~ "Vietnam",
                        TRUE ~ country),
    country = case_when(country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                        TRUE ~ country),
    country = case_when(country == "Serbia & Montenegro" ~ "Serbia",
                        TRUE ~ country)
    )


accentless <- iconv(pop$country,from="UTF-8",to="ASCII//TRANSLIT")

pop <- pop %>% 
  mutate(country = accentless)
     
setdiff(hiv_total$country, pop$country)

hiv_prev_pop <- left_join(hiv_total, pop) %>% 
  filter(!is.na(year), !is.na(population))

write_csv(hiv_prev_pop, "data/clean/hiv_prev_pop.csv")

# Incidence dataset

hiv_new <- 
  read_csv(here("data/clean/hiv_incidence.csv"))

setdiff(hiv_new$country, tidyr::population$country)

pop <- tidyr::population %>% 
  mutate(
    country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                        TRUE ~ country),
    country = case_when(country == "Democratic Republic of the Congo" ~ "Congo, Rep.",
                        TRUE ~ country),
    country = case_when(country == "Kyrgyzstan" ~ "Kyrgyz Republic",
                        TRUE ~ country),
    country = case_when(country == "Republic of Moldova" ~ "Moldova",
                        TRUE ~ country),
    country = case_when(country == "Swaziland" ~ "Eswatini",
                        TRUE ~ country),
    country = case_when(country == "United Republic of Tanzania" ~ "Tanzania",
                        TRUE ~ country),
    country = case_when(country == "United States of America" ~ "United States",
                        TRUE ~ country),
    country = case_when(country == "Serbia & Montenegro" ~ "Serbia",
                        TRUE ~ country)
  )

accentless <- iconv(pop$country,from="UTF-8",to="ASCII//TRANSLIT")

pop <- pop %>% 
  mutate(country = accentless)

setdiff(hiv_new$country, pop$country)

hiv_incd_pop <- left_join(hiv_new, pop) %>% 
  filter(!is.na(year), !is.na(population))

write_csv(hiv_incd_pop, "data/clean/hiv_incidence_pop.csv")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subset to high prev countries ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hiv_prev_pop_full <- 
  read_csv(here("data/clean/hiv_prev_pop.csv"))

prev_countries <- hiv_prev_pop_full$country %>% unique()

# Subset to countries with high incidence for better plotting
country_prev <- hiv_prev_pop_full %>% group_by(country) %>% 
  summarise(avg = mean(total_cases))

country_prev <- country_prev %>% 
  rename(country_prev, mean_prev = `.`)

overall_mean_prev <- mean(hiv_prev_pop_full$total_cases)

high_mean_prev <- country_prev %>% 
  filter(avg > overall_mean_prev)

high_prev_countries <- high_mean_prev$country %>% unique()


# Subset full to high
high_hiv_prev <- hiv_prev_pop_full %>% 
  filter(country %in% high_prev_countries)


# Write CSV
write_csv(high_hiv_prev, here("data/clean/high_hiv_prev_pop.csv"))

