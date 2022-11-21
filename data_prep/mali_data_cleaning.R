## Mali dataset exploration ----
## Joy Vaz
## 2022-03-31

#' Clean Mali data and explore data visualisation options for Chapter 3, 
#' This script will later be tidied up for the data prep folder. 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(here)
library(corrplot)
library(reshape2)
library(lubridate)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Import ----

mali_raw <- read_csv(here::here(
  "ch03_intro_to_data_viz/data/raw/britz_2021/base_smartphone.csv")) 

head(mali_raw)
names(mali_raw)

### Calculate coverage ----

mali_coverage <- mali_raw %>% 
  summarise_all(function(x) mean(!is.na(x))) %>% as.numeric()

data.frame(variable = names(mali_raw), coverage = mali_coverage)


### Select and rename variables ----

mali_01 <- mali_raw %>% 
  select(-c(Heure_Diarrhee, Code_Patient, 
            Calculation_Code, Calculation_Code2, 
            Pers_Plat), 
         age_mois = `Age-mois`,
         sang_selle1 = `Sang-Selle1`,
         sang_selle2 = `Sang-Selle2`,
         medic_tradi = `Medic-tradi`) %>% 
  rename_with(tolower)

### Misc cleaning ----

# Turn "Oui" and "Non" into 1 and 0 respectively

#' Check if the 8 columns that have "Oui" and "Non" have any NAs. If they do it
#' will mess up the ifesle() in the helperFunction below

mali_01 %>% 
  select(starts_with(c("sang", "fievre", "vomi", "allait", "medic"))) %>% 
  is.na() %>% 
  table() # no NAs

helperFunction <- function(x){ifelse(x=="Oui", 1,0)}

mali_02 <- mali_01 %>% 
  mutate(across(starts_with(c("sang", "fievre", "vomi", "allait", "medic")),
                helperFunction))

# Remove "%" signs and turn those variables from chr to num

isPercentage <- function(x){any(grepl("%$", x))}

mali_03 <- mali_02 %>%
  mutate_if(isPercentage, ~as.numeric(sub("%", "", .))/100)

# Change weird muac1 outlier

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = muac1)) +
  geom_point() # weird outlier

# Find out row number
which.max(mali_03$muac1) # 104
mali_03$age_mois[104]

#' Remove mid-upper arm circumference on row 104 because there's no way a child
#' at 0 months has an muac of 95??
#' 
mali_03$muac1[104] <- 9.5

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = muac1)) +
  geom_point() # weird outlier gone

### Correlation analysis ----

# Calculate Pearson correlation coefficent
data <- select_if(mali_03, is.numeric)

correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlation.df <- correlationMatrix %>% 
  as.data.frame() %>% 
  mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% 
  rename(feature1 = rowID, feature2 = variable, PCC = value) %>% 
  mutate(PCC = abs(PCC))

corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]
corrPairs %>%  arrange(PCC)

# Select highly correlated vars for scatterplot visualization
highlyCorrelated <- filter(corrPairs, PCC > 0.6) %>% 
  filter(PCC != 1.00)
highlyCorrelated


### Data visualization ----

ggplot(mali_03, mapping = aes(x = taille1, y = muac2)) + 
  geom_point(aes(color = factor(sexe))) +
  stat_smooth(method = "glm",
              col = "#C42126",
              se = FALSE,
              size = 1)

ggplot(mali_01, mapping = aes(x = taille1, y = age_mois, color = factor(sexe))) + 
  geom_point() +
  geom_smooth(col = "gray", size = 1) +
  geom_rug()

# Scatterplot for 5NG lesson -----

# Viral load by age (months)
ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point()

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(col = "skyblue")

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(mapping = aes(color = factor(vomissement1)))

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(mapping = aes(color = factor(vomissement1),
                           alpha = 0.7))

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(mapping = aes(color = factor(vomissement1),
                           alpha = 0.7)) +
  geom_smooth()

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(mapping = aes(color = factor(vomissement1),
                           alpha = 0.7)) +
  geom_smooth(col = "darkgray")

ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(mapping = aes(color = factor(vomissement1),
                           alpha = 0.7)) +
  geom_smooth(col = "darkgray", size = 1) +
  theme_bw()

# fever
ggplot(data = mali_03, 
       mapping = aes(x = age_mois, y = tauxviral1)) + 
  geom_point(mapping = aes(color = factor(fievre1),
                           alpha = 0.7),
             size = 2) + # size here is not an aestheic arg, bc not mapping to one of the columns in our dataset to 
  geom_smooth(col = "darkgray", size = 1) +
  theme_bw()

## practice questions

#' - change alpha
#' - change vomissement to fievre
#' - change geom smooth method

# Misc cleaning 2 ----

mali_04 <- mali_03 %>% 
  select(-ends_with("2")) %>% 
  rename_with(~gsub("\\d+", "", .))


as_Date <- function(x, format = c("ymd", "dmy", "mdy")){
  fmt <- lubridate::guess_formats(x, format)
  fmt <- c(unique(fmt), "%m/%d/%Y")
  y <- as.Date(x, format = fmt[1])
  for(i in seq_along(fmt)[-1]){
    na <- is.na(y)
    if(!any(na)) break
    y[na] <- as.Date(x[na], format = fmt[i])
  }
  y
}

as_Date(mali_04$date_inclusion)
mali_04$date_inclusion
mali_04$date_inclusion[113] <- "2020-02-18"
dates <- as_Date(mali_04$date_inclusion) %>% 
  str_replace("0020", "2020") %>% 
  ymd()
dates

malidd_05 <- mali_04 %>% 
  mutate(date_inclusion = dates)


# rename variables ----

names(malidd_05)

malidd_06  <- malidd_05 %>%  
  select(n, 
         admit_date = date_inclusion,
         sex = sexe,
         age_months = age_mois,
         height_cm = taille,
         muac_cm = muac,
         breastfeeding = allaitement,
         vomit = vomissement, 
         fever = fievre,
         bloody_stool = sang_selle,
         temp, freqrespi,
         viral_load = tauxviral,
         diarrhea_days = jours_diarrhee,
         diarrhea_episodes = nombre_selle,
         medication = medicaments,
         trad_medication = medic_tradi,
         commune = quartier,
         mother_education = educ_mere,
         father_education = educ_pere,
         household_size = pers_menage)

# coerce some columns to factors

cols <- c("sex",
          "breastfeeding",
          "vomit",
          "fever",
          "bloody_stool",
          "trad_medication")

malidd_06[cols] <- lapply(malidd_06[cols], factor) 

malidd <- malidd_06

# check coverage

malidd_coverage <- malidd %>% 
  summarise_all(function(x) mean(!is.na(x))) %>% as.numeric()

data.frame(variable = names(malidd), coverage = malidd_coverage)

# Save ----
write_csv(malidd, "ch03_intro_to_data_viz/data/clean/malidd.csv") 
save(malidd, file = here("ch03_intro_to_data_viz/data/clean/malidd.RData"))


# Bangladesh
bang_app <- read_csv(here::here(
  "chapter_03_intro_to_data_viz/data/raw/britz_2021/DiaPRData2020.01.01.csv"))
head(bang_app)
names(bang_app)

library(GGally)

mali_03[, 4:16] %>% 
  ggpairs(aes(color = factor(fievre1)), progress = F) +
  theme_bw()
ggpairs(mali_03)
