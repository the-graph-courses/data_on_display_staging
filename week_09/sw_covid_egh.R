
sw_covid <- read_csv("C:/Users/joych/Downloads/20221205_030245.csv")

sw_covid_long <- sw_covid %>%
  pivot_longer(cols = 2:6) %>% 
  rename(date = Time, canton = name, cases = value)

baseplot2 <- 
sw_covid_long %>% 
  ggplot(aes(x = date, y = cases, color = canton)) +
  geom_line()
baseplot2

baseplot2 +
  scale_x_date(date_breaks = "month",
               labels = label_date_short()) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  theme_minimal()

sw_covid %>%
  pivot_longer(cols = 2:6) %>% 
  ggplot(aes(x = Time, y = value, color = name)) +
  geom_line() +
  scale_x_date(date_breaks = "month",
               labels = label_date_short()) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  labs(title = "Daily COVID fake_cases in Switzerl and",
       color = "Canton") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank()) 

library(lubridate)

sample_df <- data.frame(
  month = rep(1:12, each = 31),
  day = rep(1:31, 12),
  fake_cases = sample(0:10000, size = 31*12),
  district = rep(c("D1", "D2", "D3"), 124)
  ) %>% 
  mutate(date = ymd(paste("2022", month, day, sep = "-"))) %>% 
  drop_na()

ggplot(sample_df, aes(date, fake_cases, color = district)) +
  geom_line() +
  labs(title = "Title")

 ggplot(sample_df, aes(date, fake_cases, color = district)) +
  geom_line() +
  labs(title = "Title") +
  scale_x_date(date_breaks = "month")

ggplot(sample_df, aes(date, fake_cases, color = district)) +
  geom_line() +
  labs(title = "Title") +
  scale_x_date(date_breaks = "month",
               labels = label_date_short())

ggplot(sample_df, aes(date, fake_cases, color = district)) +
  geom_line() +
  labs(title = "Title") +
  scale_x_date(date_breaks = "month",
               labels = label_date_short()) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
 