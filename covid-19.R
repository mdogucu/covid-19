library(tidyverse)
library(gridExtra)

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

## Reading in the Data
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") 
death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") 
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")


confirmed_long <- confirmed %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  group_by(`Country/Region`) %>% 
  summarise_all(funs(sum)) %>% 
  pivot_longer(cols = -`Country/Region`,
               names_to = "date",
               values_to = "n_cases")  %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(n_cases != 0) %>% 
  group_by(`Country/Region`) %>% 
  mutate(n_day = row_number())


death_long <- death %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  group_by(`Country/Region`) %>% 
  summarise_all(funs(sum)) %>% 
  pivot_longer(cols = -`Country/Region`,
               names_to = "date",
               values_to = "n_death")  %>% 
  mutate(date = lubridate::mdy(date))

recovered_long <- recovered %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  group_by(`Country/Region`) %>% 
  summarise_all(funs(sum)) %>% 
  pivot_longer(cols = -`Country/Region`,
               names_to = "date",
               values_to = "n_recovered")  %>% 
  mutate(date = lubridate::mdy(date))

long_data <- left_join(confirmed_long, death_long) %>% 
  left_join(recovered_long) %>% 
  filter(`Country/Region` == "Turkey" |
                      `Country/Region` == "Germany" |
                      `Country/Region` == "Italy" |
                      `Country/Region` == "Iran"  |
                      `Country/Region` == "Korea, South" |
                      `Country/Region` == "United Kingdom")



## Recoding countries into Turkish

long_data$`Country/Region` <-
  recode(long_data$`Country/Region`,
         "Turkey" = "Türkiye",
         'Iran' = 'İran',
         'Germany' = 'Almanya',
         'Italy' = 'İtalya',
         'Korea, South' = 'Güney Kore',
         'United Kingdom' = 'Birleşik Krallık')


## Visualizing Number of Cases

cases <- long_data  %>%
  rename(Ülke = `Country/Region`) %>% 
  ggplot(aes(x = n_day, y = n_cases, 
             color = Ülke)) +
  geom_line() +
  labs(x = "İlk vakadan itibaren geçen gün sayısı",
       caption = "Eğitim amaçlıdır. @MineDogucu",
       y = 'Vaka sayısı') +
  theme(
    plot.caption = element_text(size = 8),
    legend.text = element_text(size = 8))


cases

ggsave("figs/cases.png", width = 8, height = 6)

cases_tr <- long_data  %>%
  filter(`Country/Region` == 'Türkiye') %>% 
  ggplot(aes(x = n_day, y = n_cases)) +
  geom_line() +
  labs(x = "İlk vakadan itibaren geçen gün sayısı",
       caption = "Eğitim amaçlıdır. @MineDogucu",
       y = 'Vaka sayısı',
       title = "Türkiye") +
  theme(
    plot.caption = element_text(size = 8))
cases_tr

ggsave("figs/cases_tr.png", width = 8, height = 6)


## Visualizing Number of Deaths

deaths <- long_data  %>%
  rename(Ülke = `Country/Region`) %>% 
  ggplot(aes(x = n_day, y = n_death, 
             color = Ülke)) +
  geom_line() +
  labs(x = "İlk vakadan itibaren geçen gün sayısı",
       caption = "Eğitim amaçlıdır. @MineDogucu",
       y = 'Ölüm sayısı') +
  theme(
    plot.caption = element_text(size = 8),
    legend.text = element_text(size = 8))

deaths

ggsave("figs/deaths.png" , width = 8, height = 6)

deaths_tr <- long_data  %>%
  filter(`Country/Region` == 'Türkiye') %>% 
  ggplot(aes(x = n_day, y = n_death)) +
  geom_line() +
  labs(x = "İlk vakadan itibaren geçen gün sayısı",
       caption = "Eğitim amaçlıdır. @MineDogucu",
       y = 'Ölüm sayısı',
       title = "Türkiye") +
  theme(
    plot.caption = element_text(size = 8))
deaths_tr

ggsave("figs/deaths_tr.png" , width = 8, height = 6)


## Visualizing Number of Recoverıes

recovery <- long_data  %>%
  rename(Ülke = `Country/Region`) %>% 
  ggplot(aes(x = n_day, y = n_recovered, 
             color = Ülke)) +
  geom_line() +
  labs(x = "İlk vakadan itibaren geçen gün sayısı",
       caption = "Eğitim amaçlıdır. @MineDogucu",
       y = 'İyileşen sayısı') +
  theme(
    plot.caption = element_text(size = 8),
    legend.text = element_text(size = 8))

recovery

ggsave("figs/recovery.png", width = 8, height = 6)

recovery_tr <- long_data  %>%
  filter(`Country/Region` == 'Türkiye') %>% 
  ggplot(aes(x = n_day, y = n_recovered)) +
  geom_line() +
  labs(x = "İlk vakadan itibaren geçen gün sayısı",
       caption = "Eğitim amaçlıdır. @MineDogucu",
       y = 'İyileşen sayısı',
       title = "Türkiye") +
  theme(
    plot.caption = element_text(size = 8))
recovery_tr

ggsave("figs/recovery_tr.png", width = 8, height = 6)








