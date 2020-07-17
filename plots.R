library(tidyverse)
library(hrbrthemes)
library(mapdata)
library(ggthemes)
library(sf)
library(countrycode)
library(rworldmap)

world_points <- fortify(getMap(resolution = "high"))
world_points$region <- world_points$id

iso_code <- tibble(region = codelist$cow.name,
                   iso_code = codelist$genc3c) %>% 
  distinct() %>% 
  na.omit()

world <- left_join(world_points, iso_code)

diff <- setdiff(world$region, iso_code$region)

covid_data <- readxl::read_xlsx("COVID-19.xlsx") %>% 
  janitor::clean_names() %>% 
  group_by(countryterritory_code) %>%
  mutate(total_cases = sum(cases),
         total_deaths = sum(deaths),
         date_rep = as.Date(date_rep)) %>% 
  filter(date_rep == "2020-06-08") %>% 
  ungroup() %>% 
  rename(region = countries_and_territories,
         iso_code = countryterritory_code) %>% 
  select(region, iso_code, total_cases, total_deaths) %>% 
  na.omit()
  
covid_world <- left_join(world, iso_code) %>% 
  rename(country = region) %>% 
  left_join(covid_data) %>% 
  select(region, total_cases, total_deaths, long, lat, group) %>% 
  na.omit()

ggplot() +
  geom_polygon(data = covid_world, 
               aes(fill = total_cases,
                   x = long, y = lat, group = group), alpha = 1) +
  theme_void() +
  theme(panel.spacing = unit(c(0, 0, 0, 0), "null"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
        legend.position = c(0.15, 0.07),
        legend.direction = "horizontal")
