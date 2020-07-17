# packages ----------------------------------------------------------------

library("tidyverse")
library("hrbrthemes")

# plot theme --------------------------------------------------------------

theme_plot <- theme(legend.position = "bottom",
                    legend.text = element_text(size = 10),
                    legend.title = element_text(size = 12),
                    strip.text.x = element_text(size = 12),
                    plot.title = element_text(size = 16, 
                                              margin = margin(b = 0.25, 
                                                              unit = "cm"),
                                              colour = "#545454"),
                    plot.subtitle = element_text(size = 12,
                                                 margin = margin(b = 0.75, 
                                                                 unit = "cm"),
                                                 colour = "#545454"),
                    axis.title.x = element_text(size = 12,
                                                face = "bold"),
                    axis.title.y = element_text(size = 12,
                                                face = "bold"),
                    panel.border = element_blank(),
                    axis.line.x = element_line(size = 0.50, 
                                               colour = "black", 
                                               linetype = 1),
                    plot.caption = element_text(size = 8, 
                                                face = "plain",
                                                colour = "#545454",
                                                hjust = 0,
                                                margin = margin(t = 0.50, 
                                                                unit = "cm")),
                    axis.text.y = element_text(face = "bold",
                                               colour = "#545454",
                                               margin = margin(r = 5, 
                                                               l = 5)),
                    axis.text.x = element_text(face = "bold",
                                               colour = "#545454",
                                               margin = margin(t = 5, 
                                                               b = 5))) 


# new cases ---------------------------------------------------------------

new_cases <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/new_cases.csv")

covid_new_cases <- new_cases %>% 
  janitor::clean_names() %>% 
  select(date, world) %>% 
  mutate(rollmean_world = zoo::rollmean(world, 7, na.pad = TRUE, align = "right"), 
         cumulative_world = cumsum(world),
         total_world = sum(world)) %>% 
  na.omit()

last_new_cases <- covid_new_cases %>% 
  tail(1) %>% 
  select(date, rollmean_world)

p1 <- covid_new_cases %>% 
  ggplot() +
  geom_bar(aes(x = date, y = world), 
           stat = "identity", 
           width = 1, 
           fill = "#f0bab7",
           alpha = 0.85) +
  geom_line(aes(x = date, y = rollmean_world), 
            color = "#ffffff", 
            size = 1.25) + 
  geom_point(data = last_new_cases, 
             aes(x = date, y = rollmean_world),
             size = 1.75,
             color = "#ffffff") +
  geom_line(aes(x = date, y = rollmean_world), 
            color = "#ef4d40", 
            size = 0.75) + 
  geom_point(data = last_new_cases, 
             aes(x = date, y = rollmean_world),
             size = 1,
             color = "#ef4d40") + 
  shadowtext::geom_shadowtext(data = last_new_cases,
                              aes(x = date, y = rollmean_world, label = scales::comma(round(rollmean_world, 0))), 
                              hjust = - 0.175, 
                              color = "#ef4d40",
                              bg.color = "white",
                              fontface = "bold",
                              size = 2, 
                              nudge_x = 0.05) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %d") +
  labs(title = "Global cases by date reported", 
       subtitle = "Line shows 7-day rolling average",
       x = "", 
       y = "",
       caption = "Graphic: Gabriel Cabrera G.\nSources: European CDC\n@GaboC_g") +
  scale_y_continuous(labels = scales::comma,
                     expand = expand_scale(mult = c(0, .1))) + 
  theme_ipsum() +
  theme_plot

ggsave("plots/covid_new_cases.png", width = 13, height = 8, dpi = 300, type = "cairo")


# new deaths --------------------------------------------------------------

new_deaths <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/new_deaths.csv")

covid_new_deaths <- new_deaths %>% 
  janitor::clean_names() %>% 
  select(date, world) %>% 
  mutate(rollmean_world = zoo::rollmean(world, 7, na.pad = TRUE, align = "right"), 
         cumulative_world = cumsum(world),
         total_world = sum(world)) %>% 
  na.omit()

last_new_deaths <- covid_new_deaths %>% 
  tail(1) %>% 
  select(date, rollmean_world)

p2 <- covid_new_deaths %>% 
  ggplot() +
  geom_bar(aes(x = date, y = world), 
           stat = "identity", 
           width = 1, 
           fill = "#dda9cf",
           alpha = 0.85) +
  geom_line(aes(x = date, y = rollmean_world), 
            color = "#ffffff", 
            size = 1.25) + 
  geom_point(data = last_new_deaths, 
             aes(x = date, y = rollmean_world),
             size = 1.75,
             color = "#ffffff") +
  geom_line(aes(x = date, y = rollmean_world), 
            color = "#bc3697", 
            size = 0.75) + 
  geom_point(data = last_new_deaths, 
             aes(x = date, y = rollmean_world),
             size = 1,
             color = "#bc3697") + 
  shadowtext::geom_shadowtext(data = last_new_deaths,
                              aes(x = date, y = rollmean_world, label = scales::comma(round(rollmean_world, 0))), 
                              hjust = - 0.175, 
                              color = "#bc3697",
                              fontface = "bold",
                              bg.color = "white",
                              size = 2, 
                              nudge_x = 0.05) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %d") +
  labs(title = "Global deaths by date reported", 
       subtitle = "Line shows 7-day rolling average",
       x = "", 
       y = "",
       caption = "Graphic: Gabriel Cabrera G.\nSources: European CDC\n@GaboC_g") +
  scale_y_continuous(labels = scales::comma,
                     expand = expand_scale(mult = c(0, .1))) + 
  theme_ipsum() +
  theme_plot

ggsave("plots/covid_new_deaths.png", width = 13, height = 8, dpi = 300, type = "cairo")


# join cases and deaths ---------------------------------------------------

cowplot::plot_grid(p1, p2, labels = c('', ''), label_size = 12)
ggsave("plots/covid_new_cases_and_deaths.png", width = 21, height = 8, dpi = 300, type = "cairo")

covid_new_cases %>% 
  select(total_world) %>% 
  distinct() %>% 
  pull() 

covid_new_deaths %>% 
  select(total_world) %>% 
  distinct() %>% 
  pull()

# cases per countries -----------------------------------------------------

cases_per_countries <- new_cases %>% 
  janitor::clean_names() %>% 
  select(-world) %>% 
  mutate_at(vars(afghanistan:zimbabwe), list(~ zoo::rollmean(., 7, na.pad = TRUE, align = "right"))) %>% 
  tidyr::gather(countries, cases, - date) %>% 
  drop_na(countries) %>% 
  group_by(countries) %>% 
  filter(cases >= 10) %>% 
  mutate(countries = str_replace(countries, "_", " "),
         countries = str_to_title(countries),
         days_elapsed = date - min(date),
         days_elapsed = as.numeric(days_elapsed),
         end_label = ifelse(date == max(date), countries, NA),
         end_point = ifelse(date == max(date), tail(cases, 1), NA)) %>% 
  ungroup()

top_20_cases <- cases_per_countries %>% 
  select(date, countries, end_point) %>% 
  drop_na(end_point) %>% 
  arrange(desc(end_point)) %>% 
  head(20) %>% 
  select(countries) %>% 
  pull()

cases_per_countries %>% 
  group_by(countries) %>% 
  filter(countries %in% top_20_cases) %>% 
  ggplot(aes(x = days_elapsed, y = cases, group = countries)) +
  geom_line(color = "#ffffff", size = 1.50) +
  geom_line(color = "#EB5E8D", size = 0.75) +
  geom_point(data = filter(cases_per_countries, countries %in% top_20_cases), 
             aes(x = days_elapsed, y = end_point),
             size = 2.25,
             color = "#ffffff") +
  geom_point(data = filter(cases_per_countries, countries %in% top_20_cases), 
             aes(x = days_elapsed, y = end_point),
             size = 1.50,
             color = "#EB5E8D") +
  labs(title = "New confirmed cases of Covid-19",
       subtitle = "Seven-day rolling average of new cases, by number of days since 10 average daily cases first recorded",
       x = "",
       y = "",
       caption = "Graphic: Gabriel Cabrera G.\nSources: European CDC\n@GaboC_g") +
  facet_wrap(~ countries, ncol = 4) +
  scale_x_discrete(limits = c(seq(0, 100, 50), 130)) +
  scale_y_log10(labels = scales::label_number_si()) +
  theme_ipsum() +
  theme(plot.background = element_rect(fill = "#fff1e5"),
        strip.text = element_text(colour = '#EB5E8D',
                                  face = "bold",
                                  margin = margin(b = 7.5))) +
  theme_plot

ggsave("plots/covid_new_cases_per_countries.png", width = 10, height = 12, dpi = 300, type = "cairo")


# deaths per countries ----------------------------------------------------

deaths_per_countries <- new_deaths %>% 
  janitor::clean_names() %>% 
  select(-world) %>% 
  mutate_at(vars(afghanistan:zimbabwe), list(~ zoo::rollmean(., 7, na.pad = TRUE, align = "right"))) %>% 
  tidyr::gather(countries, deaths, - date) %>% 
  drop_na(countries) %>% 
  group_by(countries) %>% 
  filter(deaths >= 3) %>% 
  mutate(countries = str_replace(countries, "_", " "),
         countries = str_to_title(countries),
         days_elapsed = date - min(date),
         days_elapsed = as.numeric(days_elapsed),
         end_label = ifelse(date == max(date), countries, NA),
         end_point = ifelse(date == max(date), tail(deaths, 1), NA)) %>% 
  ungroup()

top_20_deaths <- deaths_per_countries %>% 
  select(date, countries, end_point) %>% 
  drop_na(end_point) %>% 
  arrange(desc(end_point)) %>% 
  head(20) %>% 
  select(countries) %>% 
  pull()

deaths_per_countries %>% 
  group_by(countries) %>% 
  filter(countries %in% top_20_deaths) %>% 
  ggplot(aes(x = days_elapsed, y = deaths, group = countries)) +
  geom_line(color = "#ffffff", size = 1.50) +
  geom_line(color = "#0F5499", size = 0.75) +
  geom_point(data = filter(deaths_per_countries, countries %in% top_20_deaths), 
             aes(x = days_elapsed, y = end_point),
             size = 2.25,
             color = "#ffffff") +
  geom_point(data = filter(deaths_per_countries, countries %in% top_20_deaths), 
             aes(x = days_elapsed, y = end_point),
             size = 1.50,
             color = "#0F5499") +
  labs(title = "New deaths attributed to Covid-19",
       subtitle = "Seven-day rolling average of new deaths, by number of days since 3 average daily cases first recorded",
       x = "",
       y = "",
       caption = "Graphic: Gabriel Cabrera G.\nSources: European CDC\n@GaboC_g") +
  facet_wrap(~ countries, ncol = 4) +
  scale_x_discrete(limits = c(seq(0, 100, 50), 130)) +
  scale_y_log10(labels = scales::label_number_si()) +
  theme_ipsum() +
  theme(plot.background = element_rect(fill = "#fff1e5"),
        strip.text = element_text(colour = "#0F5499",
                                  face = "bold",
                                  margin = margin(b = 7.5))) +
  theme_plot

ggsave("plots/covid_new_deaths_per_countries.png", width = 10, height = 12, dpi = 300, type = "cairo")


# united states new cases -------------------------------------------------

us_new_cases <- read_csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv")

data("state")

state_name <- tibble(state = state.abb, state_name = state.name) 
  
us_cases_covid <- us_new_cases %>% 
  left_join(state_name) %>% 
  janitor::clean_names() %>% 
  mutate(date = paste0(substr(date, 1, 4), "-", substr(date, 5, 6), "-", substr(date, 7, 8)),
         date = as.Date(date)) %>% 
  select(date, state, state_name, positive_increase) %>%
  rename(cases = positive_increase) %>% 
  drop_na(state_name) %>% 
  group_by(state_name) %>% 
  arrange(state_name, date) %>% 
  mutate(roll_mean = zoo::rollmean(cases, 7, na.pad = TRUE, align = "right")) %>% 
  slice(which.max(roll_mean >= 10) : n()) %>% 
  mutate(days_elapsed = date - min(date),
         days_elapsed = as.numeric(days_elapsed),
         end_label = ifelse(date == max(date), state_name, NA),
         end_point = ifelse(date == max(date), tail(roll_mean, 1), NA)) %>% 
  arrange(state, days_elapsed)

us_cases_covid %>% 
  filter(state %in% c("FL", "TX", "CA", "NY")) %>% 
  ggplot(aes(x = days_elapsed, y = roll_mean, group = state, colour = state)) +
  # not inclue the state of FL, TX, CA and NY
  geom_line(data = filter(us_cases_covid, !state %in% c("FL", "TX", "CA", "NY")), 
            aes(x = days_elapsed, y = roll_mean, group = state, colour = state), 
            color = "grey", size = 0.25) +
  geom_line(color = "#ffffff", size = 1.75) +
  geom_line(size = 1.00) +
  geom_point(data = filter(us_cases_covid, state %in% c("FL", "TX", "CA", "NY")), 
             aes(x = days_elapsed, y = end_point),
             size = 2.50,
             color = "#ffffff") +
  geom_point(data = filter(us_cases_covid, state %in% c("FL", "TX", "CA", "NY")), 
             aes(x = days_elapsed, y = end_point),
             size = 1.50) +
  shadowtext::geom_shadowtext(data = filter(us_cases_covid, state %in% c("FL", "TX", "CA", "NY")),
                              aes(x = days_elapsed, y = roll_mean, label = end_label, group = state, colour = state), 
                              hjust = -0.10, 
                              # color = "#0F5499",
                              bg.color = "white",
                              fontface = "bold",
                              size = 2.5, 
                              nudge_x = 0.05) + 
  labs(title = "New confirmed cases of Covid-19 in New York, California, Florida and Texas",
       subtitle = "Seven-day rolling average of new cases, by number of days since 10 average daily cases first recorded",
       x = "",
       y = "",
       caption = "Graphic: Gabriel Cabrera G.\nSources: European CDC\n@GaboC_g") +
  scale_x_continuous(breaks = c(seq(0, 140, 20))) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000), 
                labels = scales::label_number_si()) + 
  scale_color_manual(values = c("#EB5E8D", "#9DBF57", "#0F5499", "#208FCE")) +
  theme_ipsum() +
  theme_plot +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fff1e5"))

ggsave("plots/covid_new_cases_us.png", width = 13, height = 8, dpi = 300, type = "cairo")


# united states new deaths ------------------------------------------------


