
# load dependencies
library(tidyverse)
library(magrittr)
library(janitor)
library(ggrepel)
library(gganimate)

# load data reading scripts
source("load_usafacts_data.R")

# load the usafacts dataset
load_usafacts_data()

# calculate state-by-state popsizes
state_pop_df <- usafacts$population %>% 
  group_by(State) %>% 
  summarize(popsize = sum(population))

# state-by-state confirmed cases
state_confirmed_df <- usafacts$confirmed %>% 
  filter(countyFIPS != 0) %>% 
  group_by(State, date) %>% 
  summarize(confirmed = sum(confirmed))

# state-by-state confirmed deaths
state_deaths_df <- usafacts$deaths %>% 
  filter(countyFIPS != 0) %>% 
  group_by(State, date) %>% 
  summarize(deaths = sum(deaths))


# merge data together
state_df <- 
  merge(state_pop_df, state_confirmed_df, by = 'State')

state_df <-
  merge(state_df, state_deaths_df, by = c('State', 'date'))


# convert state names
statenames <- setNames(state.name, state.abb)
state_df %<>% mutate(
  State = 
    ifelse(State %in% names(statenames), 
      statenames[state_df$State],
      state_df$State))


# Filter for the last date available
last_state_df <- state_df %>% filter(date == max(date))

##################
# Static version #
##################

plt <- 
ggplot(
  state_df %>% filter(date == max(date)), 
  aes(x = deaths / popsize * 1e5, 
      y = confirmed / popsize * 1e5, 
      # fill = State,
      size = popsize, 
      # color = State,
      label = State)) + 
  geom_point(alpha = 0.5, color = 'red') + 
  geom_text_repel(size=4, color = 'white') + 
  cowplot::theme_cowplot() + 
  scale_size_continuous(
    range = c(1,20), 
    breaks = c(1e6, 10e6, 30e6),
    name = 'Population Size',
    labels = scales::comma_format()) + 
  xlab("Cumulative Deaths per 100k") + 
  ylab("Cumulative Cases per 100k") + 
  guides(
    color = F,
    label = F,
    fill = F
  ) + 
  theme(plot.background = element_rect(fill = 'black'), 
  plot.title = element_text(color = 'white'),
  axis.title.x = element_text(color = 'white'),
  axis.title.y = element_text(color = 'white'),
  axis.line.x = element_line(color = 'white'),
  axis.line.y = element_line(color = 'white'),
  axis.ticks.x = element_line(color = 'white'),
  axis.ticks.y = element_line(color = 'white'),
  axis.text.x = element_text(color = 'white'),
  axis.text.y = element_text(color = 'white'),
  legend.text = element_text(color = 'white'),
  legend.title = element_text(color = 'white'),
  plot.subtitle = element_text(color = 'white')
  ) + 
  ggtitle("Coronavirus Cumulative Cases and Deaths per Capita",
  paste0("Data as of ", format(max(state_df$date), "%b %d %Y")))

ggsave(plot=plt, "covid_cases_and_deaths_per_100k.pdf", width=12, height=9)
ggsave(plot=plt, "covid_cases_and_deaths_per_100k.png", width=12, height=9)

plt2 <- plt + scale_x_log10(labels = scales::comma_format()) + 
  scale_y_log10(labels = scales::comma_format()) 

ggsave(plot=plt2, "covid_cases_and_deaths_per_100k_log.pdf", width=12, height=9)
ggsave(plot=plt2, "covid_cases_and_deaths_per_100k_log.png", dpi=300, width=12, height=9)

####################
# animated version #
####################

animation <- ggplot(
  state_df, 
  aes(x = deaths / popsize * 1e5, 
      y = confirmed / popsize * 1e5, 
      size = popsize, 
      label = State)) + 
  geom_point(alpha = 0.5, color = 'red') + 
  geom_text_repel(size=4.5, color = 'white') + 
  cowplot::theme_cowplot() + 
  scale_size_continuous(
    range = c(1,20), 
    breaks = c(1e6, 10e6, 30e6),
    name = 'Population Size',
    labels = scales::comma_format()) + 
  xlab("Cumulative Deaths per 100k") + 
  ylab("Cumulative Cases per 100k") + 
  guides(
    color = F,
    label = F,
    fill = F
  ) + 
  theme(plot.background = element_rect(fill = 'black'), 
  plot.title = element_text(color = 'white'),
  axis.title.x = element_text(color = 'white'),
  axis.title.y = element_text(color = 'white'),
  axis.line.x = element_line(color = 'white'),
  axis.line.y = element_line(color = 'white'),
  axis.ticks.x = element_line(color = 'white'),
  axis.ticks.y = element_line(color = 'white'),
  axis.text.x = element_text(color = 'white'),
  axis.text.y = element_text(color = 'white'),
  legend.text = element_text(color = 'white'),
  legend.title = element_text(color = 'white'),
  plot.subtitle = element_text(color = 'white'),
  plot.caption = element_text(color = 'white')
  ) + 
  expand_limits(x = 0, y = 0) + 
  # ggtitle("Coronavirus Cumulative Cases and Deaths per Capita",
  # "Data as of July 3rd 2020") + 
  scale_x_log10(labels = scales::comma_format(accuracy=0.01)) + 
  scale_y_log10(labels = scales::comma_format(accuracy=0.01)) + 
  labs(title = 'COVID-19 Cumulative Cases and Deaths per Capita', 
    x = 'Cumulative Deaths per 100k', y = 'Cumulative Cases per 100k',
    subtitle = "Data as of {frame_time}",
    caption = "Data available from https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/") +
  transition_time(date)

animate(animation, 
  height = 800, width = 1200, end_pause=20, fps=2,
  nframes=
    as.integer(max(state_df$date) - min(state_df$date)) + 21
   )
anim_save("covid19_cumulative_cases_and_deaths_animated.gif")
