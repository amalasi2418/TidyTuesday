setwd("~/R/TidyTuesday/2022/2022-07-12_EU_airlines")

library(tidyverse)
library(tidyr)
library(showtext)
library(ggforce)
library(ggtext)

showtext_auto()
text="Bitter"
sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')


flights_EU <- flights %>% 
  mutate(YEAR = lubridate::isoyear(FLT_DATE), MONTH = lubridate::month(FLT_DATE)) %>%
  mutate(WEEK_DAY = lubridate::wday(FLT_DATE, week_start = 1)) %>%
  mutate(WEEK_NO = lubridate::isoweek(flights$FLT_DATE)) %>%
  mutate(YEAR_WEEK = str_c(YEAR,"-",sprintf("%02d", as.numeric(WEEK_NO))))%>%
  group_by(YEAR_WEEK,WEEK_DAY) %>% 
  summarise(TOTAL = sum(FLT_TOT_1))


bg = "#212121"

flights_EU %>% 
  ggplot(aes(YEAR_WEEK,WEEK_DAY)) + 
  geom_tile(aes(fill=TOTAL)) +
  scale_y_reverse(breaks = 1:7,
                  labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_x_discrete(breaks = c("2016-01", "2017-01", "2018-01", "2019-01", "2020-01", "2021-01", "2022-01"), 
                   labels = 2016:2022) +
  xlab("") + ylab("") +
  labs(title = "Europe's commercial air traffic",
       subtitle = "(Pre and post pandemic)",
       caption = "Data: Eurocontrol | Graphic: Abhinav Malasi",
       fill = "Total daily flights") +
  #scale_fill_viridis_c()+
  viridis::scale_fill_viridis(option="magma", limits = c(0,60000),breaks = c(0,20000,40000,60000), labels = c("0","20k","40k","60k"))+
  guides(fill = guide_colorbar(title.position = "top")) +
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size=50, margin = margin(t=10, b=10)),
        plot.subtitle = element_text(hjust = 0.5,size=30),
        plot.caption = element_text(size=20),
        legend.position = "top",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.background = element_rect(fill=bg, color=bg),
        legend.title = element_text(hjust = 0.5, size=20,margin = margin(t=10)),
        legend.text = element_text(size=20),
        axis.text = element_text(color="white", size=25),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "white"),
        plot.margin = margin(c(15,10,10,15)),
        text = element_text(color="white",family = text))


ggsave("EU_flights.png",last_plot(),width=6, height = 4,units = "in")

