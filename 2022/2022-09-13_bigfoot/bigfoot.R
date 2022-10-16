setwd("~/Abhinav/tidytuesday/bigfoot")

library(tidyverse)
library(sf)
library(showtext)
library(ggtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()


bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')



  
bigfoot %>% filter(!(longitude =="NA"), !(latitude=="NA")) %>%

st_as_sf(coords = c("longitude","latitude")) %>% ggplot()+geom_sf(aes(size=number,color=season),alpha=.15)+
  facet_wrap(~season)


bigfoot %>% ggplot(aes(temperature_high-temperature_low,humidity, color=season)) + geom_point()

bigfoot %>% ggplot(aes(temperature_high-temperature_low,dew_point, color=season)) + geom_point()

bigfoot %>% ggplot(aes(dew_point,humidity, color=season)) + geom_point()

bigfoot %>% ggplot(aes(temperature_high-temperature_low,moon_phase, color=season)) + geom_point()

bigfoot %>% ggplot(aes(temperature_high-temperature_low,precip_intensity, color=season)) + geom_point()

bigfoot %>% mutate(year=lubridate::year(bigfoot$date)) %>% 
  filter(!is.na(year)) %>% group_by(year) %>% summarise(sightings=sum(number)) %>%
  ggplot(aes(year,sightings)) + geom_line()



#############
temp = read_csv("data.csv",skip = 4) %>% janitor::clean_names()

bg = "white"

bigfoot %>% mutate(year=lubridate::year(bigfoot$date)) %>% 
  filter(!is.na(year)) %>% group_by(year) %>% summarise(sightings=sum(number)) %>% filter(year > 1870) %>%
  ggplot(aes(year,sightings/1e6)) + geom_col() +
  geom_col(data=temp,aes(year,value),fill="red") +
  geom_text(x=1950,y=4.5,label="Is rising temperatures the reason for\nincreased Bigfoot sightings?",lineheight=.25,size=35, family=text)+
  geom_richtext(x=1950,y=3,label="The global rise in <span style = 'color:red;'>temperatures</span> and sightings of bigfoot have shown a dramatic increase<br>during the past four decades with a correlation of 0.82.",size=12,lineheight=.45,family=text,label.size = NA)+
  labs(#title = "Is Global Warming the cause of increased Bigfoot sightings?",
       caption = "Source: Data.World | Graphic: Abhinav Malasi") +
  scale_x_continuous(expand = c(0,0),breaks = seq(1880,2020,by=20)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(size=20, margin=margin(t=20,b=5)),
        plot.margin = margin(t=10,b=10,r=10,l=10),
        text = element_text(family = text))


ggsave("bigfoot.png",last_plot(),width = 10,height = 6,units = "in")


bigfoot_cor <- bigfoot %>% mutate(year=lubridate::year(bigfoot$date)) %>% 
  filter(!is.na(year)) %>% group_by(year) %>% summarise(sightings=sum(number)/1e6) %>% select(year,sightings)


df <- inner_join(bigfoot_cor,temp,by="year")

cor(df$sightings*1e6,df$value)
