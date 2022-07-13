setwd("~/R/TidyTuesday/2022/2022-06-14_drought")


library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(tidycensus)
library(tigris)
library(sf)
library(gganimate)
library(showtext)

showtext_auto()

sysfonts::font_families_google()
font <- "Overpass"
font_add_google(font, font)

#drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')



TN <- drought_fips %>% 
  filter(State == "TN") %>% 
  mutate(fips_county = str_sub(FIPS,start = -3))


shape_file <- counties(state = "TN", cb = FALSE, resolution = "500k", year = 2021)


TN_shape_final <- merge(shape_file, TN,by.x="COUNTYFP",by.y="fips_county")



TN_shape_final=TN_shape_final %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         date_code = as.numeric(year)*1e4+as.numeric(month)*100+as.numeric(date))

TN_final=TN_shape_final %>% 
  filter(date_code>=20115624 & date_code <= 20176919)

TN_final=TN_final %>% group_by(FIPS,year, month) %>% 
  summarise(mean_DSCI = mean(DSCI)) %>%
  mutate(date_code = as.numeric(year)*1e4+as.numeric(month)*100) %>%
  arrange(.,date_code)


anim <- TN_final %>%
  mutate(label = paste0(year,"-",sprintf("%02d", as.numeric(month)))) %>%
  ggplot() + 
  geom_sf(aes(fill = mean_DSCI),color="#141414")+
  xlab("") + ylab("") +
  labs(title = "Droughts in Tennessee (August 2010 till January 2016)",
       subtitle = "\n{closest_state}",
       caption = "Data: National Integrated Drought Information System | Graphic: Abhinav Malasi") +
  scale_fill_gradientn(colours = c("blue","green","yellow","orange","red"),name="Drought intensity", labels = c("Low","High"),breaks=c(0,500),limits=c(0,500))+
  guides(fill=guide_colorbar(ticks.colour = NA)) +
  theme(plot.background = element_rect(fill = "#141414",color="#141414"),
        panel.background = element_rect(fill = "#141414",color="#141414"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=20,margin = margin(t=15)),
        plot.subtitle = element_text(size=15,margin = margin(t=15)),
        plot.caption = element_text(size=10,margin=margin(t=15)),
        legend.position = "top",
        legend.text = element_text(size=10,vjust = 2.5),
        legend.key.height = unit(3.5, 'mm'),
        legend.title = element_text(size=12,vjust=.8,hjust = 1.2),
        legend.margin = margin(b=-15),
        legend.background = element_rect(fill = "#141414",color="#141414"),
        text = element_text(color="white",family = font)) +
  transition_states(label)

mapGIF <- animate(anim, height = 1000, width = 1000, fps=20, duration = 20)
anim_save("drought.gif", animation=mapGIF)


