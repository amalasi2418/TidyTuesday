setwd("~/R/TidyTuesday/2022/2022-08-30_Pell_grant")

library(tidyverse)
library(showtext)

text="Overpass"

showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')


bg="#809A6F"

fig <- pell %>% 
  ggplot(aes(YEAR,RECIPIENT, size=AWARD, color=(AWARD))) + 
  geom_line() +
  labs(title = "Pell grants are beautiful",
       caption = "Data: US Dept. of Education | Graphic: Abhinav Malasi") +
  scale_y_log10()+
  coord_polar(theta = "x") + # change to y to generate the 2nd image
  theme_void() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = c("#A25B5B","#CC9C75","#D5D8B5"),trans="pseudo_log")+
  facet_wrap(~STATE, ncol = 8)+
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.title = element_text(size =80, face = "bold",hjust = 0.5,margin = margin(t=10,b=10)),
        plot.caption = element_text(size=30),
        strip.text = element_blank(),
        plot.margin = margin(t=10,b=10,r=10,l=10),
        text = element_text(family = text,color="#252525"))

ggsave("pell_grant_x.png", fig, width = 10, height = 10, units = "in")

