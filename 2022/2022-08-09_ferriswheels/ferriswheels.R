setwd("~/R/TidyTuesday/2022/2022-08-09_ferriswheels")

library(tidyverse)
library(showtext)

text = "gravitas"

font_add_google(name = text, family = text)
showtext::showtext_auto()

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

wheels_df <- wheels %>% 
  filter(!(is.na(diameter))) %>% 
  select(2:4,7,13)

angle <- seq(0, 2*pi,length=(nrow(wheels_df)+1))

wheels_df <- wheels_df %>% 
  arrange(height, diameter) %>% 
  mutate(x = height*cos(angle[1:t_row]),
         y=height*sin(angle[1:t_row]))

bg="#FFE162"

col=c("#FF6464","#91C483","#EEEEEE")

wheels_df %>% 
  mutate(continent = case_when(country %in% c("USA","Mexico","Canada")~"Americas",
                                           country %in% c("UK", "Austria")~"Europe",
                                           TRUE~"Asia")) %>%
  ggplot(aes(x,y, color=continent)) + 
  geom_point(aes(size=diameter), show.legend = FALSE) +
  geom_segment(xend=0,yend=0,size=1) +
  scale_color_manual(values = col)+
  annotate(geom = "text", x=-220, y=225, label="Chicago wheel", family=text,size=6,color="#414141")+
  annotate(geom = "text", x=675, y=-145, label="Golden Gate Flyer", family=text,size=6,color="#414141")+
  annotate(geom = "text", x=242, y=-395, label="London Eye", family=text,size=6,color="#414141")+
  annotate(geom = "text", x=655, y=-250, label="Beijing Great Wheel", family=text,size=6,color="#414141")+
  labs(title="Ferris wheels",
       subtitle = "Chicago wheel, the oldest ferris wheel was opened in 1893. The other\nlabelled ferris wheels are the tallest ones in Americas, Asia, and Europe.\nThe height and diameter of the ferris wheels are represented by the line\nsegment and dots, respectively. The country of the wheels are color coded.",
       caption="Data: ferriswheels package by Emil Hvitfeldt | Graphic: Abhinav Malasi")+
  coord_fixed()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill=bg,color=bg),
        plot.background = element_rect(fill=bg,color=bg),
        plot.title = element_text(hjust = .5,size=70,margin = margin(t=10)),
        plot.subtitle = element_text(hjust = .5,margin = margin(t=15),size=25,lineheight = .35),
        plot.caption = element_text(size=15,margin = margin(t=30)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.position = c(.8,.8),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        text=element_text(family = text,color="#414141"))


ggsave("ferriswheel.png",last_plot(),height = 6, width=7,units = "in")

