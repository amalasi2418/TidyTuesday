setwd("~/R/TidyTuesday/2022/2022-08-23_chips")

library(tidyverse)
library(showtext)
#library(ggtext)

showtext_auto()
text="Outfit"
aa=sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

chips <- readr::read_csv("chip_dataset.csv") %>% 
  janitor::clean_names() 

chips_df <- chips %>% mutate(year = release_date %>% lubridate::as_date() %>% lubridate::year())

chips_df %>% 
  #filter(type=="GPU") %>%
  ggplot(aes(year,transistors_million)) + 
  geom_tile(aes(fill=vendor,size=freq_m_hz))+
  #coord_polar(theta = "x")+
  scale_y_log10()+
  facet_wrap(~type) +
  theme_void()

chips_df %>% 
     #filter(type=="GPU") %>%
     ggplot(aes(year,transistors_million)) + 
     geom_tile(aes(color=vendor,size=freq_m_hz))+
     #coord_polar(theta = "y")+
     scale_y_log10()+
     facet_wrap(~type) +
  coord_equal()+
     theme_void()


bg = "#1B262C"

chips_df %>% 
  #filter(type=="GPU") %>%
  ggplot(aes(process_size_nm,transistors_million)) + 
  geom_point(size=4,color="#00B7C2")+
  scale_y_log10()+
  scale_x_log10()+
  xlab("Process size (nm)") +
  ylab("Transistors (in million)") +
  labs(title = "Is smaller process size better?",
       subtitle = "Process size in fabrication is the samllest possible size of a component. So far, 7 nm chips have been fabricated leading\nto fabricate more transistors per unit area on a standard chip. Both axes are in log scale showing a negative correlation.",
       caption = "Data: Chip dataset | Graphic: Abhinav Malasi") +
  #theme_void()+
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        panel.grid = element_blank(),
        plot.title = element_text(size=50),
        plot.subtitle = element_text(size=30,lineheight = .35,margin=margin(t=15)),
        plot.caption = element_text(size=20),
        plot.margin = margin(c(20,10,10,10)),
        axis.ticks = element_line(color="#E8FFC2"),
        axis.title = element_text(size=30),
        legend.background = element_rect(fill=bg, color=bg),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.key = element_rect(fill=bg, color=bg),
        axis.text = element_text(size = 25,color="#FDCB9E"),
        text = element_text(color="#FDCB9E",family=text))


ggsave("chips.png",last_plot(),width=8,height=6,units = "in")

