setwd("~/R/TidyTuesday/2022/2022-06-21_Juneteenth")


library(tidyverse)
library(showtext)

font_add_google("Open Sans","Roboto")

showtext_auto()

slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')





freq_slave <- slave_routes %>% 
  group_by(year_arrival,port_origin) %>% 
  summarise(slave_freq = sum(n_slaves_arrived,na.rm = TRUE), 
            ship_freq = n())

sum(slave_routes$n_slaves_arrived,na.rm = T)
sum(freq_slave$ship_freq)


bg = "#ffc0cb"

bg = "#dfd3c2"

slave_routes %>% group_by(year_arrival) %>% summarise(slave_freq = sum(n_slaves_arrived,na.rm = TRUE),
                                                      ship_freq = n()) %>%
  ggplot(aes(year_arrival,slave_freq,size=ship_freq)) +
  geom_point() +
  scale_y_continuous(labels = c(0,paste0(seq(20,80,by=20),"k"))) +
  xlab("") + ylab("")+
  labs(title = "Trans-Atlantic Slave Trade",
       subtitle = "On record, 36110 trans-Atlantic voyages transported 5063299 slaves between 1514-1866\nfrom Africa. On the contrary, the actual numbers are estimated to be between 10-12 million.",
       caption = "Data: www.slavevoyages.org | Graphic: Abhinav Malasi",
       size = "Voyages:")+
  theme(panel.background = element_rect(color=bg,fill=bg),
        plot.background = element_rect(color=bg,fill=bg),
        panel.grid = element_blank(),
        plot.title = element_text(size=60),
        plot.subtitle = element_text(size=40,lineheight = .35,margin = margin(t=10,b=10)),
        plot.caption = element_text(size=25),
        plot.margin = margin(t=15,b=10,r=15,l=10),
        axis.text = element_text(size=30),
        legend.position = "top",
        legend.text = element_text(size=25),
        legend.title = element_text(size=25),
        legend.background = element_rect(color=bg,fill=bg),
        legend.key = element_rect(color=bg,fill=bg),
        text = element_text(family = "Open Sans"))


ggsave("transatlantic.png", last_plot(), width = 8, height = 6, units = "in")

