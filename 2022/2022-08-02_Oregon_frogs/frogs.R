setwd("~/R/TidyTuesday/2022/2022-08-02_Oregon_frogs")

library(tidyverse)
library(waffle)
library(extrafont)
library(showtext)
library(ggtext)

showtext_auto()
text="Eczar"
#sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

bg = "#212121"

frogs %>% 
  group_by(HabType, Female) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count*100/sum(count)) %>%
  ggplot(aes(fill=Female,values=round(percent))) + 
  geom_waffle(color = bg,  
              flip = TRUE,
              size = .25,
              n_rows=5) +
  scale_fill_manual(
    name = NULL,
    values = c("#0F9EA1", "#AA00BA"),
    labels = c("Male", "Female")
  )+ 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  labs(title = "Oregon Spotted Frog's distribution",
       subtitle = "A square represents a percent of either <span style = 'color:#AA00BA;'>female</span> or <span style = 'color:#0F9EA1;'>male</span> frog.",
       caption = "Data: United States Geological Survey | Graphic: Abhinav Malasi") +
  facet_wrap(~HabType, nrow = 1, strip.position = "bottom")+
  hrbrthemes::theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = bg, color=bg),
        plot.background = element_rect(fill = bg, color=bg),
        plot.title = element_text(size=40,hjust = .5, margin = margin(t=10,b=10)),
        plot.subtitle = element_markdown(size=25,hjust = .5, lineheight = .35,margin = margin(t=0,b=10)),
        plot.caption = element_text(size=15,margin = margin(t=10,b=10)),
        axis.text = element_blank(),
        plot.margin = margin(l=-5,r=-5),
        strip.text = element_text(color="white",size=30),
        text = element_text(color="white", family = text))


ggsave("frogs.png", last_plot(), width = 4, height = 5, units = "in")  

