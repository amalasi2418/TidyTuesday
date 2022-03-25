# setwd("~/R/TidyTuesday/2022/2022-02-22")


library(tidyverse)
library(sysfonts)
library(showtext)

showtext_auto()

sysfonts::font_families_google()
sysfonts::font_add_google("Junge","Junge")

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom %>% 
  filter(Region_Name=="Asia") %>% 
  filter(!(country == "Timor-Leste")) %>%
  mutate(country = case_when(
    country == "Brunei Darussalam" ~ "Brunei",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Democratic People's Republic of Korea" ~ "North Korea",
    country == "Republic of Korea" ~ "South Korea",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Syrian Arab Republic" ~ "Syria",
    #country == "Timor-Leste" ~ "East Timor",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country)) %>% 
  ggplot(aes(year, fct_reorder(country,year,.desc=TRUE))) + 
  # fct_reorder(as.factor(country),year,.desc=F)
  geom_point(aes(color=Status,shape=Status),size=2.5) + 
  scale_shape_manual(values = c(16, 16, 16),labels = c("Free", "Not Free", "Partially Free")) +
  scale_color_manual(values = c("#52D273", "#E94F64", "#E5C454"),labels = c("Free", "Not Free", "Partially Free"))+
  #scale_fill_discrete(labels = c("Free", "Not Free", "Partially Free"))+
  ylab("")+xlab("")+
  labs(title = "Freedom in Asia 1995 - 2020")+
  labs(caption = "Source: Freedom House | Graphic: Abhinav Malasi")+
  facet_grid(Region_Name ~ ., scales = "free", space = "free") +
  theme(text = element_text(family="Junge"),
        strip.text.y = element_blank(),
        #strip.text.y = element_text(angle = -90,color="#D5D6DE"),
        strip.background = element_blank(),
        axis.text = element_text(color="#D5D6DE",size=22),
        #axis.ticks = element_blank(),
        legend.margin = margin(5, 0, -5, 0),
        legend.text = element_text(color="#D5D6DE",size = 20),
        legend.title = element_text(color="#D5D6DE",size = 20),
        legend.key = element_rect(fill="#333333",color="#333333"),
        legend.background = element_rect(color="#333333",fill="#333333"),
        legend.position = "top",
        #legend.position = c(0.05,1.1),
        legend.direction = "horizontal",
        legend.spacing.x = unit(.01, 'cm'),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, 0.5, 0.5), "cm"),
        panel.background = element_rect(fill="#333333",color="#333333"),
        plot.background = element_rect(fill="#333333",color="#333333"),
        plot.caption = element_text(size=15,face="bold",color="#D5D6DE"),
        #plot.subtitle = element_text(size=15,face="bold",color="white"),
        plot.title=element_text(size=45,hjust = 2,face="bold",color="#D5D6DE"))


ggsave("freedom_final.png",last_plot(), width=4,height = 6,units = "in")

