setwd("~/Abhinav/tidytuesday/hydrowaste")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

bg = "#525252"
txt_col = "#eeeeee"

HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')



hydro_Waste <- HydroWASTE_v10 %>% st_as_sf(coords = c("LON_WWTP","LAT_WWTP")) %>% st_set_crs(st_crs(world))


plot <- world %>% filter(!admin=="Antarctica") %>%
  ggplot()+geom_sf(fill="#222831",color=bg)+
  geom_sf(data=hydro_Waste,aes(color=as.factor(QUAL_LOC)),size=.1) +
  labs(title = "Distribution of hydro wastewater plants around the world",
       caption = "Source: Macedo et al Earth Syst. Sci. Data, 14, 559-577, 2022 | Graphic: Abhinav Malasi",
       color="Waste water plant location accuracy:") +
  guides(colour = guide_legend(override.aes = list(size = 3)))+
  scale_color_manual(values = c("#ff6363","#ffab76","#fffda2","#baffb4"),labels = c("> 80%","between 50% and 80%","below 50%","not analyzed")) +
  theme(panel.background = element_rect(fill=bg,color=bg),
        plot.background = element_rect(fill=bg,color=bg),
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.text = element_text(size=25),
        legend.title = element_text(size=30),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(.15, 'cm'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t=20,b=10,r=15,l=15),
        plot.title = element_text(size=60),
        plot.caption = element_text(size=25),
        text = element_text(colour = txt_col,family = text))


ggsave("wastewater.png",plot,width=10,height=6,units="in")


