setwd("~/R/TidyTuesday/2022/2022-07-19_technology_adoption")

library(tidyverse)
library(showtext)
library(RColorBrewer)
library(ggh4x)


showtext_auto()

sysfonts::font_families_google()
sysfonts::font_add_google("News Cycle","News Cycle")


technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')


####################################

vaccination <- technology %>% 
  filter(category=="Vaccines") %>% filter(variable %in% c("BCG","DPT","Pol3"))



vaccination$continent = countrycode::countrycode(vacc$iso3c,
                             origin = "iso3c",
                             destination = "continent")

vaccination$country = countrycode::countrycode(vacc$iso3c,
                                          origin = "iso3c",
                                          destination = "country.name")


# ratio calculation for facte panel size
vaccination %>% group_by(continent) %>% 
  summarise(count = unique(country)) %>% 
  summarise(c=n())


vaccination %>% 
  mutate(variable = str_replace(variable,"Pol3","Polio")) %>%
  ggplot(aes(year,country)) + 
  geom_tile(aes(fill=value)) + 
  xlab("") + ylab("") +
  labs(title="Vaccination adoption around the world in the last 4 decades",
       #subtitle = "Visualized the mortality rate of children under 5 per 1000 live births.",
       caption = "Data: NBER | Graphic: Abhinav Malasi") +
  scale_fill_distiller(palette = "Reds", na.value = "#de2d26",
                       direction = -1,name = "Vaccination coverage", limits =c(0,100), 
                       labels = paste0(seq(0,100,25),"%"))+
  scale_y_discrete(expand = c(.03,.03))+
  theme(panel.background = element_rect(color="black",fill = "#EEEDDE"),
        plot.background = element_rect(color="#EEEDDE",fill = "#EEEDDE"),
        panel.grid = element_blank(),
        plot.margin = margin(c(20,20,10,20)),
        plot.title = element_text(size=80, face="bold", hjust = 0.5),
        plot.title.position = "plot",
        #plot.subtitle = element_text(size = 60,hjust = 0.5, margin = margin(t=10,b=5)),
        plot.caption = element_text(size=30),
        axis.text = element_text(size=35),
        legend.position = "top",
        legend.title = element_text(size=40, lineheight = .35),
        legend.background = element_rect(color="#EEEDDE",fill = "#EEEDDE"),
        legend.key = element_rect(color="#EEEDDE",fill = "#EEEDDE"),
        legend.margin = margin(t=10, b=-10),
        legend.text = element_text(size=35),
        legend.spacing.y = unit(0.3, 'cm'),
        strip.background = element_rect(color="#EEEDDE",fill = "#EEEDDE"),
        strip.text = element_text(size=40),
        text = element_text(family = "News Cycle",color = "#3A3845")) +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = 1.25)) +
  facet_grid(continent~variable, scales = "free_y", space="free_y") +
  force_panelsizes(rows = c(54,34,47,42,13))


ggsave("vaccination.png", last_plot(), width = 15, height = 45, units = "in")

###################################################

steel <- technology %>% 
  filter(category == "Industry") %>% 
  filter(variable %in% c("steel_demand", "steel_production"))

text = "#EEEDDE"
bg = "#3A3845"

steel %>% group_by(year,variable) %>% 
  mutate(variable = str_replace(variable,"steel_demand","Demand"),
         variable = str_replace(variable,"steel_production","Supply")) %>%
  summarise(total = sum(value)) %>%
  ggplot(aes(x = year,y = total,  fill = as.factor(variable))) +
  labs(title = "Global Steel Outlook",
       caption = "Data: NBER, Annotations: wikipedia.org | Graphic: Abhinav Malasi",
       fill = "")+
  ggstream::geom_stream()+
  scale_fill_manual(values =  c("#2C5F2D","#101820ff"))+
  xlab("") + ylab("") +
  coord_flip() + 
  annotate(geom="text", x= 1967, y = 0, label="1967",size=7,color=text) +
  annotate(geom="text", x= 2019, y = 0, label="2019",size=7,color=text) +
  annotate(geom="text", x= 2019, y = 1150000, label="Demand slows down due\nto geopolitical events and\nCOVID-19",color=text,lineheight=.3,size=7) +
  geom_segment(aes(x = 2019, y = 600000, xend = 2019, yend = 120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom="text", x= 1973, y = 0, label="1973",size=7,color=text) +
  annotate(geom="text", x= 1975, y = 0, label="1975",size=7,color=text) +
  annotate(geom="text", x= 1974, y = 1175000, label="Recession in western world",size=7,color=text) +
  geom_segment(aes(x = 1974, y = 600000, xend = 1974, yend = 120000),color = text) +
  geom_segment(aes(x = 1973, y = 120000, xend = 1975, yend = 120000),color = text) +
  annotate(geom="text", x= 1973, y = 900000, label="First oil crisis",size=7,color=text) +
  geom_segment(aes(x = 1973, y = 600000, xend = 1973, yend = 120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom="text", x= 1979, y = 0, label="1979",size=7,color=text) +
  annotate(geom="text", x= 1979, y = 950000, label="Second oil crisis",size=7,color=text) +
  geom_segment(aes(x = 1979, y = 600000, xend = 1979, yend = 120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom="text", x= 1979, y = 0, label="1979",size=7,color=text) +
  annotate(geom="text", x= 1992, y = 0, label="1992",size=7,color=text) +
  annotate(geom="text", x= 1992, y = 1150000, label="Global production catches\nup with that of 1980",color=text,lineheight=.3,size=7) +
  geom_segment(aes(x = 1992, y = 600000, xend = 1992, yend = 120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom="text", x= 1993, y = 0, label="1993",size=7,color=text) +
  annotate(geom="text", x= 1993, y = -1050000, label="China overtakes USA",size=7,color=text) +
  geom_segment(aes(x = 1993, y = -600000, xend = 1993, yend = -120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom="text", x= 1996, y = 0, label="1996",size=7,color=text) +
  annotate(geom="text", x= 1996, y = -1250000, label="China surpasses Japan\nas the top producer",hjust=0.35,color=text,lineheight=.3,size=7) +
  geom_segment(aes(x = 1996, y = -600000, xend = 1996, yend = -120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom="text", x= 2017, y = 0, label="2017",size=7,color=text) +
  annotate(geom="text", x= 2017, y = -1300000, label="China surpasses rest of the world",size=7,color=text) +
  geom_segment(aes(x = 2017, y = -600000, xend = 2017, yend = -120000),color = text,
               arrow = arrow(length = unit(0.1, "cm"))) +
  theme(panel.background = element_rect(color=bg,fill = bg),
        plot.background = element_rect(color=bg,fill = bg),
        panel.grid = element_blank(),
        plot.margin = margin(c(20,20,10,20)),
        plot.title = element_text(size=60, face="bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(size=20),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size=20),
        legend.background = element_rect(color=bg,fill = bg),
        legend.key = element_rect(color=bg,fill = bg),
        legend.margin = margin(t=-20,b=10),
        legend.text = element_text(size=20),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.key.size = unit(0.5, 'cm'),
        text = element_text(family = "News Cycle",color = text)) 

ggsave("steel.png", last_plot(), width = 5, height = 5.5, units = "in")

########################

