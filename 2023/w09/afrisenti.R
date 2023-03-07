setwd("~/Abhinav/tidytuesday/african_language")


library(tidyverse)
library(showtext)

text = "Numans"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)
showtext_auto()

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

language_origin <- merge(country_regions, language_countries, by="country") %>% 
  merge(language_scripts, by="language_iso_code") %>% 
  merge(languages, by="language_iso_code") 

sentiment_testing <- merge(language_origin,afrisenti, by = "language_iso_code")  

sentiment_testing %>% 
  group_by(region, country, script, language, label) %>% 
  summarise(count = n()) %>%
  ggplot(aes())

language_origin%>% 
  group_by(country) %>% 
  summarise(count = n()) 

unique(language_origin$country)

bg= "#4D455D"

sentiment_testing %>% 
  group_by(language, label) %>% 
  summarise(count = n()) %>%
  mutate(percent = count/sum(count),
         position = cumsum(percent)) %>%
  mutate(label = str_to_title(label)) %>%
  ggplot(aes(percent,language, fill = label)) + 
  geom_col() +
  #geom_text(aes(position,language, label =round(percent)),position=position_stack(vjust=0.5))+
  geom_text(aes(label =paste0(round(percent*100),"%")),position=position_stack(vjust=0.5),size=15, family=text)+
  xlab("") + ylab("")+
  scale_fill_manual(values = c("#E96479","#F5E9CF","#7DB9B6"))+
  scale_x_continuous(labels = scales::percent) +
  #coord_polar(theta = "y")+
  #scale_fill_gradientn(colors = c("white","red"), breaks = seq(0,12500,by=2500),limits = c(0,13137), labels=seq(0,12500,by=2500))+
  #facet_wrap(~country)+
  labs(title = "Sentiment analysis of 111k+ tweets in African languages",
       #subtitle = "conducted on 111720 tweets by native speakers.",
       caption = "Source: AfriSenti: Sentiment Analysis dataset for 14 African languages | Graphic: Abhinav Malasi",
       fill = "Sentiment:")+
  theme_void()+
    theme(plot.background = element_rect(fill=bg,color=bg),
          panel.background = element_rect(fill=bg,color=bg),
          plot.margin = margin(t=20,b=10,l=20,r=20),
          plot.title = element_text(size=100, face = "bold",lineheight = .15,hjust=3.5,margin = margin(t=15,b=20)),
          #plot.subtitle = element_text(size=100, face = "bold",lineheight = .15,hjust=.5,margin = margin(t=15,b=20)),
          plot.caption = element_text(size=50, hjust=.5, margin = margin(t=25,b=15)),
          axis.text.y = element_text(color="white", size=50, hjust=1),
          axis.text.x = element_text(color="white", size=50),
          legend.text = element_text(size=50, hjust=-5),
          legend.title = element_text(size=50),
          #legend.key.height = unit(.5,"cm"),
          #legend.key = element_text(vjust = .5),
          #legend.key.width = unit(1.5,"cm"),
          axis.ticks = element_blank(),
          legend.position = "top",
          text = element_text(color="white", family = text))


ggsave("afrisenti2.png", last_plot(),width = 10, height = 10, dpi=500)
