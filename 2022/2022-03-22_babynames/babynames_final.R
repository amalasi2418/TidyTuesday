setwd("~/R/Infographics/babynames")

library(tidyverse)
library(sysfonts)
library(showtext)
library(ggtext)

showtext_auto()

sysfonts::font_families_google()
sysfonts::font_add_google("Junge","Junge")

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


male=babynames%>% filter(sex=="M") %>% group_by(year) %>% summarise(max=max(n))
female=babynames%>% filter(sex=="M") %>% group_by(year) %>% summarise(max=max(n))

for(i in 1:nrow(male)){
male$first_letter[i] <- babynames %>% filter(year==male$year[i] & n==male$max[i]) %>% select(name) %>% substr(1,1)
female$first_letter[i] <- babynames %>% filter(year==male$year[i] & n==male$max[i]) %>% select(name) %>% substr(1,1)
}


babynames_let <- babynames %>% mutate(first_letter = substr(name,1,1))

babynames_summary_m <- babynames_let %>% filter(sex=="M") %>% group_by(year, first_letter) %>% summarise(total = sum(n)) #%>% select(year, first_letter,total) 
babynames_summary_f <- babynames_let %>% filter(sex=="F") %>% group_by(year, first_letter) %>% summarise(total = sum(n)) #%>% select(year, first_letter,total) 
babynames_summary <- babynames_let %>% group_by(year, first_letter,sex) %>% summarise(total = sum(n)) #%>% select(year, first_letter,total) 


babynames_summary %>% mutate(sex = case_when(
  sex == "F"~ "Female",
  TRUE ~ "Male")) %>% 
  ggplot(aes(year, first_letter, fill= total/1e5)) + 
  geom_tile(size=6) +
  scale_fill_gradient(low="#E5C454", high="#ffffe0",name="Count (in 10,000's)",
                      )+
  xlab("Year") + ylab("")+
  facet_grid(.~sex) +
  labs(title="How popular is the starting letter of your name?",
       subtitle = "Data spans between 1880-2017 for the registered baby names\nin the USA. The black patch indicates missing data.",
       caption = "Source: babynames R package | Graphic: Abhinav Malasi")+
  theme(plot.background = element_rect(fill="#141414",color="#141414"),
        panel.background = element_rect(fill="#141414",color="#141414"),
        panel.grid = element_blank(),
        text = element_text(family="Junge",color="#D5D6DE"),
        plot.caption = element_text(size=15,face="bold",color="#D5D6DE"),
        plot.subtitle = element_text(lineheight=.35,margin = margin(0, 0, 10, 0),size=25,hjust = .5,face="bold",color="#D5D6DE"),
        plot.title=element_text(size=37,hjust = .5,face="bold",color="#D5D6DE",margin = margin(b=10)),
        plot.margin = margin(c(8,12,5,5),unit = "mm"),
        axis.text = element_text(color="#D5D6DE",size=20),
        axis.title = element_text(color="#D5D6DE",size=23),
        legend.position = "top",
        legend.box = "horizontal",
        legend.key.size = unit(.5, 'cm'),
        legend.margin = margin(c(0,0,-4,0),unit = "mm"),
        legend.background = element_rect(fill="#141414",color="#141414"),
        legend.text = element_text(color="#D5D6DE", size = 18, vjust=4),
        legend.title = element_text(color="#D5D6DE", size = 22, hjust=-.5,vjust = .85,margin = margin(b=-5)),
        strip.background = element_rect(fill="#141414",color="#141414"),
        strip.text = element_text(color="#D5D6DE",size =22))

# babynames_summary_m %>% ggplot(aes(year, first_letter, fill= total/1e5)) + 
#   geom_tile() +
#   scale_fill_gradient(low="skyblue", high="blue") 
# 
# 
# babynames_summary_f %>% ggplot(aes(year, first_letter, fill= total/1e5)) + 
#   geom_tile() +
#   scale_fill_gradient(low="pink", high="red") 

ggsave("babynames_v11.png",last_plot(), width=4,height = 5,units = "in")

