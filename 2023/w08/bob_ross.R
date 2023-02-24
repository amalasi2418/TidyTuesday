setwd("~/Abhinav/tidytuesday/bob_ross")

library(tidyverse)
library(treemapify)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)
showtext_auto()

bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

unique(bob_ross$painting_title)

aa=bob_ross %>% group_by(painting_title) %>% summarise(count = n())

t1 = gsub(",","",bob_ross$color_hex)
t2 = gsub("[[]","",t1)
t3 = gsub("[]]","",t2)
t4 = gsub("[']","",t3)
tidytext::unnest_tokens(as.data.frame(t3))
strsplit(t4," ")
t5 = unlist(strsplit(t4," "))


t6 = table(t5) %>% t()

as.data.frame(t5)%>% group_by(t5) %>% summarise(count = n()) %>%
  ggplot(aes(area = count, fill=t5)) +
  treemapify::geom_treemap(show.legend = FALSE)+
  scale_fill_manual(values = t5)


##########################

tidytext::unnest_tokens(bob_ross %>% select(season,color_hex))

temp <- bob_ross %>% 
  select(season,color_hex) %>% 
  mutate(color_hex = gsub(",","",color_hex)) %>% 
  mutate(color_hex = gsub("[[]","",color_hex)) %>% 
  mutate(color_hex = gsub("[]]","",color_hex)) %>% 
  mutate(color_hex = gsub("[']","",color_hex)) 

tidytext::unnest_tokens(temp$season, temp$color_hex)

color_palette = unique(t5)

temp %>% 
  mutate(season = paste(sprintf("Season %02d", season))) %>%
  mutate(color_hex = strsplit(color_hex, " ")) %>% 
  unnest(color_hex) %>% 
  group_by(season, color_hex) %>%
  summarise(count = n()) %>%
  ggplot(aes(area = count, fill=color_hex)) +
  treemapify::geom_treemap(show.legend = FALSE)+
  scale_fill_manual(values = color_palette) +
  facet_wrap(~season,ncol = 5) +
  labs(title = "BOB ROSS PAINTING COLORS",
       subtitle = "The colors are from the paintings of Bob Ross featured in the TV series 'The Joy of Painting'. The colors are grouped by season\nand presented using a treemap, where the rectangle's size corresponds to the frequency of the color's usage, and its fill represents\nthe actual color used in the paintings.",
       caption = "Source: Bob Ross Paintings | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(fill="gray85",color="gray85"),
        panel.background = element_rect(fill="gray85",color="gray85"),
        plot.title = element_text(size = 100, margin = margin(t=10,b=10), face = "bold"),
        plot.subtitle = element_text(size = 50, lineheight = .25, margin = margin(t=10,b=20)),
        plot.caption = element_text(size = 30, margin = margin(b=10)),
        plot.margin = margin(t=20,b=10,l=10,r=10),
        strip.text = element_text(size=30),
        text = element_text(family = text))

ggsave("bob_ross.png",last_plot(), width = 10, height = 10, dpi=500)


