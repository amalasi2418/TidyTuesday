setwd("~/Abhinav/tidytuesday/artists_2023")

library(tidyverse)
library(showtext)
library(ggtext)

text = "MonteCarlo"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)
showtext_auto()

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

artists %>% 
  group_by(artist_name,year,book) %>% 
  summarise(moma = sum(moma_count_to_year),               
            whitney = sum(whitney_count_to_year))

artists %>% 
  group_by(artist_name) %>% 
  summarise(moma = sum(moma_count_to_year),               
            whitney = sum(whitney_count_to_year))

artists %>% 
  group_by(artist_race_nwi) %>% 
  summarise(moma = sum(moma_count_to_year),               
            whitney = sum(whitney_count_to_year))


artists %>% 
  group_by(book,artist_race_nwi) %>% 
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))
  ggplot(aes(book,artist_race_nwi,size=count))+
  geom_point()

artists %>% na.omit() %>% #filter(year>1970) %>%
  ggplot(aes(book,artist_name,color=book)) + 
  geom_tile(aes(fill=mean(space_ratio_per_page_total)))+
  scale_fill_continuous(trans = "log") +
  xlab("") + ylab("")

bg = "#141414"

artists %>% na.omit() %>% #filter(year>1970) %>%
  group_by(book,artist_name) %>%
  summarise(space_ratio = mean(space_ratio_per_page_total)) %>%
  ggplot(aes(book,artist_name,color=book)) + 
  geom_tile(aes(fill=space_ratio), show.legend = FALSE)+
  scale_fill_continuous(trans = "log") +
  coord_polar()+
  xlab("") + ylab("") +
  theme_void() +
  labs(title = "<span style = 'color:#00BFC4;'>Gardner</span> or <span style = 'color:#F8766D;'>Janson </span>   ? ",
       subtitle = "Each line represents an artist and line brightness indicates the mean space ratio per page.",
       caption = "Source: arthistory data package | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.title = element_markdown(size=180,color="#f5f5f5",hjust = .5, margin = margin(t=30, b=20)),
        plot.subtitle = element_text(size=100, color = "#f5f5f5",hjust = .5),
        plot.caption = element_text(size=50, color = "#f5f5f5",hjust = .5, margin = margin(b=20)),
        text = element_text(family = text))
  

ggsave("artists1.png", last_plot(), height = 8, width = 8, dpi=500)
