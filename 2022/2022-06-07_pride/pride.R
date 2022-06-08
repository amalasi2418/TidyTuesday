setwd("~/R/TidyTuesday/2022/2022-06-07_pride")

library(tidyverse)
library(showtext)
library(ggrepel)

showtext_auto()

sysfonts::font_families_google()
sysfonts::font_add_google("Nova Flat","Nova Flat")

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')
fortune_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv')
static_list <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv')
pride_sponsors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv')
corp_by_politicians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politicians.csv')
donors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv')


anti_donations_summary <- static_list %>% 
  filter(!is.na(`Pride?`)) %>% 
  group_by(`Pride?`,`HRC Business Pledge`) %>%
  summarise(donation = sum(`Amount Contributed Across States`)) %>% 
  ungroup() %>%
  mutate(percent = donation*100/sum(donation))

set.seed(12345)
anti_donations <- static_list %>% 
  filter(!is.na(`Pride?`)) %>% 
  mutate(quad = case_when(`Pride?` == TRUE & `HRC Business Pledge`==TRUE~1,
                          `Pride?` == TRUE & `HRC Business Pledge`==FALSE~2,
                          `Pride?` == FALSE & `HRC Business Pledge`==TRUE~3,
                          TRUE ~ 4),
         x = case_when(quad == 1~ runif(125,1.1,1.9),
                       quad == 2~ runif(125,1.1,1.9),
                       quad == 3~ runif(125,0.1,.9),
                       TRUE ~ runif(125,0.1,.9)),
         y = case_when(quad == 1~ runif(125,1.1,1.9),
                       quad == 2~ runif(125,0.1,.9),
                       quad == 3~ runif(125,1.1,1.9),
                       TRUE ~ runif(125,0.1,.9)),
         nudge_y = runif(125,0,1),
         label = case_when(
           `Amount Contributed Across States`>70000 ~ Company,
           TRUE~""))

# anti_donations%>% group_by(quad) %>% summarise(count=n())

# contributions from the top 10 donors
#sum(anti_donations[1:10,4])/sum(anti_donations[,4])

anti_donations %>% 
  ggplot(aes(x,y,size=`Amount Contributed Across States`)) +
  geom_point(color="#141414")+
  xlab("Pride Sponsor") + ylab("Pledge Signer")+
  geom_hline(yintercept = 1,size=1,color="#212121") +
  geom_vline(xintercept = 1,size=1,color="#212121") +
  geom_text(aes(label=label),color="#212121",nudge_y = .075,size = 10,family = "Nova Flat")+
  scale_size_continuous(labels = scales::comma,breaks=c(1e5,3e5,5e5)) +
  scale_x_continuous(breaks = c(.5,1.5),labels = c("FALSE","TRUE")) +
  scale_y_continuous(breaks = c(.5,1.5),labels = c("FALSE","TRUE")) +
  labs(title = "Anti-LGBTQ Corporate Campaign Donations",
       subtitle = "Double standards of corporations in the USA, where on one hand promote the LGBTQ\ncommunity and on the other hand funnel money to politicians with anti-LGBTQ agenda.\nThe data consists of corportations who either sponsor Pride or have signed pledge to\nsign HRC Business Statement opposing Anti-LGBTQ state legislation. The bubble size\ncorrelates to the amount of donations made to politicians in 6 US states. The top 10\ndonors contibuted to around 50% of the total donations.",
       caption = "Data: Data for Progress | Graphic: Abhinav Malasi",
       size = "Donations (in $):") +
  theme(panel.background = element_rect(fill="#FFEEEE",color = "#FFEEEE"),
        plot.background = element_rect(fill="#FFEEEE",color = "#FFEEEE"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(c(10,20,10,10)),
        plot.title = element_text(size=80),
        plot.subtitle = element_text(size=40,lineheight = .35,margin=margin(t=15,b=15)),
        plot.caption = element_text(size=30, margin=margin(t=10)),
        axis.text.y = element_text(angle=90,hjust = -.01),
        axis.text = element_text(size=30,color="#212121"),
        axis.title = element_text(size=40),
        legend.background = element_rect(fill="#FFEEEE",color = "#FFEEEE"),
        legend.text = element_text(size=35, margin = margin(r=-10,l=-15)),
        legend.title = element_text(size=40),
        legend.key = element_rect(fill="#FFEEEE",color = "#FFEEEE"),
        legend.position = "top",
        text = element_text(family = "Nova Flat", color="#141414"))


ggsave("pride.png",last_plot(),width = 8,height = 8, units = "in")

