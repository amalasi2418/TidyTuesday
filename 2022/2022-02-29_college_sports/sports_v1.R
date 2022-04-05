setwd("~/R/Infographics/sports")

library(tidyverse)
library(tidyr)
library(ggtext)
library(showtext)

font_add_google("Fira Sans","fira")
font_add_google("Fira Sans Extra Condensed","cond")

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

TN <- sports %>% filter(institution_name == "The University of Tennessee-Knoxville")


SEC_east <- c("The University of Tennessee-Knoxville","University of Florida",
              "Vanderbilt University","University of South Carolina-Columbia",
              "University of Missouri-Columbia","University of Georgia","University of Kentucky")

SEC_west <- c("The University of Alabama","University of Arkansas","Auburn University",
              "Louisiana State University and Agricultural & Mechanical College",
              "University of Mississippi",
              "Mississippi State University","Texas A & M University-College Station")


SEC_east_conf <- sports %>% filter(institution_name %in% SEC_east)

SEC_west_conf <- sports %>% filter(institution_name %in% SEC_west)

SEC_conf <- rbind(SEC_east_conf,SEC_west_conf)

SEC_conf %>% 
  filter(year == "2019" & sports %in% c("Basketball","Football")) %>%
  group_by(institution_name) %>% 
  summarise(exp = sum(total_exp_menwomen,na.rm = T),rev = sum(total_rev_menwomen,na.rm = T)) %>%
  ggplot(aes(rev,institution_name)) +geom_point()+
  geom_point(aes(exp,institution_name),col="red")


SEC_final <- SEC_conf %>% 
  filter(year == "2019" & sports %in% c("Basketball","Football")) %>%
  mutate(profit_m = rev_men - exp_men,
         institution_name = case_when(
           institution_name == "Louisiana State University and Agricultural & Mechanical College" ~ "Louisiana State University",
           TRUE ~ institution_name
         ))

SEC_wide <- SEC_final[,c(3,28,29)]

SEC_long <- spread(SEC_wide,sports,profit_m)


SEC_long1 <- SEC_long %>% 
  rowwise() %>% 
  mutate( diff = Football-Basketball) %>% 
  arrange(diff) %>%
  mutate(institution_name = factor(institution_name,institution_name))

SEC_long1 %>% ggplot() +
  geom_segment(aes(x = Basketball/1e6, xend = Football/1e6, y = institution_name, yend = institution_name), size =1) +
  geom_point(aes(Basketball/1e6,institution_name), color = "#2D4263", size = 3) +
  geom_point(aes(Football/1e6,institution_name), color = "#C84B31", size = 3) +
  labs(title = "Profitable sport of SEC: <span style = 'color:#C84B31;'>Football</span> or <span style = 'color:#2D4263;'>Basketball</span>",
       caption = "Data: Equity in Athletics Data Analysis | Graphic: Abhinav Malasi") +
  ylab("") + xlab("Profits (in million $)") +
  theme(panel.background = element_rect(color = "#191919", fill = "#191919"),
        plot.background = element_rect(color = "#191919", fill = "#191919"),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 60, hjust = .5, margin = margin(b=20)),
        plot.caption = element_text(size = 20, margin = margin(t=20)),
        axis.text = element_text(color = "#ECDBBA", size = 25),
        axis.title.x =  element_text(color = "#ECDBBA", size = 27, margin = margin(t=5)),
        axis.ticks.y = element_blank(),
        plot.margin = margin(c(20,5,20,5)),
        text = element_text(color = "#ECDBBA", family = "fira"))


ggsave("SEC3.png", last_plot(), width = 6, height = 5, units = "in")

