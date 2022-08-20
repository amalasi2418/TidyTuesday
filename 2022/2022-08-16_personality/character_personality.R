setwd("~/R/TidyTuesday/2022/2022-08-16_personality")

library(tidyverse)
library(ggforce)
library(showtext)
library(ggimage)
library(cowplot)

showtext_auto()
text="Oswald"
sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv') %>%
  filter(uni_name=="Marvel Cinematic Universe")

personality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')%>%
  filter(uni_name=="Marvel Cinematic Universe")

myers_briggs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/myers_briggs.csv') %>%
  filter(uni_name=="Marvel Cinematic Universe")



mb_marvel <- myers_briggs %>% 
  select(char_name, myers_briggs,avg_match_perc)%>% 
  group_by(char_name) %>% 
  top_n(1,avg_match_perc)


length =length(unique(mb_marvel$myers_briggs))

range_min = 50
range_max = 75

angle = seq(0, 2*pi, length = length+1)
r = 12*10
x = r*cos(angle[1:8])
y = r*sin(angle[1:8])


df = data.frame(x,y, 
                label=unique(mb_marvel$myers_briggs),
                r=r,
                x_pos=x*(r+14)/r, 
                y_pos=y*(r+14)/r)

circle = data.frame(x0=0,y0=0, rad=seq(2,12,length=6)*10)

delTheta = pi*5/180

mb_marvel_loc <- mb_marvel %>% 
  cbind(del_theta = c(-1.5*delTheta,1.5*delTheta,0,0,1.5*delTheta,-1.5*delTheta,1.5*delTheta,0,0,3*delTheta,0,-3*delTheta,-1.5*delTheta,0,0)) %>% 
  mutate(r_in = .4*avg_match_perc-18,
                     theta = case_when(myers_briggs=="ENTJ"~angle[1],
                                       myers_briggs=="ENFP"~angle[2],
                                       myers_briggs=="ESFJ"~angle[3],
                                       myers_briggs=="ENFJ"~angle[4],
                                       myers_briggs=="INTJ"~angle[5],
                                       myers_briggs=="ENTP"~angle[6],
                                       myers_briggs=="ISFJ"~angle[7],
                                       TRUE~angle[8]),
                     
                     x_in = r_in*cos(theta+del_theta)*10,
                     y_in = r_in*sin(theta+del_theta)*10,
                     image = case_when(char_name == "Tony Stark"~"images/Iron_Man.png",
                                       char_name == "Thor"~"images/Thor.png",
                                       char_name == "Thanos"~"images/Thanos.png",
                                       char_name == "Gamora"~"images/Gamora.png",
                                       char_name == "Loki"~"images/Loki.png",
                                       char_name == "Peggy Carter"~"images/Agent_Carter.png",
                                       char_name == "Hawkeye"~"images/Hawkeye.png",
                                       char_name == "Nick Fury"~"images/Nick_Fury.png",
                                       char_name == "Black Panther"~"images/Black_Panther.png",
                                       char_name == "Bruce Banner"~"images/Bruce_Banner.png",
                                       char_name == "Dr. Strange"~"images/Doctor_Strange.png",
                                       char_name == "Captain America"~"images/Captain_America.png",
                                       char_name == "Black Widow"~"images/Black_Widow.png",
                                       char_name == "Captain Marvel"~"images/Captain_Marvel.png",
                                       TRUE ~ "images/Peter_Jason_Quill.png"))


(framework <- df %>%
  ggplot() +
  geom_segment(aes(x=0,y=0,xend=x,yend=y), color="gray80")+
  geom_text( aes(x=x_pos,y=y_pos, label=label),size=14,family = text, color="#141414")+
  geom_circle(data=circle, aes(x0=x0,y0=y0,r=rad), color="gray80")+
  geom_text( data = circle, aes(x=rad*cos(pi/2),y=rad*sin(pi/2)), label=seq(50,75,by=5),size=10,family = text, color="#141414")+
  geom_text( data = circle, aes(x=rad*cos(-pi/2),y=rad*sin(-pi/2)), label=seq(50,75,by=5),size=10,family = text, color="#141414")+
  coord_equal()+
  theme_void())

bg="white"

main_plot=framework + 
  geom_circle(data=mb_marvel_loc,aes(x0=x_in,y0=y_in,r=15),color="#FF0000",fill="#FF0000")+
  geom_image( data=mb_marvel_loc,aes(x=x_in,y=y_in, image=image),size=.09) +
  labs(subtitle = "Myer-Briggs personality rating of Marvel Cinematic Universe characters",
       caption = "Data: Open-Source Psychometrics Project | Images: marvelcinematicuniverse.fandom.com, wikipedia.com | Graphic: Abhinav Malasi")+
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.subtitle = element_text(size=60,hjust=.5),
        plot.caption = element_text(size=30),
        plot.margin = margin(t=10,b=10),
        text=element_text(family = text, color="#141414"))



title = ggdraw() +
  draw_image("Marvel.png") 


final_plot <- plot_grid(title,main_plot,ncol=1,rel_heights = c(.15,1)) +
  theme(plot.background = element_rect(fill = bg, color = bg),
        panel.background = element_rect(fill = bg, color = bg),
        plot.margin = margin(c(20,10,20,10)))
  
ggsave("marvel_personality_traits.png", final_plot, width = 10, height=11, units = "in")  
