setwd("~/R/TidyTuesday/2022/2022-05-31_reputation")

library(tidyverse)
library(ggforce)
library(RColorBrewer)
library(cowplot)
library(showtext)

showtext_auto()

sysfonts::font_families_google()
sysfonts::font_add_google("Eczar","Eczar")

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

df = expand_grid(x=1:10,y=1:10)

radius = seq(.2,.8, by=.1)/2
df1 <- data.frame()
temp = data.frame()
for (i in 1:nrow(df)) {
  temp = cbind(df[i,],r=radius)
  df1 = rbind(df1,temp)
}

#df = cbind(df,r=rep(radius,100))

rep_2022 <-cbind(reputation,df1) %>% 
  mutate(color = case_when(score>=80 ~ "Excellent",
                           score >= 75 ~ "Very Good",
                           score >= 70 ~ "Good",
                           score >= 65 ~ "Fair",
                           score >= 55 ~ "Poor",
                           score >= 50 ~ "Very Poor",
                           TRUE ~ "Critical"))


rep_2022$color <- factor(rep_2022$color, levels = c("Excellent","Very Good","Good","Fair","Poor","Very Poor","Critical"))

rep_2022 %>% ggplot()+
  geom_circle(aes(x0=x,y0=y,r=r,color=color))

rep_2022_final <- merge(rep_2022,poll %>% filter(year==2021),by="company")


rep_2022_final_1 <- rep_2022_final[order(rep_2022_final$`2022_rank`, decreasing = FALSE), ]

plot <- rep_2022_final %>% 
  arrange(`2022_rank`,company) %>%
  #group_by(`2022_rank`) %>%
  ggplot()+
  geom_circle(aes(x0=x,y0=y,r=r,color=(color)))+
  geom_text(aes(x,y,label=`2022_rank`))+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~ company,scales = "free")


label_facet <- function(original_var, custom_name){
  lev <- levels(as.factor(original_var))
  lab <- unique(custom_name)
  names(lab) <- lev
  return(lab)  
}


plot <- rep_2022_final_1 %>% 
  #arrange(`2022_rank`,company) %>%
  #group_by(`2022_rank`) %>%
  ggplot()+
  geom_circle(aes(x0=x,y0=y,r=r,color=color),size=1)+
  geom_text(aes(x,y,label=`2022_rank`),family = "Eczar",size=15)+
  scale_color_brewer(palette = "Dark2", name = "Score",
  guide = guide_legend(nrow = 1)) +
  #scale_color_manual()
  theme_void()+
  theme(legend.position = "none",
        text = element_text(family = "Eczar"),
        strip.text.x = element_text(size = 40,margin=margin(b=5))) +
  facet_wrap(~ `2022_rank`,scales = "free",
             labeller = labeller(`2022_rank` = label_facet(rep_2022_final_1$`2022_rank`, rep_2022_final_1$company)))


ggsave("plot5.png", plot, width=22, height=25, units = "in")


plot_leg_1 <- data.frame(x0=0,y0=0,r=radius, attribute = c("Trust","Ethics","Growth","P&S","Citizenship","Vision","Culture"))

legend1 <- plot_leg_1 %>% ggplot()+
  geom_circle(aes(x0=x0,y0=y0,r=r),size=1) + 
  geom_text(aes(x= 0.5,y = r+.2,label=attribute),family = "Eczar",size=12,hjust = 0) +
  geom_text(aes(x= 0,y = 0,label=25),family = "Eczar",size=12) +
  geom_text(aes(x= 0.5,y = -.35,label="Rank"),family = "Eczar",size=12,hjust = 0) +
  geom_segment(
    aes(x = .2, y = r+.2,
    xend = 0, yend = r),
    lineend = "round", # See available arrow types in example above
    #linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.03, "npc")),
    colour = "black" # Also accepts "red", "blue' etc
  ) +
  geom_segment(
    aes(x = .2, y = r+.2,
        xend = 0.44, yend = r+.2),
    #lineend = "round", # See available arrow types in example above
    #linejoin = "round",
    size = 1, 
    colour = "black" # Also accepts "red", "blue' etc
  ) +
  geom_segment(
    aes(x = 0.005, y = -0.05, xend = .25, yend = -.35),
    size=1)+
  geom_segment(
    aes(x = 0.25, y = -0.35, xend = .45, yend = -.35),
    size=1,
    #lineend = "round",
    arrow = arrow(length = unit(0.03, "npc")))+
  coord_cartesian(clip = "off")+
  scale_x_continuous(expand = c(-0.6,1))+
  scale_y_continuous(expand = c(0,0.1))+
  theme_void()

legend1

plot_leg_2 = data.frame(matrix(NA,nrow = 6))
plot_leg_2$attribute = c("80 & above: Excellent","75-79: Very Good","70-74: Good","65-69: Fair","55-64: Poor","50-54: Very Poor")

plot_leg_2$attribute <- factor(plot_leg_2$attribute, levels = c("80 & above: Excellent","75-79: Very Good","70-74: Good","65-69: Fair","55-64: Poor","50-54: Very Poor"))


plot_leg_2 = cbind(plot_leg_2,x=rep(1,6),
                        xend=rep(1.5,6)-.2,
                        y=6:1,
                        yend=6:1)

legend2 <- plot_leg_2[,-1] %>% 
  ggplot()+ 
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend,color=attribute),size=1) + 
  geom_text(aes(x= xend+.2,y = yend,label=attribute),family = "Eczar",size=12, hjust=0) +
  scale_x_continuous(expand = c(0,1))+
  scale_color_brewer(palette = "Dark2")+
  labs(title = "Legend")+
  theme_void()+
  coord_cartesian(clip="off")+
  theme(legend.position = "none",
        plot.title = element_text(family = "Eczar", hjust=.75,size = 60, margin = margin(t=20,b=20)),
        plot.margin = margin(r=50,t=20,b=40))

legend2


caption <- ggdraw() + 
  draw_label(
    "Data: Axios Harris Poll 100 | Graphic: Abhinav Malasi",
    x = 0.8,
    #family = "Eczar",
    #color = "#FAF5E4",
    size=40,
    hjust = 0)+
  theme(
    plot.margin = margin(c(0,0,20,0)),
    text = element_text(family = "Eczar")
  )



title = ggplot() +
  labs(title = "CORPORATE REPUTATION RANKING 2022") +
  theme(
    plot.margin = margin(c(40,0,40,0)),
    plot.title = element_text(face = 'bold',size=100,hjust=.5),
    #plot.subtitle = element_text(size=50,lineheight = .3,hjust=.5),
    text = element_text(family = "Eczar")
  )

subtitle = ggplot() +
  labs(subtitle = "The reputation for the top 100 visible companies that made the mark on the American citizens.\nThe ranking is based on the seven identified attributes by Axios Harris Poll. Each circle denotes\na unique attribute which is color coded according to the score.") +
  theme(
    plot.margin = margin(c(20,0,40,0)),
    #plot.title = element_text(face = 'bold',size=80,hjust=.5),
    plot.subtitle = element_text(size=70,lineheight = .3,hjust=.5),
    text = element_text(family = "Eczar")
  )

panel0 <- plot_grid(title,subtitle,ncol = 1,rel_heights = c(1,1))

panel1 <- plot_grid(panel0,legend2,legend1,nrow = 1,rel_widths = c(1.2,.3,.35))#,rel_heights = c(1,.2,.2))

panel2 <- plot_grid(panel1,plot,caption,ncol = 1,rel_heights = c(.1,1,.03))+ 
  theme(panel.background = element_rect(color="white",fill="white"))

ggsave("reputation.png", panel2, width=22, height=30.8, units = "in")

