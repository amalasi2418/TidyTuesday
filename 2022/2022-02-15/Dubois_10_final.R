# https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/2022/challenge10/data.csv

setwd("~/R/TidyTuesday/2022/2022-02-15")

library(tidyverse)
library(tidyr)

data <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge10/data.csv")

data$percent_not_enrolled <- 100-data$`Percent Enrolled`

data$rel_length <- c(100,120,160)

colnames(data)[2] <- c("percent_enrolled")

status1=relevel(data_long$status, "percent_enrolled")

data_long <- gather(data, status,percent,percent_enrolled:percent_not_enrolled,factor_key = TRUE)
  
data_long %>% 
  mutate(value=-rel_length*percent/100,status1=relevel(data_long$status, "percent_not_enrolled")) %>% select(-percent) %>% ggplot() + 
  geom_col(aes(x=Year,y=value,fill=status1),position=position_stack(),width = 5) +
  theme(legend.position = c(0.35,0.15),
        legend.key = element_rect(fill="#dfd3c2",color="#dfd3c2"),
        legend.background = element_rect(fill="transparent",color="transparent"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.spacing.y = unit(0.25, 'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5, .5, 0, .5), "cm"),
        panel.background = element_rect(fill="#dfd3c2",color="#dfd3c2"),
        plot.background = element_rect(fill="#dfd3c2",color="#dfd3c2"),
        plot.title=element_text(size=13,family="Open Sans",face="bold"),
        plot.subtitle=element_text(size=11,hjust=0.5,family="Open Sans",face="bold"))+
  scale_fill_manual(values = c("black","#ca183b"),
                    labels = c("PROPORTION OF CHILDREN NOT ENROLLED", "PROPORTION OF CHILDREN ENROLLED"),
                    guide = guide_legend(reverse = TRUE, byrow = TRUE))+
  xlab("") + ylab("") +
 #scale_fill_discrete(labels = c("Proportion of children ENROLLED", "Proportion of children not ENROLLED"))+
 # guides(color = guide_legend(order=2))+
  geom_text(aes(x=1876,y=0,label="1876"),vjust=-.4, size=8)+
  geom_text(aes(x=1886,y=0,label="1886"),vjust=-.4, size=8)+
  geom_text(aes(x=1896,y=0,label="1896"),vjust=-.4, size=8)+
  geom_text(aes(x=1876,y=-19,label="37.59%"), size=10)+
  geom_text(aes(x=1886,y=-34,label="56.66%"), size=10)+
  geom_text(aes(x=1896,y=-45.8,label="57.29%"), size=10)+
  #labs(title = "Proportion of total negro children of school age who are enrolled in the\n public schools.")+
  labs(title = "PROPORTION OF TOTAL NEGRO CHILDREN OF SCHOOL AGE WHO ARE ENROLLED IN THE PUBLIC SCHOOLS.")+
  labs(subtitle = "DONE BY ATLANTA UNIVERSITY\n")

  
  
ggsave("Dubois_10_final.png",last_plot(), width=3.5,height=4, unit="in")
