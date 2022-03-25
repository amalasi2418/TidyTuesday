# data: https://github.com/ajstarks/dubois-data-portraits/blob/259eea829c7c26551898b95543edbac23100d283/challenge/2022/challenge07/data.csv

# GRAPH 7

setwd("~/R/TidyTuesday/2022/2022-02-15")

library(tidyverse)
library(tidyr)
library(showtext)

font_add_google("Open Sans","Roboto")

showtext_auto()

data <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv")

# transform dataframe from wide to long
data_long <- gather(data,status, percentage, Widowed:Single, factor_key = TRUE)

data_long <- data_long %>% 
  mutate(percentage= case_when(
  Gender == "Male"~-percentage,
  TRUE ~ percentage
))  

# adding axis label on secondary axis
# https://github.com/tidyverse/ggplot2/issues/3171
guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}


limit = c(seq(100,10,-10),seq(0,100,10))

# grid lines
x = seq(-100,100,2)
y = rep(0.5,length(x))
y1 = rep(9.5,length(x))

df = data.frame(x=x,y=y,xend=x,yend=y1)
yy = seq(.5,9.5,1)
x1 = rep(-100,length(yy))
x2 = rep(100,length(yy))

df1= data.frame(x=x1,y=yy,xend=x2,yend=yy)


##### plotting

data_long %>%
  ggplot() + 
  geom_col(aes(percentage,Group,fill=status),position = position_stack(),width=1)+
  geom_segment(data=df,aes(x=x,y=y,xend=xend,yend=yend),size=.1)+
  geom_segment(data=df1,aes(x=x,y=y,xend=xend,yend=yend),size=.1)+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_text(vjust=18,size = 12,family="Roboto",face="bold"),
        axis.text.y = element_text(size = 12,family="Roboto",face="bold"),
        axis.title.x = element_text(vjust=16,size=16,family="Roboto",face="bold"),
        plot.margin = unit(c(.5, .25, 0, .25), "cm"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="#dfd3c2",color="#dfd3c2"),
        plot.background = element_rect(fill="#dfd3c2",color="#dfd3c2"),
        plot.title=element_text(size=20,family="Open Sans",hjust=0.5,face="bold"),
        plot.subtitle=element_text(size=16,hjust=0.5,family="Open Sans",face="bold"))+
  scale_fill_manual(values = c("#2e8059","#ca183b","#064da1"))+
  scale_x_continuous(breaks = seq(-100,100,10),labels = limit,expand = c(0,-10)) +
  scale_y_discrete(expand = c(0,1)) +
  coord_cartesian(clip="off")+
  
  # adding axis label on secondary axis
  # https://github.com/tidyverse/ggplot2/issues/3171
  guides(y.sec = guide_axis_label_trans(~paste(.x)))+
  xlab("PER CENTS.")+ylab("") +
  geom_text(aes(x=35,y="15-20",label="SINGLE"),angle = -45,family="Roboto",size=6)+
  geom_text(aes(x=-35,y="15-20",label="SINGLE"),angle = 45,family="Roboto",size=6)+
  geom_text(aes(x=50,y="30-35",label="MARRIED"),angle = -45,family="Roboto",size=6)+
  geom_text(aes(x=-50,y="30-35",label="MARRIED"),angle = 45,family="Roboto",size=6)+
  geom_text(aes(x=90,y="45-55",label="WIDOWED"),angle = -65,family="Roboto",size=6)+
  geom_text(aes(x=-95,y="55-65",label="WIDOWED"),angle = 75,family="Roboto",size=6)+
  geom_text(aes(x=-50,y="Over 65",label="MALES."),vjust=-4,family="Roboto",size=6)+
  geom_text(aes(x=50,y="Over 65",label="FEMALES."),vjust=-4,family="Roboto",size=6)+
  geom_text(aes(x=110,y="Over 65",label="AGES."),size=4,vjust=-1,family="Roboto")+
  geom_text(aes(x=-110,y="Over 65",label="AGES."),size=4,vjust=-1,family="Roboto")+
  labs(title="Conjugal condition of American Negroes according to age periods.")+
  labs(subtitle="Done by Atlanta University.")




ggsave("Dubois_7_final.png",last_plot(), width=3.5,height=4, unit="in")

