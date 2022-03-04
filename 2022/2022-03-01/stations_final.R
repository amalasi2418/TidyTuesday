setwd("~/R/TidyTuesday/2022/2022-03-01")

library(tidyverse)
library(sf)
library(tigris)
library(sysfonts)
library(showtext)
library(ggtext)

showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Junge","Junge")

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

colnames(stations)


stations_tn <- stations %>% filter( STATE=="TN")

stations_tn <- st_as_sf(stations_tn, coords = c("X","Y"),crs = 4326, agr = "constant")


# extracting US states and county data
states <- map_data("state")
counties <- map_data("county")
Tennessee <- subset(states, region == "tennessee")
head(Tennessee)

tn_county <- subset(counties, region == "tennessee")
head(tn_county)


temp_tn_sf <- st_as_sf(Tennessee,coords = c("long","lat"),crs = 4326, agr = "constant")

# extracting interstate data
roads_tn <- primary_secondary_roads(state="Tennessee")
roads_tn_fed <- roads_tn%>%filter(MTFCC=="S1100") %>% filter(RTTYP=="I" | RTTYP=="M")


# summary statistics
stations %>% summarize(a=table(FUEL_TYPE_CODE))
aa <- table(stations$FUEL_TYPE_CODE)
table(temp$FUEL_TYPE_CODE)

# plot

plot=ggplot() + geom_polygon(data = Tennessee, aes(x=long, y = lat),color="gray20") +
  geom_polygon(data = tn_county, aes(x=long, y = lat, group = group),color="gray20",fill="#191A19",size=.3) +
  xlab("")+ylab("")+
  coord_fixed(1.3) + 
  geom_sf(data =roads_tn%>%filter(MTFCC=="S1100"),color="#1E5128")+
  scale_color_manual(values = c("#4E9F3D"))+
  labs(title = "Electric charging stations in Tennessee",
       subtitle = "The map shows the distribution of electric charging stations along <span style = 'color:#1E5128;'>interstates</span> in Tennessee.",
       caption = "Data: US DOT | Graphic: Abhinav Malasi") +
  theme(text = element_text(family="Junge",color="#D8E9A8"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black",color="black"),
        legend.text = element_text(color="white"),
        strip.background = element_rect(fill = "black",color="black"),
        strip.text = element_text(color="white"),
        legend.key = element_rect(fill = "black",color="black"),
        legend.title = element_blank(),
        plot.margin = unit(c(-25,0, -35, 0), "pt"),
        panel.background = element_rect(fill="#191A19",color="#191A19"),
        plot.background = element_rect(fill="#191A19",color="#191A19"),
        plot.title = element_text(size=45,hjust=.5,margin = margin(b=5)),
        plot.subtitle = element_markdown(size=30,hjust=.5),
        plot.caption = element_text(size=15,hjust=.95)
  ) 

plot+geom_point(data = stations_tn%>% filter(FUEL_TYPE_CODE=="ELEC"), aes(x=LONGDD,y=LATDD,color=FUEL_TYPE_CODE), size=.5,alpha=.75,shape=20,show.legend = FALSE) 


ggsave("alternative_fuel_TN_final.png",last_plot(), width=7,height = 2.8,dpi=320)


