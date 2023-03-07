setwd("~/Abhinav/tidytuesday/numbats")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(cowplot)
library(magick)
library(ggtext)
library(showtext)

text = "Numans"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)
showtext_auto()

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

oceania <- ne_countries(scale = "large",type="countries",continent = "oceania",returnclass = "sf") %>%
  filter(sovereignt == "Australia")

numbats_sf <- st_as_sf(numbats %>% filter(!is.na(decimalLongitude),!is.na(decimalLatitude)), 
                       coords=c("decimalLongitude","decimalLatitude")) %>% 
  st_set_crs(st_crs(oceania))


inset <- oceania %>% ggplot() + geom_sf(fill=bg, size=1, color="black") +
  geom_sf(data=numbats_sf , aes(color=scientificName)) +
  scale_color_manual(values = c("#698269","#a77646"))+
  coord_sf(xlim = c(110,155), ylim = c(-45,-10))+
  theme_void() +
  theme(plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill=NA, color=NA),
        legend.position = "none")




#unique(numbats_sf$scientificName)

#numbats_sf %>% group_by(year, tmax, scientificName) %>% summarise(count = n())
#numbats_sf %>% group_by(year, tmin, scientificName) %>% summarise(count = n())

#numbats_sf %>% filter(!is.na(year)) %>% group_by(scientificName) %>% summarise(year = max(year))


bg = "#EDF1D6"

#"#a77646"

numbat_text = "Numbats are endangered marsupials native to Australia. Merely less than 1000 
  of them survive  due to habitat loss and introduced predators. Two species of 
  Numbats existed in Australia: <span style = 'color:#698269;'>Myrmecobius fasciatus</span> and <span style = 'color:#a77646;'>Myrmecobius fasciatus rufus</span>, 
  of which rufus is now extinct."

plot <- numbats %>% 
  group_by(scientificName, year) %>% 
  summarise(count = n()) %>%
  ggplot(aes(year, count))+
  geom_point(size=2, shape=21, stroke=2) + geom_line(size=.8) +
  #geom_vline(xintercept = 1960) +
  geom_segment(x=1960,xend=1960,y=0,yend=140,size=1,linetype="dashed")+
  xlab("") + ylab("") +
  coord_cartesian(clip = "off")+
  scale_y_continuous(position = "right", limits = c(0,240),breaks=seq(0,150,by=50),labels=seq(0,150,by=50))+
  scale_x_continuous(labels = seq(1850,2025, by=25),breaks = seq(1850,2025, by=25))+
  geom_textbox(x=1905,y=190,label= numbat_text,width = unit(5, "inch"),fill=bg,box.color=bg,size=15, lineheight=.25,color="black", family=text)+
  geom_textbox(x=1985,y=100,label="Last sighting of\n<span style = 'color:#a77646;'>Myrmecobius fasciatus rufus</span> around 1960s",fill=bg,box.color=bg,size=15, lineheight=.25,color="black", family=text)+
  geom_curve(aes(x = 1985, y = 107, xend = 1960, yend = 130),
             arrow = arrow(length = unit(0.02, "npc")), 
             size = 1, curvature = 0.3, angle = 90)+
  geom_text(x=2035,y=165,label="# of\nsightings",size=15, lineheight=.2,color="black", family=text)+
  geom_text(x=1915,y=230,label="Numbat sightings recorded since 1850",size=40, lineheight=.25,color="black", family=text, fontface = "bold")+
  labs(caption = "Source: Atlas of Living Australia, www.numbat.org.au, www.wikipedia.org | Image: www.wpclipart.com | Graphic: Abhinav Malasi")+
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        panel.grid = element_blank(),
        #axis.text.y.left = element_blank(),
        axis.text.y =  element_text(size=40),
        axis.text.x = element_text(size=40),
        legend.background = element_rect(fill=bg, color=bg),
        legend.key = element_rect(fill=bg, color=bg),
        plot.caption = element_text(size=30,hjust=.5, margin=margin(t=20,b=15)),
        plot.margin = margin(r=20,l=20),
        text = element_text(family = text))


p <- ggdraw(plot) + 
  draw_plot(inset, x = .04, y = .15, width = 0.5, height = 0.5)

#logo_file <- system.file("extdata", "photo.png", package = "cowplot")

logo_file <- image_read("photo.png")

ggdraw() + 
  draw_plot(p) +
  draw_image(logo_file, x = .65, y = .25, width = 0.25)



ggsave2("numbats.png",last_plot(), width = 10, height = 6, dpi=500)

