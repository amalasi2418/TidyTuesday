setwd("~/Abhinav/tidytuesday/lego")

library(tidyverse)
library(packcircles)
library(showtext)

showtext_auto()

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)



inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')

elements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')

themes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')

parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')


total = merge(inventories,inventory_parts,by.x = "id",by.y = "inventory_id")

total1 <- merge(total,colors,by.x="color_id",by.y = "id")

df_final <- merge(total1, sets,by="set_num")



#################################################################
# merge datasets with color coding, year, and number of individual parts

df_colors <- merge(df_final, parts, by="part_num")

df_colors_subset <- df_colors %>% select(name.x, rgb, year,quantity) %>% 
  group_by(year,rgb,name.x) %>% 
  summarise(total = sum(quantity)) 

df_colros_total <- df_colors_subset %>% group_by(year) %>%
  summarise(total_year = sum(total))

df = merge(df_colors_subset,df_colros_total,by="year")

a=df %>% filter(!(rgb=="F3C305")) %>% mutate(percent = total/total_year, rgb = paste0("#",rgb), year = case_when(year<1952~year+1,
                                                                                                                 TRUE~year))




# circle packing from r-graph-gallery.com


df_pack <- a %>% group_by(rgb, name.x) %>% summarise(count = sum(total))

packing <- circleProgressiveLayout(df_pack$count,sizetype = "area")

df_pack1 <- cbind(df_pack,packing)

gg_layout <- circleLayoutVertices(packing,npoints = 50)


bg = "gray85"

ggplot()+
  geom_polygon(data=gg_layout,aes(x,y,group=id, fill=as.factor(id)))+
  labs(title = "The Colors of LEGO",
       subtitle = "Lego has used 202 unique colors. The size of the circle depicts the number of parts.",
       caption = "Data: Rebrickable | Graphic: Abhinav Malasi")+
  theme_void()+
  coord_equal()+
  scale_fill_manual(values = df_pack$rgb)+
  theme(legend.position = "none",
        plot.title = element_text(size=80,face = "bold"),
        plot.subtitle = element_text(size=40, lineheight = .35,margin = margin(t=15)),
        plot.caption = element_text(size=30),
        plot.margin = margin(t=20,b=10),
        plot.background = element_rect(color=bg,fill=bg),
        panel.background = element_rect(color=bg,fill=bg),
        text = element_text(family = text))

ggsave("lego.png",last_plot(),width = 10,height=10, units = "in")
