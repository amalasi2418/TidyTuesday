
library(tidyverse)
library(ambient)
library(marquee)
library(showtext)
sysfonts::font_families_google()

text = "Mouse Memoirs"
#text = "Nova Flat"
sysfonts::font_add_google(text,text)
showtext_auto()

steps = readxl::read_xlsx("steps walked.xlsx") %>%
  janitor::clean_names()

gen_square = function(x1 = 0, y1 = 0, delx = 0, dely = 0, factor = 100){
  square = data.frame(x = c(0,1,1,0), y = c(0,0,1,1)) %>%
    mutate(x = factor * x + delx/2 + x1 ,
           y = factor * y + dely/2 + y1 )
  
  return(square)
}

gen_custom_square = function(x1 = 0, y1 = 0, a = 100){
  a = sqrt(a)
  x1 = x1
  y1 = y1
  cust_square = gen_square(x1 = x1, y1 = y1, delx = 100-a, dely = 100-a, factor = a)
  
  return(cust_square)
}

years = unique(steps$year)
months = unique(steps$month)

df = data.frame()
df1 = data.frame()
yr = 2018

for (i in 1:length(years)) {
  
  for (j in 1:length(months)) {
    k = steps %>%
      filter(year == yr, month == j) %>%
      pull(average_steps) 
    temp = gen_square(x1=i*100, y1=j*100) %>% mutate(id = paste0(i,j))
    temp1 = gen_custom_square(x1=i*100, y1=j*100, a = k) %>% mutate(id = paste0(i,j))
    df = rbind(df,temp)
    df1 = rbind(df1,temp1)
    
  }
  yr = yr+1
}

bg = "#f1f7ed"
txt_col = "#243e36"

plot = df %>%
  ggplot(aes(y,x, group = id)) +
  geom_polygon(size = 1, show.legend = FALSE, color = txt_col, fill = bg) +
  geom_polygon(data = df1, aes(y, x,group = id), size = 1, fill = "#7ca982", show.legend = FALSE) +
  coord_fixed(clip = "off") +
  labs(title = "Average monthly steps between 2018-2024",
       caption = "Data art: Abhinav Malasi") +
  theme_void() +
  annotate(GeomMarquee, 
           label = "Each square represents the monthly target of 10,000 steps, and the filled area indicates the average steps taken during that month. 
           Left to right: months of the year. Top to bottom: years from 2018 to 2024. Data was collected using the iPhone Health app.", 
           x = 1320, 
           y = 955, 
           #style = text_box_style,
           style = marquee::classic_style() %>% modify_style("body", family = text),
           size = 6.5, 
           fill = bg,
           color = txt_col,
           width = 0.91, 
           #family = text,
           hjust = "right",
           vjust = "top"
  )+
  #geom_text(x = 1090, y = 50, 
  #          label = "Each white square represents the monthly target of 10,000 steps, and the filled area indicates the average steps taken during that\nmonth. Left to right: months of the year. Top to bottom: years from 2018 till 2024. Data collected using the health app of iphone.", 
  #          size = 20, color = "white", lineheight = 0.25,
  #          family = text)+
  theme(plot.background = element_rect(fill = bg, color = bg),
        panel.background = element_rect(fill = bg, color = bg),#
        plot.margin = margin(t = 50, b = -50),
        plot.title = element_text(size = 140, hjust = .2, margin = margin(b=-10)),
        plot.caption = element_text(size = 35, lineheight = .25, hjust = 0.93, margin = margin(t = -10)),
        text = element_text(color = txt_col, family = text))

ggsave("steps_dataart9.png", plot, width = 10, height = 10, dpi = 300)  
