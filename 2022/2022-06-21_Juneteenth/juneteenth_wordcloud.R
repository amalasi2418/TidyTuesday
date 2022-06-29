setwd("~/R/Infographics/2022-06-21_Juneteenth")


library(wordcloud2)
library(tm)
library(tidyverse)
library(htmlwidgets)

# install these packages
#install.packages("webshot")
#webshot::install_phantomjs()


african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')


# world map was downloaded and cropped to the proper size. 
# The ocean was filled with black color in Paint (quickest way)

#####################################################

children_names <- african_names %>% filter(gender=="Boy" | gender=="Girl") %>% select(name)

 
names <- children_names
docs <- Corpus(VectorSource(names))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

df$word <- noquote(noquote(substr(df[,1],2,nchar(df[,1])-2)))

df = df %>% arrange(.,desc(freq))

name_cloud <- wordcloud2(data=df, figPath = "world-map4.png", color = "skyblue", size=.5,backgroundColor="black",rotateRatio = 0)

saveWidget(name_cloud,"2.html",selfcontained = F)

webshot2::webshot("2.html","2.png",vwidth = 2000, vheight = 2000, delay =10)
# have to refresh the html couple of times till it shows the wordcloud
# png file does not show anything
# I had to resize the browser to get the appropriate image dimension and then saved it
# Additional text was added in paint for quick turn around. It can also be done in R.