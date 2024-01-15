library(networkD3)
library(htmlwidgets)
library(dplyr)
library(stringr)

#NOTE TO SELF:
#Need to put all the data in common units
#With labels in original units
#And figure out how to minimize crossings.


#Year to show
year<-2045
year_col<-paste0('X', year)

#Prepare the feedstock_to_commodity data
f2c<-read.csv('data/feedstock_to_commodity_currentprojects.csv')
f2c[is.na(f2c)] <- 0
f2c_year<-f2c[,c('Feedstock', 'Commodity', 'Unit.Code', year_col)]
colnames(f2c_year)<-c('Feedstock', 'Commodity', 'Unit.Code', 'Quantity_Produced')


#Prepare the commodity_to_use data
c2u<-read.csv('data/commodity_to_use_currentprojects.csv')
c2u[is.na(c2u)] <- 0
c2u_year<-c2u[,c('Commodity', 'End.Use', year_col)]
colnames(c2u_year)<-c('Commodity', 'End.Use', 'Use')

#Sum up the totals of each commodity
total_commoditiesyear<-f2c_year %>% 
  group_by(Commodity) %>%
  summarise(total_produced = sum(Quantity_Produced))
  
c2u_year<-merge(c2u_year, total_commoditiesyear, by=('Commodity'))
c2u_year$Quantity_Used<-c2u_year$Use*c2u_year$total_produced

#Put the data in Sankey format
links_f2c<-f2c_year[,c('Feedstock', 'Commodity', 'Produced')]
colnames(links_f2c)<-c('source_name', 'target_name', 'value')
links_c2u<-c2u_year[,c('Commodity', 'End.Use', 'Quantity_Used')]
colnames(links_c2u)<-c('source_name', 'target_name', 'value')
links<-rbind(links_f2c, links_c2u)

#Remove any links where value is 0
links<-links[links$value>0, ]

#Replace the source and target with node numbers
nodes<-data.frame(unique(c(links$source_name), links$target_name))
colnames(nodes)<-c('name')
node_mapping<-data.frame(nodes$name, 0:(length(nodes$name)-1))
colnames(node_mapping)<-c('name', 'id')

links<-merge(links, node_mapping, by.x='source_name', by.y='name')
links<-merge(links, node_mapping, by.x='target_name', by.y='name')
colnames(links)<-str_replace_all(colnames(links), 'id.x', 'source')
colnames(links)<-str_replace_all(colnames(links), 'id.y', 'target')



p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name", iterations=32, 
                   units = "TWh", fontSize = 12, nodeWidth = 30, sinksRight = FALSE)
p
#library(htmlwidgets)
#saveWidget(p, file=paste0( getwd(), "sankeySJV.html"))

