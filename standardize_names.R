
library(tidyverse)
library(igraph)


name_matches <- read_csv("Fencing Name Comparisons.csv") %>% 
  select("match","name_key","name_key2","gender","weapon","bthyear")
head(name_matches)

#combine all identifiers into a single column
combine_ids <- function(...){
  str_c(...,sep = "_")
}
name_matches <- name_matches %>% rowwise() %>% mutate(name_key = combine_ids(name_key,gender,weapon,bthyear),
                                                      name_key2 = combine_ids(name_key2,gender,weapon,bthyear)) 

#turn data into a graph
graph <- graph.data.frame(name_matches %>% filter(match == 1) %>% select(name_key,name_key2),directed = F)
dg <- decompose.graph(graph)

#list names of verticies grouped by connected graphs
name_links <- map(dg,function(x){V(x)$name})

#combine into a single dataframe to merge
make_df <- function(list_element){
  vec <- unlist(list_element)
  std_name <- str_split(vec[1],"_",simplify = T)[1]
  data.frame(name_key_combined = vec,std_name = std_name,stringsAsFactors = F)
}
name_standardizations <- do.call(rbind,map(name_links,make_df))

#spread identifiers back into multiple columns
name_standardizations <- name_standardizations %>% separate(name_key_combined,into = c("name_key","gender","weapon","bthyear"),sep = "_")
head(name_standardizations)

#output
write_csv(name_standardizations,path = "name_standardizations.csv")

graph_plot <- graph.data.frame(name_matches[1:1000] %>% filter(match == 1) %>% select(name_key,name_key2),directed = F)
ggnet2(graph_plot,label = F,layout.exp=0,color = "lightskyblue2")

#make header image
set.seed(0515)
png(width = 1280,height = 400,filename = "sample_graph.png")
ggnet2(graph_plot,label = F,layout.exp=0,color = "lightskyblue2")
dev.off()
