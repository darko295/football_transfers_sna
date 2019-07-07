library(visNetwork)
library(igraph)
library(dplyr)
library(ggplot2)
library(DT)
library(reshape2)

source("functions.R")
top5 <- c("England","Spain","Italy","Germany","France")

### TOP 5 LEAGUES GRAPH - 2018

#loading data
df_18 <- read.csv("data/clean_data_18.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_18 <- read.csv("data/clubs_data_18.csv", header = T, sep = ",", stringsAsFactors = F)

#we only need teams from top5 leagues
df_18 <- df_18 %>% filter(DivFrom %in% top5 & DivTo %in% top5)
club_df_18 <- club_df_18 %>% filter(Division %in% top5)

#computing some graph measures
measures <- compute_measures(df_18,club_df_18)
club_df_18 <- join(club_df_18, measures, by="ID")

#nodes param for visNetwork function
nodes <- data.frame(id = club_df_18$ID,
                    value = club_df_18$earned,
                    label = club_df_18$Club,
                    group = club_df_18$Division
)

#edges param for visNetwork function
edges <- data.frame(from = df_18$ClubFromID, 
                    to = df_18$ClubToID,
                    smooth = TRUE,
                    value = df_18$Amount,
                    #width = round((df$Amount - min(df$Amount))/(max(df$Amount)-min(df$Amount))*20+1,0),
                    title = paste0("Player: ",df_18$Name,
                                   "<br>From: ",df_18$ClubFrom,
                                   "<br>To: ",df_18$ClubTo,
                                   "<br>Amount: ",df_18$Amount,"M€")
                    
)

#plotting
visNetwork(nodes,edges,width = "100%", height = "1000px") %>%
  visLegend(useGroups = TRUE) %>%
  visNodes(scaling = list("min" = 10,"max"=50)) %>%
  visEdges(scaling = list("min" = 1,"max"=20),
           arrows = "to") %>%
  visIgraphLayout(layout = "layout_with_dh") %>%
  #visLayout(randomSeed = T) %>%
  visOptions(highlightNearest = T,selectedBy = "group", nodesIdSelection = T) %>%
  visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 150))

#for the purpose of calculating graph metrics igraph package is more suitable
#we will need start and end points for each edge as the first 2 column, additional columns are
#considered as node attributes
igraph_data <- df_18 %>% select(ClubFromID, ClubToID,Name, ClubFrom,ClubTo,Amount)

#creating igraph graph 
graph1 <- graph_from_data_frame(igraph_data,T)

#now we can get in, out & total degree with the help of degree f
in_degree <- degree(graph1, mode = "in")
out_degree <- degree(graph1, mode = "out")
total_degree <- degree(graph1, mode = "total")

in_degree <- as.data.frame(in_degree)
out_degree <- as.data.frame(out_degree)
total_degree <- as.data.frame(total_degree)

in_degree$Club <- row.names(in_degree)
out_degree$Club <- row.names(out_degree)
total_degree$Club <- row.names(total_degree)


##### PLOTS #####

# DEGREE DIST
df_18 <- read.csv("data/clean_data_18.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_18 <- read.csv("data/clubs_data_18.csv", header = T, sep = ",", stringsAsFactors = F)
df_18 <- df_18 %>% filter(DivFrom %in% top5 & DivTo %in% top5)
club_df_18 <- club_df_18 %>% filter(Division %in% top5)

measures_18 <- compute_measures(df_18 ,club_df_18)
club_df_18 <- join(club_df_18, measures_18, by="ID")


df_04 <- read.csv(data/"clean_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_04 <- read.csv("data/clubs_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
df_04 <- df_04 %>% filter(DivFrom %in% top5 & DivTo %in% top5)
club_df_04 <- club_df_04 %>% filter(Division %in% top5)

measures_04 <- compute_measures(df_04, club_df_04)
club_df_04 <- join(club_df_04, measures_04, by="ID")

club_df_04$Year_F <- 2004
club_df_18$Year_F <- 2018

club_df_all <- rbind(club_df_04, club_df_18)


dist <- club_df_all %>% select(total_degree, Year_F)
dist$Year_F <- as.factor(dist$Year_F)

#plot shows that there was more interactions in 2004, but what about money spent?
ggplot(dist, aes(total_degree, fill = Year_F)) + geom_density(alpha = 0.2) 

mean(dist$total_degree[dist$Year_F==2004],na.rm = T)
mean(dist$total_degree[dist$Year_F==2018],na.rm = T)

# MONEY SPENT DIST
spent_dist <- club_df_all %>% select(spent, Year_F)
spent_dist$Year_F <- as.factor(spent_dist$Year_F)

ggplot(spent_dist, aes(spent, fill = Year_F)) + geom_density(alpha = 0.2)

### CALCULATIONS

# median of money spent for 2004
median(spent_dist$spent[spent_dist$Year_F==2004],na.rm = T)
summary(spent_dist$spent[spent_dist$Year_F==2004])

# % of transfers occured between clubs from same Division by year
df <- read.csv("data/td.csv", header = T, sep = ",", stringsAsFactors = F)
df_18 <- df %>% filter(Year_F == 2018 & DivFrom %in% top5 & DivTo %in% top5)
df_04 <- df %>% filter(Year_F == 2004 & DivFrom %in% top5 & DivTo %in% top5)

length(which(df_18$DivFrom == df_18$DivTo))/nrow(df_18)
length(which(df_04$DivFrom == df_04$DivTo))/nrow(df_04)

# % of transfers occured between clubs from same Division by Division
sd_perc_18 <- data.frame()
sd_perc_04 <- data.frame()
for (i in 1:length(top5)) {
  df_18 <- df %>% filter(Year_F == 2018 & (DivFrom %in% top5[i] | DivTo %in% top5[i]))
  df_04 <- df %>% filter(Year_F == 2004 & (DivFrom %in% top5[i] | DivTo %in% top5[i]))
  
  same_div_18 <- length(which(df_18$DivFrom == df_18$DivTo))
  same_div_04 <- length(which(df_04$DivFrom == df_04$DivTo))
  
  total_18 <- nrow(df_18)
  total_04 <- nrow(df_04)
  
  perc_18 <- round(same_div_18/total_18*100,2)
  perc_04 <- round(same_div_04/total_04*100,2)
  temp <- data.frame("Division"=top5[i], "Total"=total_18,"From same division"=same_div_18,"Percent"=perc_18)
  sd_perc_18 <- rbind(sd_perc_18,temp)
  temp <- data.frame("Division"=top5[i], "Total"=total_04,"From same division"=same_div_04,"Percent"=perc_04)
  sd_perc_04 <- rbind(sd_perc_04,temp)
}

#2004
datatable(sd_perc_04,colnames = c("Number of transfers","From same division","Percentage"),
          rownames = F, options = list(autoWidth = F, searching = FALSE, scrollX = F,
                                       bPaginate= FALSE, dom = 'Bt'))
#2018
datatable(sd_perc_18,colnames = c("Number of transfers","From same division","Percentage"),
          rownames = F, options = list(autoWidth = F, searching = FALSE, scrollX = F,
                                       bPaginate= FALSE, dom = 'Bt'))


######## CHECKING NAT FREQUENCIES
df_18_nat <- df %>% filter(Year_F == 2018) %>% group_by(Nat) %>% summarise(count=n()) %>% arrange(desc(count))
df_04_nat <- df %>% filter(Year_F == 2004) %>% group_by(Nat) %>% summarise(count=n()) %>% arrange(desc(count))


df <- read.csv("data/td.csv", header = T, sep = ",", stringsAsFactors = F)
club_df <- read.csv("data/cd.csv", header = T, sep = ",", stringsAsFactors = F)

eng_2004 <- compute_graph_info_by_div_year(df,"England")
spa_2004 <- compute_graph_info_by_div_year(df,"Spain")
ita_2004 <- compute_graph_info_by_div_year(df,"Italy")
fra_2004 <- compute_graph_info_by_div_year(df,"France")
ger_2004 <- compute_graph_info_by_div_year(df,"Germany")
complete_2004 <- bind_rows(eng_2004, spa_2004, ita_2004,fra_2004,ger_2004)

eng_2018 <- compute_graph_info_by_div_year(df,"England",2018)
spa_2018 <- compute_graph_info_by_div_year(df,"Spain",2018)
ita_2018 <- compute_graph_info_by_div_year(df,"Italy",2018)
fra_2018 <- compute_graph_info_by_div_year(df,"France",2018)
ger_2018 <- compute_graph_info_by_div_year(df,"Germany",2018)
complete_2018 <- bind_rows(eng_2018, spa_2018, ita_2018,fra_2018,ger_2018)






### divisions with most interactions
df <- df %>% filter(DivFrom %in% top5 & DivTo %in% top5)
df$Comb <- paste0(df$DivFrom,"-",df$DivTo)
comb_dist_04 <- df %>% filter(Year_F == 2004) %>% group_by(Comb) %>% dplyr::summarise(count = n())
comb_dist_18 <- df %>% filter(Year_F == 2018) %>% group_by(Comb) %>% dplyr::summarise(count = n())


comb_dist_04 <- compute_interactions(df %>% filter(Year_F == 2004))
colnames(comb_dist_04) <- "Num. of tr. 2004"
comb_dist_04$comb <- row.names(comb_dist_04)
row.names(comb_dist_04) <- NULL
comb_dist_18 <- compute_interactions(df %>% filter(Year_F == 2018))
colnames(comb_dist_18) <- "Num. of tr. 2018"
comb_dist_18$comb <- row.names(comb_dist_18)
row.names(comb_dist_18) <- NULL

comb_dist <- merge(comb_dist_04,comb_dist_18,by="comb")

comb_dist$comb <- gsub("eng","England",comb_dist$comb)
comb_dist$comb <- gsub("spa","Spain",comb_dist$comb)
comb_dist$comb <- gsub("ita","Italy",comb_dist$comb)
comb_dist$comb <- gsub("ger","Germany",comb_dist$comb)
comb_dist$comb <- gsub("fra","France",comb_dist$comb)
comb_dist$comb <- gsub("_","-",comb_dist$comb)





#INTERACTIONS BETWEEN DIVISIONS BY YEAR

#2018
club_df_18 <- club_df %>% filter(Year_F == 2018 & Division %in% top5)
df_18 <- df %>% filter(Year_F == 2018 & DivFrom %in% top5 & DivTo %in% top5) %>%
  select(ClubFromID,ClubToID,DivFrom,DivTo)


club_df_18 <- club_df_18 %>% filter(ID %in% df_18$ClubFromID | ID %in% df_18$ClubToID)

df_18_dist <- df_18 %>% group_by(DivFrom,DivTo) %>% dplyr::summarise(degree = n())


df_18_dist$DivFromID = mapvalues(df_18_dist$DivFrom,from = c("England","Spain","Italy","Germany","France"),c(1:5))
df_18_dist$DivToID = mapvalues(df_18_dist$DivTo,from = c("England","Spain","Italy","Germany","France"),c(1:5))



nodes <- data.frame(id = 1:5,
                    label = top5,
                    group = top5,
                    title = top5
                    
)

edges <- data.frame(from = df_18_dist$DivFromID, 
                    to = df_18_dist$DivToID,
                    value = df_18_dist$degree,
                    label = df_18_dist$degree,
                    font.size = c(20),
                    smooth = TRUE
)

#2004
club_df_04 <- club_df %>% filter(Year_F == 2004 & Division %in% top5)
df_04 <- df %>% filter(Year_F == 2004 & DivFrom %in% top5 & DivTo %in% top5) %>%
  select(ClubFromID,ClubToID,DivFrom,DivTo)


club_df_04 <- club_df_04 %>% filter(ID %in% df_04$ClubFromID | ID %in% df_04$ClubToID)

df_04_dist <- df_04 %>% group_by(DivFrom,DivTo) %>% dplyr::summarise(degree = n())


df_04_dist$DivFromID = mapvalues(df_04_dist$DivFrom,from = c("England","Spain","Italy","Germany","France"),c(1:5))
df_04_dist$DivToID = mapvalues(df_04_dist$DivTo,from = c("England","Spain","Italy","Germany","France"),c(1:5))



nodes <- data.frame(id = 1:5,
                    label = top5,
                    group = top5,
                    title = top5
                    
)

edges <- data.frame(from = df_04_dist$DivFromID, 
                    to = df_04_dist$DivToID,
                    value = df_04_dist$degree,
                    label = df_04_dist$degree,
                    font.size = c(20),
                    smooth = TRUE
)



visNetwork(nodes,edges) %>%
  visLegend(width = 0.07, position = "right",zoom = F) %>%
  visEdges(scaling = list("min" = 1,"max"=8),
           arrows = "to",arrowStrikethrough = F) %>%
  #visLayout(randomSeed = T) %>%
visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(highlightNearest = T,selectedBy = "group", nodesIdSelection = T) %>%
  visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 150))

#GRAPH WITHOUT MONACO, ROMA & LEICESTER
df_18 <- df %>% filter(Year_F == 2018 & DivFrom %in% top5 & DivTo %in% top5 & !ClubFrom %in% c("Monaco","AS Roma","Leicester") & !ClubTo %in% c("Monaco","AS Roma","Leicester")) %>%
  select(ClubFromID,ClubToID)
g <- graph_from_data_frame(df_18)

ncomp_dir <- count_components(g,mode = "strong")

### COLOR GRADIENT NODES BASED ON OUT DEGREE
### NODE SIZE BASED ON EARNED MONEY
### 2004 ###

df <- read.csv("data/td.csv", header = T, sep = ",", stringsAsFactors = F)
club_df <- read.csv("data/cd.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_04 <- club_df %>% filter(Year_F == 2004)
df_04 <- df %>% filter(Year_F == 2004)

measures <- compute_measures(df_04,club_df_04)
club_df_04 <- join(club_df_04, measures, by="ID")
club_df_04 <- club_df_04[-which(is.na(club_df_04$profit_loss)),]

club_df_04 <- club_df_04 %>% filter(Year_F == 2004 & Division %in% top5)
df_04 <- df_04 %>% filter(Year_F == 2004 & DivFrom %in% top5 & DivTo %in% top5)


row.names(club_df_04) <- NULL
g1 <- graph_from_data_frame(df_04 %>% select(ClubFromID,ClubToID,Amount),vertices = club_df_04$ID)
E(g1)$amount <- df_04$Amount
V(g1)$earned <- club_df_04$earned
V(g1)$spent <- club_df_04$spent
V(g1)$tot_activity <- club_df_04$earned + club_df_04$spent
V(g1)$profit_loss <- club_df_04$profit_loss
V(g1)$out_degree <- club_df_04$out_degree
V(g1)$in_degree <- club_df_04$in_degree
V(g1)$total_degree <- club_df_04$total_degree

sp_col_palette = attr_based_color_gradient(V(g1)$tot_activity , 
                                           c('yellow','red')) 


V(g1)$color <- sp_col_palette
V(g1)$size <- V(g1)$total_degree*5
V(g1)$title <- club_df_04$Club
V(g1)$name <- club_df_04$Club


visIgraph(g1) %>% 
  visNodes(title = club_df_04$Club,label = club_df_04$Club)

### 2018 ###
club_df_18 <- club_df %>% filter(Year_F == 2018)
df_18 <- df %>% filter(Year_F == 2018)

measures <- compute_measures(df_18,club_df_18)
club_df_18 <- join(club_df_18, measures, by="ID")
club_df_18 <- club_df_18[-which(is.na(club_df_18$profit_loss)),]

club_df_18 <- club_df_18 %>% filter(Division %in% top5)
df_18 <- df_18 %>% filter(DivFrom %in% top5 & DivTo %in% top5)

row.names(club_df_18) <- NULL
g2 <- graph_from_data_frame(df_18 %>% select(ClubFromID,ClubToID,Amount),vertices = club_df_18$ID)
E(g2)$amount <- df_18$Amount
V(g2)$earned <- club_df_18$earned
V(g2)$spent <- club_df_18$spent
V(g2)$tot_activity <- club_df_18$earned + club_df_18$spent
V(g2)$profit_loss <- club_df_18$profit_loss
V(g2)$out_degree <- club_df_18$out_degree
V(g2)$in_degree <- club_df_18$in_degree
V(g2)$total_degree <- club_df_18$total_degree
sp_col_palette = attr_based_color_gradient(V(g2)$tot_activity, 
                                           c('yellow','red')) 


V(g2)$color <- sp_col_palette
V(g2)$size <- V(g2)$total_degree*5
V(g2)$title <- club_df_18$Club
V(g2)$name <- club_df_18$Club

visIgraph(g2) %>% 
  visNodes(title = club_df_18$Club,label = club_df_18$Club)

########### CHELSEA, GLADBACH & MAN UTD 2004 #############
datatable(club_df_04 %>% filter(Club %in% c("Chelsea","M'gladbach","Man United")) %>%
              select(-ID, -Year_F, -closeness,-betweenness), rownames = F,
          options = list(autoWidth = TRUE, searching = FALSE, scrollX = F, bPaginate= FALSE,
                         dom = 'Bt', buttons = list(
                           list(
                             extend = 'excel',
                             text = '.XLSX',
                             filename = 'Selected club data'
                           )
                         )))
######################################

########### MONACO, PSG & CHELSEA 2018 #############
datatable(club_df_18 %>% filter(Club %in% c("Chelsea","Monaco","Paris S-G")) %>%
              select(-ID, -Year_F, -closeness,-betweenness), caption = "Last selected club info", rownames = F,
          options = list(autoWidth = TRUE, searching = FALSE, scrollX = F, bPaginate= FALSE,
                         dom = 'Bt', buttons = list(
                           list(
                             extend = 'excel',
                             text = '.XLSX',
                             filename = 'Selected club data'
                           )
                         )))
######################################

### BETWEENNESS vs TOTAL DEGREE - ITALY & FRANCE
club_df_04 <- club_df %>% filter(Year_F == 2004 & Division %in% c("Italy","France"))
df_04 <- df %>% filter(Year_F == 2004 & DivFrom %in% c("Italy","France") & DivTo %in% c("Italy","France"))

measures <- compute_measures(df_04,club_df_04)
club_df_04 <- join(club_df_04, measures, by="ID")


row.names(club_df_04) <- NULL
g1 <- graph_from_data_frame(df_04 %>% select(ClubFromID,ClubToID,Amount),vertices = club_df_04$ID)
E(g1)$amount <- df_04$Amount
V(g1)$earned <- club_df_04$earned
V(g1)$spent <- club_df_04$spent
V(g1)$tot_activity <- club_df_04$earned + club_df_04$spent
V(g1)$profit_loss <- club_df_04$profit_loss
V(g1)$out_degree <- club_df_04$out_degree
V(g1)$in_degree <- club_df_04$in_degree
V(g1)$total_degree <- club_df_04$total_degree
V(g1)$betweenness <- club_df_04$betweenness

sp_col_palette = attr_based_color_gradient(V(g1)$betweenness , 
                                           c('yellow','red','purple')) 


V(g1)$color <- sp_col_palette
V(g1)$size <- V(g1)$total_degree*5
V(g1)$title <- club_df_04$Club
V(g1)$name <- club_df_04$Club

visIgraph(g1) %>% 
  visNodes(title = club_df_04$Club,label = club_df_04$Club)

### BETWEENNESS vs TOTAL DEGREE - TOP 5
club_df_04 <- club_df %>% filter(Year_F == 2004 & Division %in% top5)
df_04 <- df %>% filter(Year_F == 2004 & DivFrom %in% top5 & DivTo %in% top5)

measures <- compute_measures(df_04,club_df_04)
club_df_04 <- join(club_df_04, measures, by="ID")


row.names(club_df_04) <- NULL
g1 <- graph_from_data_frame(df_04 %>% select(ClubFromID,ClubToID,Amount),vertices = club_df_04$ID)
E(g1)$amount <- df_04$Amount
V(g1)$earned <- club_df_04$earned
V(g1)$spent <- club_df_04$spent
V(g1)$tot_activity <- club_df_04$earned + club_df_04$spent
V(g1)$profit_loss <- club_df_04$profit_loss
V(g1)$out_degree <- club_df_04$out_degree
V(g1)$in_degree <- club_df_04$in_degree
V(g1)$total_degree <- club_df_04$total_degree
V(g1)$betweenness <- club_df_04$betweenness
V(g1)$domestic <- club_df_04$Domestic

sp_col_palette = attr_based_color_gradient(V(g1)$domestic , 
                                           c('yellow','red','purple')) 


V(g1)$color <- sp_col_palette
V(g1)$size <- V(g1)$total_degree*5
V(g1)$title <- club_df_04$Club
V(g1)$name <- club_df_04$Club
V(g1)$group <- club_df_04$Division

visIgraph(g1) %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 0.5),selectedBy = "group")

  
#GRAPH WITH "OTHER" LEAGUES INCLUDED - TRANSFERS *FROM* OTHER LEAGUES
df_04 <- read.csv("data/clean_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_04 <- read.csv("data/clubs_data_04.csv", header = T, sep = ",", stringsAsFactors = F)

df_04 <- df_04 %>% filter(DivFrom == "Other")

measures <- compute_measures(df_04,club_df_04)
club_df_04 <- join(club_df_04, measures, by="ID")

nodes <- data.frame(id = club_df_04$ID,
                    value = club_df_04$earned,
                    label = club_df_04$Club,
                    group = club_df_04$Division,
                    title = club_df_04$Club
)

edges <- data.frame(from = df_04$ClubFromID, 
                    to = df_04$ClubToID,
                    smooth = TRUE,
                    value = df_04$Amount,
                    #width = round((df$Amount - min(df$Amount))/(max(df$Amount)-min(df$Amount))*20+1,0),
                    title = paste0("Player: ",df_04$Name,
                                   "<br>From: ",df_04$ClubFrom,
                                   "<br>To: ",df_04$ClubTo,
                                   "<br>Amount: ",df_04$Amount,"M€")
                    
)

visNetwork(nodes,edges) %>%
  visGroups(groupname = "England", color = "#97c2fc") %>% #c("#97c2fc","#ffff00","#fb7e81","#eb7df4","#7be141")
  visGroups(groupname = "Spain", color = "#ffff00") %>%
  visGroups(groupname = "Italy", color = "#fb7e81") %>%
  visGroups(groupname = "Germany", color = "#eb7df4") %>%
  visGroups(groupname = "France", color = "#7be141") %>%
  visGroups(groupname = "Other", color = "#000000") %>%
  visLegend(width = 0.07, position = "right",zoom = F) %>%
  visNodes(scaling = list("min" = 10,"max"=50)) %>%
  visEdges(scaling = list("min" = 1,"max"=20),
           arrows = "to",arrowStrikethrough = F) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = T,selectedBy = "group", nodesIdSelection = T) %>%
  visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 110))

### some metrics will be calculated for each combination of top5 league & OTHER
oth_m_04 <- data.frame()
for (i in 1:length(top5)) {
  df_f <- df_04 %>% filter(DivTo == top5[i]) %>% select(ClubFromID,ClubToID,ClubFrom,Amount)
  club_df_f <- club_df_04 %>% filter(Division %in% c("Other",top5[i]))
  g <- graph_from_data_frame(df_f)
  
  nodes_n <- vcount(g)
  edges_n <- ecount(g)
  density <- round(edge_density(g),2)
  links_per_node <- round(edges_n/nodes_n,2)
  move <- paste0("Other-",top5[i])
  
  temp <- data.frame("Division" = move,
                      "Num of nodes" = nodes_n - length(unique(df_f$ClubFrom)),
                      "Num of edges" = edges_n,
                      "Density" = density,
                      "Links per node" = links_per_node,
                      "Total earned" = sum(df_f$Amount),
                      "Average earned" = round(mean(df_f$Amount),2)
                      )
  
  oth_m_04 <- rbind(oth_m_04,temp)
}  


### SAME FOR 2018
df_18 <- read.csv("data/clean_data_18.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_18 <- read.csv("data/clubs_data_18.csv", header = T, sep = ",", stringsAsFactors = F)

df_18 <- df_18 %>% filter(DivFrom == "Other")

measures <- compute_measures(df_18,club_df_18)
club_df_18 <- join(club_df_18, measures, by="ID")

mean(df_18$Amount)

nodes <- data.frame(id = club_df_18$ID,
                    value = club_df_18$earned,
                    label = club_df_18$Club,
                    group = club_df_18$Division,
                    title = club_df_18$Club
)

edges <- data.frame(from = df_18$ClubFromID, 
                    to = df_18$ClubToID,
                    smooth = TRUE,
                    value = df_18$Amount,
                    #width = round((df$Amount - min(df$Amount))/(max(df$Amount)-min(df$Amount))*20+1,0),
                    title = paste0("Player: ",df_18$Name,
                                   "<br>From: ",df_18$ClubFrom,
                                   "<br>To: ",df_18$ClubTo,
                                   "<br>Amount: ",df_18$Amount,"M€")
                    
)

visNetwork(nodes,edges) %>%
  visGroups(groupname = "England", color = "#97c2fc") %>% #c("#97c2fc","#ffff00","#fb7e81","#eb7df4","#7be141")
  visGroups(groupname = "Spain", color = "#ffff00") %>%
  visGroups(groupname = "Italy", color = "#fb7e81") %>%
  visGroups(groupname = "Germany", color = "#eb7df4") %>%
  visGroups(groupname = "France", color = "#7be141") %>%
  visGroups(groupname = "Other", color = "#000000") %>%
  visLegend(width = 0.07, position = "right",zoom = F) %>%
  visNodes(scaling = list("min" = 10,"max"=50)) %>%
  visEdges(scaling = list("min" = 1,"max"=20),
           arrows = "to",arrowStrikethrough = F) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = list(enabled = T, degree = list(from = 1, to = 1)),selectedBy = "group", nodesIdSelection = T) %>%
  visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 110))

### some metrics will be calculated for each combination of top5 league & OTHER
oth_m_18 <- data.frame()
for (i in 1:length(top5)) {
  df_f <- df_18 %>% filter(DivTo == top5[i]) %>% select(ClubFromID,ClubToID,ClubFrom,Amount)
  club_df_f <- club_df_18 %>% filter(Division %in% c("Other",top5[i]))
  g <- graph_from_data_frame(df_f)
  
  nodes_n <- vcount(g)
  edges_n <- ecount(g)
  density <- round(edge_density(g),2)
  links_per_node <- round(edges_n/nodes_n,2)
  move <- paste0("Other-",top5[i])
  
  temp <- data.frame("Division" = move,
                     "Num of nodes" = nodes_n - length(unique(df_f$ClubFrom)),
                     "Num of edges" = edges_n,
                     "Density" = density,
                     "Links per node" = links_per_node,
                     "Total earned" = sum(df_f$Amount),
                     "Average earned" = round(mean(df_f$Amount),2)
  )
  
  oth_m_18 <- rbind(oth_m_18,temp)
}

#earned by league (other)
earned <- df_18 %>% group_by(ClubFrom,DivTo) %>% dplyr::summarise(spent = sum(Amount)) %>%
  arrange(desc(spent))

casted <- dcast(spent, DivTo~ClubFrom)
casted[is.na(casted)] <- 0

#GRAPH WITH "OTHER" LEAGUES INCLUDED - TRANSFERS *TO* OTHER LEAGUES
df_04 <- read.csv("data/clean_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_04 <- read.csv("data/clubs_data_04.csv", header = T, sep = ",", stringsAsFactors = F)

df_04 <- df_04 %>% filter(DivTo == "Other")



measures <- compute_measures(df_04,club_df_04)
club_df_04 <- join(club_df_04, measures, by="ID")

nodes <- data.frame(id = club_df_04$ID,
                    value = club_df_04$spent,
                    label = club_df_04$Club,
                    group = club_df_04$Division,
                    title = club_df_04$Club
)

edges <- data.frame(from = df_04$ClubFromID, 
                    to = df_04$ClubToID,
                    smooth = TRUE,
                    value = df_04$Amount,
                    #width = round((df$Amount - min(df$Amount))/(max(df$Amount)-min(df$Amount))*20+1,0),
                    title = paste0("Player: ",df_04$Name,
                                   "<br>From: ",df_04$ClubFrom,
                                   "<br>To: ",df_04$ClubTo,
                                   "<br>Amount: ",df_04$Amount,"M€")
                    
)

visNetwork(nodes,edges) %>%
  visGroups(groupname = "England", color = "#97c2fc") %>%
  visGroups(groupname = "Spain", color = "#ffff00") %>%
  visGroups(groupname = "Italy", color = "#fb7e81") %>%
  visGroups(groupname = "Germany", color = "#eb7df4") %>%
  visGroups(groupname = "France", color = "#7be141") %>%
  visGroups(groupname = "Other", color = "#000000") %>%
  visLegend(width = 0.07, position = "right",zoom = F) %>%
  visNodes(scaling = list("min" = 10,"max"=50)) %>%
  visEdges(scaling = list("min" = 1,"max"=20),
           arrows = "to",arrowStrikethrough = F) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = T,selectedBy = "group", nodesIdSelection = T) %>%
  visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 110))

### some metrics will be calculated for each combination of top5 league & OTHER
oth_m_04 <- data.frame()
for (i in 1:length(top5)) {
  df_f <- df_04 %>% filter(DivFrom == top5[i]) %>% select(ClubFromID,ClubToID,ClubTo,Amount)
  club_df_f <- club_df_04 %>% filter(Division %in% c("Other",top5[i]))
  g <- graph_from_data_frame(df_f)
  
  nodes_n <- vcount(g)
  edges_n <- ecount(g)
  density <- round(edge_density(g),2)
  links_per_node <- round(edges_n/nodes_n,2)
  move <- paste0(top5[i],"-Other")
  
  temp <- data.frame("Division" = move,
                      "Num of nodes" = nodes_n - length(unique(df_f$ClubTo)),
                      "Num of edges" = edges_n,
                      "Density" = density,
                      "Links per node" = links_per_node,
                      "Total spent" = sum(df_f$Amount),
                      "Average spent" = round(mean(df_f$Amount),2)
                      )
  
  oth_m_04 <- rbind(oth_m_04,temp)
}  
spent <- df_04 %>% group_by(ClubTo,DivFrom) %>% dplyr::summarise(spent = sum(Amount)) %>%
  arrange(desc(spent))

casted <- dcast(spent, DivFrom~ClubTo)
casted[is.na(casted)] <- 0



### SAME FOR 2018
df_18 <- read.csv("data/clean_data_18.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_18 <- read.csv("data/clubs_data_18.csv", header = T, sep = ",", stringsAsFactors = F)

df_18 <- df_18 %>% filter(DivTo == "Other")

measures <- compute_measures(df_18,club_df_18)
club_df_18 <- join(club_df_18, measures, by="ID")

mean(df_18$Amount)

nodes <- data.frame(id = club_df_18$ID,
                    value = club_df_18$spent,
                    label = club_df_18$Club,
                    group = club_df_18$Division,
                    title = club_df_18$Club
)

edges <- data.frame(from = df_18$ClubFromID, 
                    to = df_18$ClubToID,
                    smooth = TRUE,
                    value = df_18$Amount,
                    #width = round((df$Amount - min(df$Amount))/(max(df$Amount)-min(df$Amount))*20+1,0),
                    title = paste0("Player: ",df_18$Name,
                                   "<br>From: ",df_18$ClubFrom,
                                   "<br>To: ",df_18$ClubTo,
                                   "<br>Amount: ",df_18$Amount,"M€")
                    
)

visNetwork(nodes,edges) %>%
  visGroups(groupname = "England", color = "#97c2fc") %>% #c("#97c2fc","#ffff00","#fb7e81","#eb7df4","#7be141")
  visGroups(groupname = "Spain", color = "#ffff00") %>%
  visGroups(groupname = "Italy", color = "#fb7e81") %>%
  visGroups(groupname = "Germany", color = "#eb7df4") %>%
  visGroups(groupname = "France", color = "#7be141") %>%
  visGroups(groupname = "Other", color = "#000000") %>%
  visLegend(width = 0.07, position = "right",zoom = F) %>%
  visNodes(scaling = list("min" = 10,"max"=50)) %>%
  visEdges(scaling = list("min" = 1,"max"=20),
           arrows = "to",arrowStrikethrough = F) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(highlightNearest = list(enabled = T, degree = list(from = 1, to = 1)),selectedBy = "group", nodesIdSelection = T) %>%
  visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 110))

### some metrics will be calculated for each combination of top5 league & OTHER
oth_m_18 <- data.frame()
for (i in 1:length(top5)) {
  df_f <- df_18 %>% filter(DivFrom == top5[i]) %>% select(ClubFromID,ClubToID,ClubTo,Amount)
  club_df_f <- club_df_18 %>% filter(Division %in% c("Other",top5[i]))
  g <- graph_from_data_frame(df_f)
  
  
  nodes_n <- vcount(g)
  edges_n <- ecount(g)
  density <- round(edge_density(g),2)
  links_per_node <- round(edges_n/nodes_n,2)
  move <- paste0(top5[i],"-Other")
  
  temp <- data.frame("Division" = move,
                     "Num of nodes" = nodes_n - length(unique(df_f$ClubTo)),
                     "Num of edges" = edges_n,
                     "Density" = density,
                     "Links per node" = links_per_node,
                     "Total spent" = sum(df_f$Amount),
                     "Average spent" = round(mean(df_f$Amount),2)
  )
  
  oth_m_18 <- rbind(oth_m_18,temp)
}  

#spent by league (other)
spent <- df_18 %>% group_by(ClubTo,DivFrom) %>% dplyr::summarise(spent = sum(Amount)) %>%
  arrange(desc(spent))

casted <- dcast(spent, DivFrom~ClubTo)
casted[is.na(casted)] <- 0
