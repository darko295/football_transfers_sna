library(dplyr)
library(stringr)

#loading data
data <- read.csv("data/transfers_data_18.csv", header = T, sep = ",", stringsAsFactors = F)
club_data <- read.csv("data/clubs_data_raw_18.csv", header = T, sep = ",", stringsAsFactors = F)
club_data$Division <- c(rep("England",20),rep("Spain",20),rep("Italy",20),rep("France",20),rep("Germany",18))

top5 <- c("England","Spain","Italy","Germany","France")

#splitting permanent and loan moves
loans_data_index <- which(data$Amount == "Loan")
loans_data <- data[loans_data_index,]
transfers_data <- data[-loans_data_index,]

#assign 0 to any observation with the "Free" or empty string values
data$Amount[data$Amount %in% c("Free",""," ")] <- 0

#we are interested in player transfers only, becouse of that any value in Amount column that
#doesn't contain number is excluded
transfers_data <- transfers_data[grep('[0-9]', transfers_data$Amount),]
transfers_data$Amount <- gsub(" ","", transfers_data$Amount)

#some values in Amount column are represented in millions, some in hundreds,
#this for loop fixes that with the help of str_count & gsub functions
#also if player is moving/coming to/from a club from same division NAs are occured 
#in DivTo/DivFrom columns, 2 if statements are used to assign correct values
for (i in 1:nrow(transfers_data)) {
  amount <- transfers_data$Amount[i]
  if(str_count(amount,"M€") > 0){
    amount <- gsub("[M€]","", amount)
    amount <- as.numeric(amount)
    amount <- amount * 1000000
  }else if(str_count(amount,"k") > 0){
    amount <- gsub("[k€]","", amount)
    amount <- as.numeric(amount)
    amount <- amount * 1000
  }else if(amount == "0"){
    amount <- as.numeric(amount)
  }
  
  transfers_data$Amount[i] <- amount
  
}

for (i in 1:nrow(transfers_data)) {
  if(is.na(transfers_data$DivTo[i])){
    transfers_data$DivTo[i] <- transfers_data$DivFrom[i]
  }
  if(is.na(transfers_data$DivFrom[i])){
    transfers_data$DivFrom[i] <- transfers_data$DivTo[i]
  }
}

#now we can make column numeric and divide by 1mil
transfers_data$Amount <- as.numeric(transfers_data$Amount) 
transfers_data$Amount <- transfers_data$Amount/1000000


#as we are interested only in transfers occured between clubs from top 5 european leagues,
#our transfers df will be filtered and for that we will use club_data df

transfers_data <- transfers_data %>% filter(DivFrom %in% top5 | DivTo %in% top5)

transfers_data$ClubFrom[transfers_data$ClubFrom == "Barça"] <- "FC Barcelona"
transfers_data$ClubTo[transfers_data$ClubTo == "Barça"] <- "FC Barcelona"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Wolves"] <- "Wolverhampton"
transfers_data$ClubTo[transfers_data$ClubTo == "Wolves"] <- "Wolverhampton"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Man City"] <- "Manchester City"
transfers_data$ClubTo[transfers_data$ClubTo == "Man City"] <- "Manchester City"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Ath. Bilbao"] <- "Athletic Bilbao"
transfers_data$ClubTo[transfers_data$ClubTo == "Ath. Bilbao"] <- "Athletic Bilbao"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Atlético"] <- "Atlético Madrid"
transfers_data$ClubTo[transfers_data$ClubTo == "Atlético"] <- "Atlético Madrid"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Bayer"] <- "Bayer Leverkusen"
transfers_data$ClubTo[transfers_data$ClubTo == "Bayer"] <- "Bayer Leverkusen"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Bayern"] <- "Bayern Munich"
transfers_data$ClubTo[transfers_data$ClubTo == "Bayern"] <- "Bayern Munich"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Brighton"] <- "Brighton & Hove"
transfers_data$ClubTo[transfers_data$ClubTo == "Brighton"] <- "Brighton & Hove"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Newcastle"] <- "Newcastle United"
transfers_data$ClubTo[transfers_data$ClubTo == "Newcastle"] <- "Newcastle United"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Werder"] <- "Werder Bremen"
transfers_data$ClubTo[transfers_data$ClubTo == "Werder"] <- "Werder Bremen"
transfers_data$ClubFrom[transfers_data$ClubFrom == "Chievo"] <- "Chievo Verona"
transfers_data$ClubTo[transfers_data$ClubTo == "Chievo"] <- "Chievo Verona"


#transfers_data <- transfers_data %>% filter(ClubFrom %in% club_data$Club | ClubTo %in% club_data$Club)

#to further explore market, countries whose clubs interacted with 4 or more clubs from top 5 leagues
#are added as a whole nation
to_dist <- transfers_data %>% group_by(DivTo) %>% summarise(n = n()) %>% arrange(desc(n))
from_dist <- transfers_data %>% group_by(DivFrom) %>% summarise(n = n()) %>% arrange(desc(n))

to_dist <- to_dist %>% filter(!DivTo %in% top5 & n > 4) 
from_dist <- from_dist %>% filter(!DivFrom %in% top5 & n > 4)
union_add <- union(to_dist$DivTo, from_dist$DivFrom)

#adding those nations to club_list df, those are marked as "Other" in Divison column
for (i in 1:length(union_add)) {
  id <- nrow(club_data)+1
  club <- union_add[i]
  div <- "Other"
  temp <- data.frame(ID = id, Club = club, Division = div)
  club_data <- rbind(club_data,temp)
}

transfers_data <- transfers_data %>% filter(DivFrom %in% c(top5,union_add) & DivTo %in% c(top5,union_add))
transfers_data <- transfers_data %>% filter(DivFrom %in% c(union_add,top5) & DivTo %in% c(union_add,top5))
brojac <- 0
indeksi <- list()
for (i in 1:nrow(transfers_data)) {
  club_from <- transfers_data$ClubFrom[i]
  club_to <- transfers_data$ClubTo[i]
  if(club_from %in% club_data$Club){

    if(transfers_data$DivFrom[i] != div){
      div <- club_data$Division[club_data$Club == club_from]
      brojac <- brojac + 1 
      indeksi[length(indeksi)+1] <- club_from
      transfers_data$DivFrom[i] <- div
      }
    
  }else{
    transfers_data$ClubFrom[i] <- transfers_data$DivFrom[i]
  }
  
  if(club_to %in% club_data$Club){
    
    if(transfers_data$DivTo[i] != div){
      div <- club_data$Division[club_data$Club == club_to]
      brojac <- brojac + 1
      transfers_data$DivTo[i] <- div
      }
    
  }else{
    transfers_data$ClubTo[i] <- transfers_data$DivTo[i]
  }
}

#spliting data in 2 parts so we can detect duplicates easier
transfers_data_in <- transfers_data %>% filter(InOut == "in")
transfers_data_out <- transfers_data %>% filter(InOut == "out")

to_remove <- list()
brojac <- 0
for (i in 1:nrow(transfers_data_in)) {
  temp_in <- transfers_data_in[i,]
  for (j in 1:nrow(transfers_data_out)) {
    temp_out <- transfers_data_out[j,]
    
    if(temp_in$Name == temp_out$Name && temp_in$Amount == temp_out$Amount){
      brojac <- brojac + 1
      to_remove[brojac] <- j
    }
  }
}

to_remove <- as.numeric(to_remove)
transfers_data_clean <- rbind(transfers_data_in,transfers_data_out[-to_remove,])

#We can see that every player has one move, except few players that moved twice to different clubs
not_summary <- transfers_data_clean %>% group_by(Name) %>% summarise(n = n()) %>% arrange(desc(n))

transfers_data_clean <- transfers_data_clean %>% filter(!ClubFrom %in% top5)
transfers_data_clean <- transfers_data_clean %>% filter(!ClubTo %in% top5)

#web scraping is not always reliable, for the last check
#DivFrom & DivTo will be compared with Division col from club_data df
#by using club as reference
bad_indexes <- list()
for (i in 1:nrow(transfers_data_clean)) {
  if(transfers_data_clean$DivFrom[i] != club_data$Division[club_data$Club == transfers_data_clean$ClubFrom[i]]){
    transfers_data_clean$DivFrom[i] <- club_data$Division[club_data$Club == transfers_data_clean$ClubFrom[i]]
    #bad_indexes[length(bad_indexes)+1] <- i
  }
  
  if(transfers_data_clean$DivTo[i] != club_data$Division[club_data$Club == transfers_data_clean$ClubTo[i]]){
    transfers_data_clean$DivTo[i] <- club_data$Division[club_data$Club == transfers_data_clean$ClubTo[i]]
    #bad_indexes[length(bad_indexes)+1] <- i
  }
}



#Time has come to replace club names for numbers
transfers_data_clean$ClubFromID <- NA
transfers_data_clean$ClubToID <- NA
for (i in 1:nrow(transfers_data_clean)) {
  transfers_data_clean$ClubFromID[i] <-  as.character(club_data$ID[transfers_data_clean$ClubFrom[i] == club_data$Club]) 
  transfers_data_clean$ClubToID[i] <-  as.character(club_data$ID[transfers_data_clean$ClubTo[i] == club_data$Club])
}

#When for loop is done, there should not be any NA left, let's check
length(which(is.na(transfers_data_clean$ClubFromID)))
length(which(is.na(transfers_data_clean$ClubToID)))



write.csv(transfers_data_clean,"data/clean_data_18.csv",row.names = F)
write.csv(club_data,"data/clubs_data_18.csv",row.names = F)

#data is now preprocessed and saved in csv format
rm(list = ls())


