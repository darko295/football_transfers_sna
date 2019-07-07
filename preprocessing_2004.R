library(dplyr)
library(stringr)

#loading data
data_04 <- read.csv("data/transfers_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
club_data_04 <- read.csv("data/clubs_data_raw_04.csv", header = T, sep = ",", stringsAsFactors = F)

club_data_04$Division <- c(rep("England",20),rep("Spain",20),rep("Italy",20),rep("France",20),rep("Germany",18))

top5 <- c("England","Spain","Italy","Germany","France")

#splitting permanent and loan moves
loans_data_index <- which(data_04$Amount == "Loan")
loans_data_04 <- data_04[loans_data_index,]
transfers_data_04 <- data_04[-loans_data_index,]

#assign 0 to any observation with the "Free" or empty string values
transfers_data_04$Amount[transfers_data_04$Amount %in% c("Free",""," ")] <- 0

#we are interested in player transfers only, becouse of that any value in Amount column that
#doesn't contain number is excluded
transfers_data_04 <- transfers_data_04[grep('[0-9]', transfers_data_04$Amount),]
transfers_data_04$Amount <- gsub(" ","", transfers_data_04$Amount)

#some values in Amount column are represented in millions, some in hundreds,
#this for loop fixes that with the help of str_count & gsub functions
#also if player is moving/coming to/from a club from same division NAs are occured 
#in DivTo/DivFrom columns, 2 if statements are used to assign correct values
for (i in 1:nrow(transfers_data_04)) {
  amount <- transfers_data_04$Amount[i]
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
  
  transfers_data_04$Amount[i] <- amount
  
}

for (i in 1:nrow(transfers_data_04)) {
  if(is.na(transfers_data_04$DivTo[i])){
    transfers_data_04$DivTo[i] <- transfers_data_04$DivFrom[i]
  }
  if(is.na(transfers_data_04$DivFrom[i])){
    transfers_data_04$DivFrom[i] <- transfers_data_04$DivTo[i]
  }
}

#now we can make column numeric and divide by 1mil
transfers_data_04$Amount <- as.numeric(transfers_data_04$Amount) 
transfers_data_04$Amount <- transfers_data_04$Amount/1000000


#as we are interested only in transfers occured between clubs from top 5 european leagues,
#our transfers df will be filtered and for that we will use club_data df

transfers_data_04 <- transfers_data_04 %>% filter(DivFrom %in% top5 | DivTo %in% top5)

transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Barça"] <- "FC Barcelona"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Barça"] <- "FC Barcelona"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Wolves"] <- "Wolverhampton"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Wolves"] <- "Wolverhampton"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Man City"] <- "Manchester City"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Man City"] <- "Manchester City"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Ath. Bilbao"] <- "Athletic Bilbao"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Ath. Bilbao"] <- "Athletic Bilbao"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Atlético"] <- "Atlético Madrid"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Atlético"] <- "Atlético Madrid"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Bayer"] <- "Bayer Leverkusen"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Bayer"] <- "Bayer Leverkusen"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Bayern"] <- "Bayern Munich"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Bayern"] <- "Bayern Munich"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Brighton"] <- "Brighton & Hove"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Brighton"] <- "Brighton & Hove"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Newcastle"] <- "Newcastle United"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Newcastle"] <- "Newcastle United"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Werder"] <- "Werder Bremen"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Werder"] <- "Werder Bremen"
transfers_data_04$ClubFrom[transfers_data_04$ClubFrom == "Chievo"] <- "Chievo Verona"
transfers_data_04$ClubTo[transfers_data_04$ClubTo == "Chievo"] <- "Chievo Verona"


#test <- transfers_data_04 %>% filter(ClubFrom %in% club_data_04$Club | ClubTo %in% club_data_04$Club)

#to further explore market, countries whose clubs interacted with 4 or more clubs from top 5 leagues
#are added as a whole nation
to_dist <- transfers_data_04 %>% group_by(DivTo) %>% summarise(n = n()) %>% arrange(desc(n))
from_dist <- transfers_data_04 %>% group_by(DivFrom) %>% summarise(n = n()) %>% arrange(desc(n))

to_dist <- to_dist %>% filter(!DivTo %in% top5 & n > 4) 
from_dist <- from_dist %>% filter(!DivFrom %in% top5 & n > 4)
union_add <- union(to_dist$DivTo, from_dist$DivFrom)

#adding those nations to club_list df, those are marked as "Other" in Divison column
for (i in 1:length(union_add)) {
  id <- nrow(club_data_04)+1
  club <- union_add[i]
  div <- "Other"
  temp <- data.frame(ID = id, Club = club, Division = div)
  club_data_04 <- rbind(club_data_04,temp)
}

transfers_data_04 <- transfers_data_04 %>% filter(DivFrom %in% c(top5,union_add) & DivTo %in% c(top5,union_add))
transfers_data_04 <- transfers_data_04 %>% filter(DivFrom %in% c(union_add,top5) & DivTo %in% c(union_add,top5))
brojac <- 0
indeksi <- list()
for (i in 1:nrow(transfers_data_04)) {
  club_from <- transfers_data_04$ClubFrom[i]
  club_to <- transfers_data_04$ClubTo[i]
  if(club_from %in% club_data_04$Club){

    if(transfers_data_04$DivFrom[i] != div){
      div <- club_data_04$Division[club_data_04$Club == club_from]
      brojac <- brojac + 1 
      indeksi[length(indeksi)+1] <- club_from
      transfers_data_04$DivFrom[i] <- div
      }
    
  }else{
    transfers_data_04$ClubFrom[i] <- transfers_data_04$DivFrom[i]
  }
  
  if(club_to %in% club_data_04$Club){
    
    if(transfers_data_04$DivTo[i] != div){
      div <- club_data_04$Division[club_data_04$Club == club_to]
      brojac <- brojac + 1
      transfers_data_04$DivTo[i] <- div
      }
    
  }else{
    transfers_data_04$ClubTo[i] <- transfers_data_04$DivTo[i]
  }
}

#spliting data in 2 parts so we can detect duplicates easier
transfers_data_04_in <- transfers_data_04 %>% filter(InOut == "in")
transfers_data_04_out <- transfers_data_04 %>% filter(InOut == "out")

to_remove <- list()
brojac <- 0
for (i in 1:nrow(transfers_data_04_in)) {
  temp_in <- transfers_data_04_in[i,]
  for (j in 1:nrow(transfers_data_04_out)) {
    temp_out <- transfers_data_04_out[j,]
    
    if(temp_in$Name == temp_out$Name && temp_in$Amount == temp_out$Amount){
      brojac <- brojac + 1
      to_remove[brojac] <- j
    }
  }
}

to_remove <- as.numeric(to_remove)
transfers_data_04_clean <- rbind(transfers_data_04_in,transfers_data_04_out[-to_remove,])

#We can see that every player has one move, except few players that moved twice to different clubs
not_summary <- transfers_data_04_clean %>% group_by(Name) %>% summarise(n = n()) %>% arrange(desc(n))

transfers_data_04_clean <- transfers_data_04_clean %>% filter(!ClubFrom %in% top5)
transfers_data_04_clean <- transfers_data_04_clean %>% filter(!ClubTo %in% top5)

#web scraping is not always reliable, for the last check
#DivFrom & DivTo will be compared with Division col from club_data df
#by using club as reference
bad_indexes <- list()
for (i in 1:nrow(transfers_data_04_clean)) {
  if(transfers_data_04_clean$DivFrom[i] != club_data_04$Division[club_data_04$Club == transfers_data_04_clean$ClubFrom[i]]){
    transfers_data_04_clean$DivFrom[i] <- club_data_04$Division[club_data_04$Club == transfers_data_04_clean$ClubFrom[i]]
    #bad_indexes[length(bad_indexes)+1] <- i
  }
  
  if(transfers_data_04_clean$DivTo[i] != club_data_04$Division[club_data_04$Club == transfers_data_04_clean$ClubTo[i]]){
    transfers_data_04_clean$DivTo[i] <- club_data_04$Division[club_data_04$Club == transfers_data_04_clean$ClubTo[i]]
    #bad_indexes[length(bad_indexes)+1] <- i
  }
}


#Time has come to replace club names for numbers
transfers_data_04_clean$ClubFromID <- NA
transfers_data_04_clean$ClubToID <- NA
for (i in 1:nrow(transfers_data_04_clean)) {
  transfers_data_04_clean$ClubFromID[i] <-  as.character(club_data_04$ID[transfers_data_04_clean$ClubFrom[i] == club_data_04$Club]) 
  transfers_data_04_clean$ClubToID[i] <-  as.character(club_data_04$ID[transfers_data_04_clean$ClubTo[i] == club_data_04$Club])
}

#When for loop is done, there should not be any NA left, let's check
length(which(is.na(transfers_data_04_clean$ClubFromID)))
length(which(is.na(transfers_data_04_clean$ClubToID)))



test <- transfers_data_04_clean[as.integer(bad_indexes),]
test <- test %>% filter(DivFrom %in% top5 & DivTo %in% top5)

earned <- transfers_data_04_clean %>% group_by(ClubFrom) %>% summarise(earned = sum(Amount))
spent <- transfers_data_04_clean %>% group_by(ClubTo) %>% summarise(spent = sum(Amount))

names(earned)[1] <- "Club"
names(spent)[1] <- "Club"

transfer_balance <- merge(earned,spent,all = T)

transfer_balance[is.na(transfer_balance)] <- 0
transfer_balance$profit_loss <- transfer_balance$earned - transfer_balance$spent
#club_data_04 <- club_data_04[!club_data_04$Club %in% setdiff(club_data_04$Club,transfer_balance$Club),]
library(plyr)
club_data_04 <- join(club_data_04,transfer_balance)

write.csv(transfers_data_04_clean,"data/clean_data_04.csv",row.names = F)
write.csv(club_data_04,"data/clubs_data_04.csv",row.names = F)

