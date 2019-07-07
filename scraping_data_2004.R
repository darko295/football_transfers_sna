library(rvest)
library(dplyr)

data <- data.frame()
club_list <- list()
club_df <- data.frame()

#PL
url <- "https://www.footballdatabase.eu/en/competition/transfers/56-premier_league/2004-2005"
page <- read_html(url)
clubs <- page %>% html_nodes(".transferstab > .line")
div_to <- "England"
for (i in 1:length(clubs)) {
  club <- clubs[i]  %>% html_node(".clubtab > h3 > a") %>% html_text(trim = T)
  print(club)
  club_list[length(club_list)+1] <- club
  transfers_in <- clubs[i] %>% html_node(".transfers1") %>% html_nodes(".line")
  transfers_out <- clubs[i] %>% html_node(".transfers2") %>% html_nodes(".line")
  for (j in 1:length(transfers_in)) {
    tr_date <- transfers_in[j] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_in[j] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_in[j] %>% html_node(".player > a") %>% html_text(trim = T)
    club_from <- transfers_in[j] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_from_country <- transfers_in[j] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_in[j] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "in"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club_from, DivFrom = club_from_country, ClubTo = club,
                       DivTo = div_to, Amount = amount)
    data <- rbind(data,temp)
  }
  for (k in 1:length(transfers_out)) {
    tr_date <- transfers_out[k] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_out[k] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_out[k] %>% html_node(".player > a") %>% html_text(trim = T)
    club_to <- transfers_out[k] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_to_country <- transfers_out[k] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_out[k] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "out"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club, DivFrom = div_to, ClubTo = club_to,
                       DivTo = club_to_country, Amount = amount)
    data <- rbind(data,temp)
  }
}

#LA LIGA
url <- "https://www.footballdatabase.eu/en/competition/transfers/58-primera_division/2004-2005"
page <- read_html(url)
clubs <- page %>% html_nodes(".transferstab > .line")
div_to <- "Spain"
for (i in 1:length(clubs)) {
  club <- clubs[i]  %>% html_node(".clubtab > h3 > a") %>% html_text(trim = T)
  print(club)
  club_list[length(club_list)+1] <- club
  transfers_in <- clubs[i] %>% html_node(".transfers1") %>% html_nodes(".line")
  transfers_out <- clubs[i] %>% html_node(".transfers2") %>% html_nodes(".line")
  for (j in 1:length(transfers_in)) {
    tr_date <- transfers_in[j] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_in[j] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_in[j] %>% html_node(".player > a") %>% html_text(trim = T)
    club_from <- transfers_in[j] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_from_country <- transfers_in[j] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_in[j] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "in"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club_from, DivFrom = club_from_country, ClubTo = club,
                       DivTo = div_to, Amount = amount)
    data <- rbind(data,temp)
  }
  for (k in 1:length(transfers_out)) {
    tr_date <- transfers_out[k] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_out[k] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_out[k] %>% html_node(".player > a") %>% html_text(trim = T)
    club_to <- transfers_out[k] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_to_country <- transfers_out[k] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_out[k] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "out"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club, DivFrom = div_to, ClubTo = club_to,
                       DivTo = club_to_country, Amount = amount)
    data <- rbind(data,temp)
  }
}

#SERIA A
url <- "https://www.footballdatabase.eu/en/competition/transfers/59-serie_a/2004-2005"
page <- read_html(url)
clubs <- page %>% html_nodes(".transferstab > .line")
div_to <- "Italy"
for (i in 1:length(clubs)) {
  club <- clubs[i]  %>% html_node(".clubtab > h3 > a") %>% html_text(trim = T)
  print(club)
  club_list[length(club_list)+1] <- club
  transfers_in <- clubs[i] %>% html_node(".transfers1") %>% html_nodes(".line")
  transfers_out <- clubs[i] %>% html_node(".transfers2") %>% html_nodes(".line")
  for (j in 1:length(transfers_in)) {
    tr_date <- transfers_in[j] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_in[j] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_in[j] %>% html_node(".player > a") %>% html_text(trim = T)
    club_from <- transfers_in[j] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_from_country <- transfers_in[j] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_in[j] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "in"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club_from, DivFrom = club_from_country, ClubTo = club,
                       DivTo = div_to, Amount = amount)
    data <- rbind(data,temp)
  }
  for (k in 1:length(transfers_out)) {
    tr_date <- transfers_out[k] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_out[k] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_out[k] %>% html_node(".player > a") %>% html_text(trim = T)
    club_to <- transfers_out[k] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_to_country <- transfers_out[k] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_out[k] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "out"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club, DivFrom = div_to, ClubTo = club_to,
                       DivTo = club_to_country, Amount = amount)
    data <- rbind(data,temp)
  }
}

#LIGA 1
url <- "https://www.footballdatabase.eu/en/competition/transfers/53-ligue_1/2004-2005"
page <- read_html(url)
clubs <- page %>% html_nodes(".transferstab > .line")
div_to <- "France"
for (i in 1:length(clubs)) {
  club <- clubs[i]  %>% html_node(".clubtab > h3 > a") %>% html_text(trim = T)
  print(club)
  club_list[length(club_list)+1] <- club
  transfers_in <- clubs[i] %>% html_node(".transfers1") %>% html_nodes(".line")
  transfers_out <- clubs[i] %>% html_node(".transfers2") %>% html_nodes(".line")
  for (j in 1:length(transfers_in)) {
    tr_date <- transfers_in[j] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_in[j] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_in[j] %>% html_node(".player > a") %>% html_text(trim = T)
    club_from <- transfers_in[j] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_from_country <- transfers_in[j] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_in[j] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "in"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club_from, DivFrom = club_from_country, ClubTo = club,
                       DivTo = div_to, Amount = amount)
    data <- rbind(data,temp)
  }
  for (k in 1:length(transfers_out)) {
    tr_date <- transfers_out[k] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_out[k] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_out[k] %>% html_node(".player > a") %>% html_text(trim = T)
    club_to <- transfers_out[k] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_to_country <- transfers_out[k] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_out[k] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "out"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club, DivFrom = div_to, ClubTo = club_to,
                       DivTo = club_to_country, Amount = amount)
    data <- rbind(data,temp)
  }
}

#BUNDESLIGA
url <- "https://www.footballdatabase.eu/en/competition/transfers/54-bundesliga/2004-2005"
page <- read_html(url)
clubs <- page %>% html_nodes(".transferstab > .line")
div_to <- "Germany"
for (i in 1:length(clubs)) {
  club <- clubs[i]  %>% html_node(".clubtab > h3 > a") %>% html_text(trim = T)
  print(club)
  club_list[length(club_list)+1] <- club
  transfers_in <- clubs[i] %>% html_node(".transfers1") %>% html_nodes(".line")
  transfers_out <- clubs[i] %>% html_node(".transfers2") %>% html_nodes(".line")
  for (j in 1:length(transfers_in)) {
    tr_date <- transfers_in[j] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_in[j] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_in[j] %>% html_node(".player > a") %>% html_text(trim = T)
    club_from <- transfers_in[j] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_from_country <- transfers_in[j] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_in[j] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "in"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club_from, DivFrom = club_from_country, ClubTo = club,
                       DivTo = div_to, Amount = amount)
    data <- rbind(data,temp)
  }
  for (k in 1:length(transfers_out)) {
    tr_date <- transfers_out[k] %>% html_node(".date") %>% html_text(trim = T)
    player_country <- transfers_out[k] %>% html_node(".player > span") %>% html_attr("title")
    player_name <- transfers_out[k] %>% html_node(".player > a") %>% html_text(trim = T)
    club_to <- transfers_out[k] %>% html_node(".otherclub > a") %>% html_text(trim = T)
    club_to_country <- transfers_out[k] %>% html_node(".otherclub > span") %>% html_attr("title")
    amount <- transfers_out[k] %>% html_node(".amount") %>% html_text(trim = T)
    in_out <- "out"
    temp <- data.frame(TDate = tr_date, Nat = player_country, Name = player_name, InOut = in_out,
                       ClubFrom = club, DivFrom = div_to, ClubTo = club_to,
                       DivTo = club_to_country, Amount = amount)
    data <- rbind(data,temp)
  }
}
club_df <- data.frame(Club = as.character(club_list))
club_df <- data.frame(ID = 1:nrow(club_df),Club = club_df$Club)
row.names(club_df) <- NULL

write.csv(data, "data/transfers_data_04.csv", row.names = F)
write.csv(club_df, "data/clubs_data_raw_04.csv",row.names = F)
