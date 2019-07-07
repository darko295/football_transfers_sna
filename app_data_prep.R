#### PREPARING DATA FOR THE APP
#### 1) transforming transfers data to long format

df_04 <- read.csv("data/clean_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
df_18 <- read.csv("data/clean_data_18.csv", header = T, sep = ",", stringsAsFactors = F)

df_04$Year_F <- 2004
df_18$Year_F <- 2018

df <- rbind(df_04,df_18)
write.csv(df,"data/td.csv",row.names = F)

#### same thing has to be done with club_df

club_df_04 <- read.csv("data/clubs_data_04.csv", header = T, sep = ",", stringsAsFactors = F)
club_df_18 <- read.csv("data/clubs_data_18.csv", header = T, sep = ",", stringsAsFactors = F)

club_df_04$Year_F <- 2004
club_df_18$Year_F <- 2018

club_df <- rbind(club_df_04,club_df_18)

write.csv(club_df,"data/cd.csv",row.names = F)
