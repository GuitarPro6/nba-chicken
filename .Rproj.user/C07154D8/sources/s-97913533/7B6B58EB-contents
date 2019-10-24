library(stringr)
library(tidyr)
library(here)

#Calculating the probability of "Fowl-Pay" promotion. 
ft_data <- read.csv(here('free_throws.csv'), header=TRUE,stringsAsFactors=FALSE)
#618,000 free throws between 2006 and 2016
#Transform game column
ft_data <- separate(ft_data,col="game",into=c("Away", "Home"), sep=" - ")
ft_data <- separate(ft_data,col="score",into=c("Away_sc", "Home_sc"), sep=" - ")
ft_data$Home_sc <- as.numeric(ft_data$Home_sc)
ft_data$Away_sc <- as.numeric(ft_data$Away_sc)
ft_data$Home_sc_diff <- c(NA,diff(ft_data$Home_sc))
ft_data$Away_sc_diff <- c(NA,diff(ft_data$Away_sc))
ft_data$home_bool <- as.integer(as.logical((ft_data$shot_made == 1 & ft_data$Home_sc_diff == 1) | (ft_data$shot_made == 0 & ft_data$Home_sc_diff == 0)))
#1st Quarter
first_ft <- ft_data[which(ft_data$period==1.0),]
shot_sum <- aggregate(first_ft[c("shot_made","home_bool")],by=list(first_ft$player,first_ft$time,first_ft$period,first_ft$game_id,first_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
first_both_made <- shot_sum[which(shot_sum$shot_made >= 2),]
first_both_miss <- shot_sum[which(shot_sum$shot_made == 0),]

#2nd Quarter
second_ft <- ft_data[which(ft_data$period==2.0),]
shot_sum <- aggregate(second_ft[c("shot_made","home_bool")],by=list(second_ft$player,second_ft$time,second_ft$period,second_ft$game_id, second_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
second_both_made <- shot_sum[which(shot_sum$shot_made >= 2),]
second_both_miss <- shot_sum[which(shot_sum$shot_made == 0),]

#3rd Quarter
third_ft <- ft_data[which(ft_data$period==3.0),]
shot_sum <- aggregate(third_ft[c("shot_made","home_bool")],by=list(third_ft$player,third_ft$time,third_ft$period,third_ft$game_id,third_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
third_both_made <- shot_sum[which(shot_sum$shot_made >= 2),]
third_both_miss <- shot_sum[which(shot_sum$shot_made == 0),]

#4th quarter
#186,280 free throws in fourth quarter

fourth_ft <- ft_data[which(ft_data$period==4.0),]
shot_sum <- aggregate(fourth_ft[c("shot_made","home_bool")],by=list(fourth_ft$player,fourth_ft$time,fourth_ft$period,fourth_ft$game_id,fourth_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
fourth_both_made <- shot_sum[which(shot_sum$shot_made == 2),]
fourth_both_miss <- shot_sum[which(shot_sum$shot_made == 0),]

fourth_both_miss_opp <- shot_sum[which(shot_sum$shot_made == 0 & shot_sum$home_bool == 0),]


############NO CONSECUTIVE MISSES

#number of games
num_games <- length(unique(ft_data$game_id))

#number of games with no consecutive missed free throws in fourth quarter by opponent
num_no_miss <- num_games - length(unique(fourth_both_miss_opp$game_id))

#probability of chicken
1-num_no_miss/num_games

#Misses by opponent in fourth quarter
fourth_both_miss_opp <- shot_sum[which(shot_sum$shot_made == 0 & shot_sum$home_bool==0),]


#Jazz

jazz_fourth_ft <- fourth_ft[which(fourth_ft$Home == "UTAH" | fourth_ft$Away == "UTAH"),]
jazz_fourth_home_ft <- fourth_ft[which(fourth_ft$Home == "UTAH"),]
shot_sum <- aggregate(jazz_fourth_home_ft[c("shot_made","home_bool")],by=list(jazz_fourth_home_ft$player,jazz_fourth_home_ft$time,jazz_fourth_home_ft$period,jazz_fourth_home_ft$game_id,jazz_fourth_home_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
both_miss <- shot_sum[which(shot_sum$shot_made == 0 & shot_sum$home_bool == 0),]

#number of games
num_games <- length(unique(jazz_fourth_home_ft$game_id))

#number of games with no consecutive missed free throws in fourth quarter by opponent
num_no_miss <- num_games - length(unique(both_miss$game_id))

#probability of chicken
1-num_no_miss/num_games


#Wizards
jazz_fourth_home_ft <- fourth_ft[which(fourth_ft$Home == "WSH"),]
shot_sum <- aggregate(jazz_fourth_home_ft[c("shot_made","home_bool")],by=list(jazz_fourth_home_ft$player,jazz_fourth_home_ft$time,jazz_fourth_home_ft$period,jazz_fourth_home_ft$game_id,jazz_fourth_home_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
both_miss <- shot_sum[which(shot_sum$shot_made == 0 & shot_sum$home_bool == 0),]

#number of games
num_games <- length(unique(jazz_fourth_home_ft$game_id))

#number of games with no consecutive missed free throws in fourth quarter by opponent
num_no_miss <- num_games - length(unique(both_miss$game_id))

#probability of chicken
1-num_no_miss/num_games


#Bulls
jazz_fourth_home_ft <- fourth_ft[which(fourth_ft$Home == "CHI"),]
shot_sum <- aggregate(jazz_fourth_home_ft[c("shot_made","home_bool")],by=list(jazz_fourth_home_ft$player,jazz_fourth_home_ft$time,jazz_fourth_home_ft$period,jazz_fourth_home_ft$game_id,jazz_fourth_home_ft$season), sum)
colnames(shot_sum) <- c("player", "time", "period", "game_id", "season", "shot_made", "home_bool")
both_miss <- shot_sum[which(shot_sum$shot_made == 0 & shot_sum$home_bool == 0),]

#number of games
num_games <- length(unique(jazz_fourth_home_ft$game_id))

#number of games with no consecutive missed free throws in fourth quarter by opponent
num_no_miss <- num_games - length(unique(both_miss$game_id))

#probability of chicken
1-num_no_miss/num_games

#14,090 free throws made when Jazz are playing in the fourth quarter

#Misses in the fourth quarter in general with the Jazz
shot_sum <- aggregate(jazz_fourth_ft[c("shot_made","home_bool")],by=list(jazz_fourth_ft$player,jazz_fourth_ft$time,jazz_fourth_ft$period,jazz_fourth_ft$season), sum)
both_made <- shot_sum[which(shot_sum$shot_made == 2),]
both_miss <- shot_sum[which(shot_sum$shot_made == 0),]


#Misses in general

#Misses when Utah at home by opponent

#first
jazz_first <- first_ft[which(first_ft$Home == "UTAH"),]
jfft_home_sum <- aggregate(jazz_first[c("shot_made","home_bool")],by=list(jazz_first$player,jazz_first$time,jazz_first$period,jazz_first$season), sum)
first_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
first_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#second
jazz_second <- second_ft[which(second_ft$Home == "UTAH"),]
jfft_home_sum <- aggregate(jazz_second[c("shot_made","home_bool")],by=list(jazz_second$player,jazz_second$time,jazz_second$period,jazz_second$season), sum)
second_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
second_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#third
jazz_third <- third_ft[which(fourth_ft$Home == "UTAH"),]
jfft_home_sum <- aggregate(jazz_third[c("shot_made","home_bool")],by=list(jazz_third$player,jazz_third$time,jazz_third$period,jazz_third$season), sum)
third_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
third_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]

#fourth
jazz_fourth_ft_home_opp <- fourth_ft[which(fourth_ft$Home == "UTAH"),]
jfft_home_sum <- aggregate(jazz_fourth_ft_home_opp[c("shot_made","home_bool")],by=list(jazz_fourth_ft_home_opp$player,jazz_fourth_ft_home_opp$time,jazz_fourth_ft_home_opp$period,jazz_fourth_ft_home_opp$season), sum)
fourth_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
fourth_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#Washington
#Misses when Utah at home by opponent

#first
jazz_first <- first_ft[which(first_ft$Home == "WSH"),]
jfft_home_sum <- aggregate(jazz_first[c("shot_made","home_bool")],by=list(jazz_first$player,jazz_first$time,jazz_first$period,jazz_first$season), sum)
first_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
first_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#second
jazz_second <- second_ft[which(second_ft$Home == "WSH"),]
jfft_home_sum <- aggregate(jazz_second[c("shot_made","home_bool")],by=list(jazz_second$player,jazz_second$time,jazz_second$period,jazz_second$season), sum)
second_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
second_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#third
jazz_third <- third_ft[which(third_ft$Home == "WSH"),]
jfft_home_sum <- aggregate(jazz_third[c("shot_made","home_bool")],by=list(jazz_third$player,jazz_third$time,jazz_third$period,jazz_third$season), sum)
third_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
third_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]

#fourth
jazz_fourth_ft_home_opp <- fourth_ft[which(fourth_ft$Home == "WSH"),]
jfft_home_sum <- aggregate(jazz_fourth_ft_home_opp[c("shot_made","home_bool")],by=list(jazz_fourth_ft_home_opp$player,jazz_fourth_ft_home_opp$time,jazz_fourth_ft_home_opp$period,jazz_fourth_ft_home_opp$season), sum)
fourth_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
fourth_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#Chicago
#Misses when Utah at home by opponent

#first
jazz_first <- first_ft[which(first_ft$Home == "CHI"),]
jfft_home_sum <- aggregate(jazz_first[c("shot_made","home_bool")],by=list(jazz_first$player,jazz_first$time,jazz_first$period,jazz_first$season), sum)
first_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
first_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#second
jazz_second <- second_ft[which(second_ft$Home == "CHI"),]
jfft_home_sum <- aggregate(jazz_second[c("shot_made","home_bool")],by=list(jazz_second$player,jazz_second$time,jazz_second$period,jazz_second$season), sum)
second_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
second_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]


#third
jazz_third <- third_ft[which(third_ft$Home == "CHI"),]
jfft_home_sum <- aggregate(jazz_third[c("shot_made","home_bool")],by=list(jazz_third$player,jazz_third$time,jazz_third$period,jazz_third$season), sum)
third_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
third_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]

#fourth
jazz_fourth_ft_home_opp <- fourth_ft[which(fourth_ft$Home == "CHI"),]
jfft_home_sum <- aggregate(jazz_fourth_ft_home_opp[c("shot_made","home_bool")],by=list(jazz_fourth_ft_home_opp$player,jazz_fourth_ft_home_opp$time,jazz_fourth_ft_home_opp$period,jazz_fourth_ft_home_opp$season), sum)
fourth_both_made <- jfft_home_sum[which(jfft_home_sum$shot_made == 2),]
fourth_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0),]

opp_both_miss <- jfft_home_sum[which(jfft_home_sum$shot_made == 0 & jfft_home_sum$home_bool==0),]

#Home team player misses free throw


num_games <- length(unique(ft_data$game_id))
num_games_no_miss <- length(unique(ft_data[which(ft_data$home_bool == 1 & ft_data$shot_made == 0),'game_id']))

num_games_no_miss/num_games

#Promo started in 2020, is there a difference with the Jazz, Wizards, and Rockets?

#Look at probabilities for the other teams, is it similar? 

#What proportion of free throws is in the 4th quarter? A lot right?

#What is the probability you will get free Chic-fil-a at a give Jazz home game? Probability that there is a fourth quarter foul shot
#multiplied by probability that the opponent of the Jazz misses two consecutive free throw (probability of missing first multiplied by probability of missing second.)

#Prob that they miss both should be 1 - P(make at least one), check that these are correct. 





#https://www.printyourbrackets.com/how-many-games-are-in-the-nba-season.html 1230 games during NBA season

#https://ftw.usatoday.com/2017/10/how-free-chick-fil-a-made-fans-stop-caring-if-their-team-was-winning-or-losing-for-nearly-a-decade
#https://www.sportsbusinessdaily.com/Daily/Issues/2016/03/17/Marketing-and-Sponsorship/Chick-Fil-A.aspx
#https://www.geeksforgeeks.org/sql-group-by/
#https://www.wsj.com/articles/missed-free-throws-for-players-means-free-chicken-sandwiches-for-fans-1458170731
#
