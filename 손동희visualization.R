basket <- read.csv("basket1718.csv")
ticket <- fread("ticket1718.csv", header=T)

################# unite ticket data for ticket variable ###############
basket$pos %<>% as.factor
basket$inputtime %<>% as.factor


basket %<>% select(-season_code, -inputtime, - game_code)
colnames(basket)

basket <- left_join(basket, ticket, by="game_no")

################## make lag variable ############# 

setDT(basket)[, paste0("lag_", colnames(basket)[1]) := shift(game_no, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[2]) := shift(team_code, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[3]) := shift(home_away, 1), player_no]

setDT(basket)[, paste0("lag_", colnames(basket)[6]) := shift(pos, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[7]) := shift(away_team, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[8]) := shift(start_flag, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[9]) := shift(play_min, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[10]) := shift(play_sec, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[11]) := shift(fg, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[12]) := shift(fg_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[13]) := shift(ft, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[14]) := shift(ft_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[15]) := shift(threep, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[16]) := shift(threep_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[17]) := shift(dk, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[18]) := shift(dk_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[19]) := shift(pp, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[20]) := shift(pp_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[21]) := shift(o_r, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[22]) := shift(d_r, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[23]) := shift(a_s, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[24]) := shift(s_t, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[25]) := shift(b_s, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[26]) := shift(gd, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[27]) := shift(t_o, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[28]) := shift(wft, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[29]) := shift(woft, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[30]) := shift(idf, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[31]) := shift(tf, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[32]) := shift(ef, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[33]) := shift(foul_tot, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[34]) := shift(fb, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[35]) := shift(p_score, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[36]) := shift(score, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[37]) := shift(inout, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[38]) := shift(inout1, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[39]) := shift(fo, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[40]) := shift(ticket, 1), player_no]


# Make Binomial Column ------------------------------------------------------------------------------------------------------------------------------------

basket %<>% mutate(pos_rating = 
                     (score + s_t + b_s + d_r) * 1.0 +
                     (o_r + a_s + gd) * 1.5 +
                     (play_min) / 4,
                   neg_rating =
                     (t_o) * 1.5 + 
                     (fg_a - fg) * 1.0 + 
                     (threep_a - threep) * 0.9 + 
                     (ft_a - ft) * 0.8,
                   lag_pos_rating = 
                     (lag_score + lag_s_t + lag_b_s + lag_d_r) * 1.0 +
                     (lag_o_r + lag_a_s + lag_gd) * 1.5 +
                     (lag_play_min) / 4,
                   lag_neg_rating =
                     (lag_t_o) * 1.5 + 
                     (lag_fg_a - lag_fg) * 1.0 + 
                     (lag_threep_a - lag_threep) * 0.9 + 
                     (lag_ft_a - lag_ft) * 0.8,
                   rating = pos_rating - neg_rating,
                   lag_rating = lag_pos_rating - lag_neg_rating,
                   binom = ifelse((rating - lag_rating)>0, 1, 0)) %>% 
  select(-pos_rating, -neg_rating, -lag_pos_rating, -lag_neg_rating, -rating, -lag_rating)

# Percentage ---------------------------------------------------------------------------------------------------------------------------------------------

basket %<>% mutate(lag_percentage = (lag_fg+lag_ft+lag_threep) / (lag_fg_a+lag_ft_a+lag_threep_a))


# Make ROUND variable -----------------------------------------------------------------------------------------------------------------------------------

basket %<>% arrange(game_no, team_code)
basket_team <- basket %>% select(game_no, team_code)
basket_team %<>% unique()

round <- vector('numeric', length = nrow(basket_team))

for(i in unique(basket_team$team_code)){
  round[basket_team$team_code == i] <- 2:(sum(basket_team$team_code == i)+1)
}

basket_team$round <- round

basket_team %<>% unite(key, game_no, team_code)

basket$key <- paste(basket$game_no, basket$team_code, sep = '_')

basket <- left_join(basket, basket_team, by = c('key' = 'key')) %>% select(-key)

write.csv(basket, "eda_basket1718.csv", row.names=F) #for EDA



################################ 2016 - 2017 data ###############################


################## read data ################

basket <- read.csv("basket1617.csv")
ticket <- fread("ticket1617.csv", header=T)

################# unite ticket data for ticket variable ###############
basket$pos %<>% as.factor
basket$inputtime %<>% as.factor

basket %<>% select(-season_code, -inputtime, - game_code)
colnames(basket)

basket <- left_join(basket, ticket, by="game_no")

################## make lag variable ############# 

setDT(basket)[, paste0("lag_", colnames(basket)[1]) := shift(game_no, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[2]) := shift(team_code, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[3]) := shift(home_away, 1), player_no]

setDT(basket)[, paste0("lag_", colnames(basket)[6]) := shift(pos, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[7]) := shift(away_team, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[8]) := shift(start_flag, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[9]) := shift(play_min, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[10]) := shift(play_sec, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[11]) := shift(fg, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[12]) := shift(fg_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[13]) := shift(ft, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[14]) := shift(ft_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[15]) := shift(threep, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[16]) := shift(threep_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[17]) := shift(dk, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[18]) := shift(dk_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[19]) := shift(pp, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[20]) := shift(pp_a, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[21]) := shift(o_r, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[22]) := shift(d_r, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[23]) := shift(a_s, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[24]) := shift(s_t, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[25]) := shift(b_s, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[26]) := shift(gd, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[27]) := shift(t_o, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[28]) := shift(wft, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[29]) := shift(woft, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[30]) := shift(idf, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[31]) := shift(tf, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[32]) := shift(ef, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[33]) := shift(foul_tot, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[34]) := shift(fb, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[35]) := shift(p_score, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[36]) := shift(score, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[37]) := shift(inout, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[38]) := shift(inout1, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[39]) := shift(fo, 1), player_no]
setDT(basket)[, paste0("lag_", colnames(basket)[40]) := shift(ticket, 1), player_no]



# Make Binomial Column ------------------------------------------------------------------------------------------------------------------------------------

basket %<>% mutate(pos_rating = 
                     (score + s_t + b_s + d_r) * 1.0 +
                     (o_r + a_s + gd) * 1.5 +
                     (play_min) / 4,
                   neg_rating =
                     (t_o) * 1.5 + 
                     (fg_a - fg) * 1.0 + 
                     (threep_a - threep) * 0.9 + 
                     (ft_a - ft) * 0.8,
                   lag_pos_rating = 
                     (lag_score + lag_s_t + lag_b_s + lag_d_r) * 1.0 +
                     (lag_o_r + lag_a_s + lag_gd) * 1.5 +
                     (lag_play_min) / 4,
                   lag_neg_rating =
                     (lag_t_o) * 1.5 + 
                     (lag_fg_a - lag_fg) * 1.0 + 
                     (lag_threep_a - lag_threep) * 0.9 + 
                     (lag_ft_a - lag_ft) * 0.8,
                   rating = pos_rating - neg_rating,
                   lag_rating = lag_pos_rating - lag_neg_rating,
                   binom = ifelse((rating - lag_rating)>0, 1, 0)) %>% 
  select(-pos_rating, -neg_rating, -lag_pos_rating, -lag_neg_rating, -rating, -lag_rating)

# Percentage ---------------------------------------------------------------------------------------------------------------------------------------------

basket %<>% mutate(lag_percentage = (lag_fg+lag_ft+lag_threep) / (lag_fg_a+lag_ft_a+lag_threep_a))


# Make ROUND variable -----------------------------------------------------------------------------------------------------------------------------------

basket %<>% arrange(game_no, team_code)
basket_team <- basket %>% select(game_no, team_code)
basket_team %<>% unique()

round <- vector('numeric', length = nrow(basket_team))

for(i in unique(basket_team$team_code)){
  round[basket_team$team_code == i] <- 2:(sum(basket_team$team_code == i)+1)
}

basket_team$round <- round

basket_team %<>% unite(key, game_no, team_code)

basket$key <- paste(basket$game_no, basket$team_code, sep = '_')

basket <- left_join(basket, basket_team, by = c('key' = 'key')) %>% select(-key)

write.csv(basket, "eda_basket1617.csv", row.names=F)


#############Start EDA######################33
###########################################
library(RColorBrewer)
eda1617 <- read.csv("eda_basket1617.csv")
eda1718 <- read.csv("eda_basket1718.csv")
eda <- rbind(eda1617, eda1718)

# clustering label join#

clust_info <- read.csv("player_cluster_inf.csv")
clust_info %<>%  select(player_no, cluster)

eda <- left_join(eda, clust_info, by = 'player_no')

eda %>% filter(play_min >= 10) %>% group_by(player_no) %>% summarize_if(is.numeric, mean) %>% nrow()
#208row 

# theme function.
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#(1) Postion
position <- eda %>% mutate(pos = as.character(pos)) %>% 
  group_by(pos, player_no) %>%  summarise_if(is.numeric, mean) %>% 
  ggplot(aes(x = pos)) + 
  geom_bar(alpha =  0.8, col = palette[2], aes(fill = pos)) + 
  fte_theme() +theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
                    axis.title = element_text(hjust=0.5, size= 10, face='bold'),
                    legend.position = 'none',
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = 'Position_Count', x = 'Position', y = 'Count')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(position, filename = "Position_Count.png")

#(2) Play Min ( 10min in 1 quarter )

range(eda$play_min)
eda$cluster <- as.character(eda$cluster)
palette <- brewer.pal("Greys", n=9)

#10분이상 자른 근거 (1 quarter)

tenmin_reason <- eda %>% filter(play_min > 0) %>% ggplot(aes(x = play_min)) + 
  geom_histogram(breaks = seq(0,50, 1), alpha = 0.8, fill = "#c0392b", col = "white") +
  xlim(c(0,50)) + 
  theme(plot.title = element_text(hjust=0.5,size = 15,face = 'bold'),
        axis.title = element_text(hjust=0.5,size= 10,face='bold'),
        legend.position = 'none',
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent" , colour = 'transparent'))+
  labs(title = 'Histogram of Play_Min', x = 'Play_Min', y = 'Count')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(tenmin_reason, filename = "Histogram of Play_Min.png", bg = "transparent")

#facet by cluster
pf <- eda %>% filter(play_min > 0 & is.na(cluster)==0) %>% ggplot(aes(x = play_min)) + 
  geom_histogram(aes(fill = cluster),breaks = seq(0,50, 1),  col = palette[2]) +
  xlim(c(0,50)) + facet_wrap(~cluster, nrow = 1)+
  fte_theme() +
  theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
        axis.title = element_text(hjust=0.5, size = 10, face='bold'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size=8),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="grey", fill=palette[2])) +
  labs(title = 'Histogram of Play_Min by Cluster', x = 'Play_Min', y = 'Count')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(pf, filename = "Histogram of Play_Min by Cluster.png")

#(3) ticket
range(eda$ticket)
edaticket<- eda %>% ggplot(aes(x = ticket)) + 
  geom_histogram(breaks = seq(700,7600, 300), alpha = 0.8, fill = "#c0392b", col = palette[2]) +
  xlim(c(700,7600)) + fte_theme() +
  theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
        axis.title = element_text(hjust=0.5, size = 10, face='bold'),
        legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = 'Histogram of Ticket Sales', x = 'Ticket Sales', y = 'Count')+
  geom_hline(yintercept=0, size=1, color="grey")

ggsave(edaticket, filename = "Histogram of Ticket Sales.png")


## ticket flow by season and game
flow1 <- eda1617 %>% group_by(team_code, round) %>% 
  summarise(mean = mean(ticket, na.rm =T)) %>% ungroup %>% 
  mutate(team_code = as.character(team_code)) %>% 
  ggplot(aes(x = round, y = mean)) + geom_line(aes(col = team_code)) + 
  geom_point(aes(col = team_code)) + facet_wrap(~ team_code, nrow = 2)+
  fte_theme() +
  theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
        axis.title = element_text(hjust=0.5, size = 10, face='bold'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size=8),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="grey", fill=palette[2])) +
  labs(title = 'Flow of Ticket Sales by Team 1617', x = 'Round', y = 'Sales')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(flow1, filename = "Flow of Ticket Sales by Team 1617.png")


flow2 <- eda1718 %>% group_by(team_code, round) %>% 
  summarise(mean = mean(ticket, na.rm =T)) %>% ungroup %>% 
  mutate(team_code = as.character(team_code)) %>% 
  ggplot(aes(x = round, y = mean)) + geom_line(aes(col = team_code)) + 
  geom_point(aes(col = team_code)) + facet_wrap(~ team_code, nrow = 2)+
  fte_theme() +
  theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
        axis.title = element_text(hjust=0.5, size = 10, face='bold'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size=8),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="grey", fill=palette[2])) +
  labs(title = 'Flow of Ticket Sales by Team 1718', x = 'Round', y = 'Sales')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(flow2, filename = "Flow of Ticket Sales by Team 1718.png")

#(4) cluster info

clus <- clust_info %>%  filter(is.na(cluster) == 0)%>% 
  group_by(cluster) %>% summarise(count = n()) %>% mutate(cluster = as.character(cluster)) %>% 
  ggplot(aes(x=cluster, y = count, fill = cluster)) + 
  geom_bar(alpha =  0.8, col = "grey", width = 0.8, stat = "identity")+
  fte_theme() +theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
                     axis.title = element_text(hjust=0.5, size= 10, face='bold'),
                     legend.position = 'TRUE',
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = 'How many player in a cluster?', x = 'Cluster', y = 'Count')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(clus, filename = "How many player in a cluster.png")

#(5) Mean score whose play more than 10 min

normalscore <- eda %>% filter(play_min >= 10) %>% 
  group_by(player_no) %>% summarise(mean = mean(score)) %>%
  ggplot(aes(x = mean)) +
  geom_histogram(breaks = seq(0,30, 2), alpha = 0.8, fill = "#c0392b", col = palette[2]) +
  xlim(c(0,30)) + fte_theme() +
  theme(plot.title = element_text(hjust=0.5, size = 15, face = 'bold'),
        axis.title = element_text(hjust=0.5, size = 10, face='bold'),
        legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = 'Average Score of a Player', x = 'AVG SCORE', y = 'Count')+
  geom_hline(yintercept=0, size=1, color="grey")
  
ggsave(normalscore, filename = "Average Score of a Player.png")

##
clustscore <- eda %>% filter(play_min >=10  & is.na(cluster)==0) %>%  
  group_by(player_no) %>% summarise(mean = mean(score), cluster = mean(as.numeric(cluster))) %>% 
  mutate(cluster = as.character(cluster)) %>% 
  ggplot(aes(x = mean)) + 
  geom_histogram(aes(fill = cluster),breaks = seq(0,30, 2),  col = palette[2]) +
  xlim(c(0,30)) + facet_wrap(~cluster, nrow = 1)+
  fte_theme() +
  theme(plot.title = element_text(hjust=0.5, size = 12, face = 'bold'),
        axis.title = element_text(hjust=0.5, size = 10, face='bold'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(size=8),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="grey", fill=palette[2])) +
  labs(title = 'Average Score of a Player by Cluster', x = 'AVG SCORE', y = 'Count')+
  geom_hline(yintercept=0, size=0.4, color="grey")

ggsave(clustscore, filename = "Average Score of a Player by Cluster.png")



#### rating
rating_1718 <- basket %>% filter(play_min >= 10) %>% ggplot(aes(x = rating)) + 
  geom_histogram(breaks = seq(0,75, 3), alpha = 0.5, fill = "forestgreen", col = "white") +
  xlim(c(0,75)) + 
  theme(plot.title = element_text(hjust=0.5,size = 15,face = 'bold'),
        axis.title = element_text(hjust=0.5,size= 10,face='bold'),
        legend.position = 'none',
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent" , colour = 'transparent'))+
  labs(title = '17/18 Histogram of Rating', x = 'Rating', y = 'Count')+
  geom_hline(yintercept=0, size=0.8, color="grey")

ggsave(rating_1718, filename = "Histogram of Rating_1718.png", bg = "transparent")

#1617
rating_1617 <- basket %>% filter(play_min >= 10) %>% ggplot(aes(x = rating)) + 
  geom_histogram(breaks = seq(0,75, 3), alpha = 0.5, fill = "forestgreen", col = "white") +
  xlim(c(0,75)) + 
  theme(plot.title = element_text(hjust=0.5,size = 15,face = 'bold'),
        axis.title = element_text(hjust=0.5,size= 10,face='bold'),
        legend.position = 'none',
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent" , colour = 'transparent'))+
  labs(title = '16/17 Histogram of Rating', x = 'Rating', y = 'Count')+
  geom_hline(yintercept=0, size=0.8, color="grey")

ggsave(rating_1617, filename = "Histogram of Rating_1617.png", bg = "transparent")

# binom1617
 contribution1617 <-
   eda1617 %>% filter(play_min >= 10, !is.na(binom)) %>% mutate(binom = as.character(binom)) %>% 
   ggplot(aes(x = binom)) + geom_bar(width = 0.5, alpha = 0.8, aes(fill = binom)) +
  theme(plot.title = element_text(hjust=0.5,size = 15,face = 'bold'),
        axis.title = element_text(hjust=0.5,size= 10,face='bold'),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent" , colour = 'transparent'))+
  labs(title = '16/17 전 경기 대비 공헌도 상승여부', x = 'Binom', y = 'Count')+
  geom_hline(yintercept=0, size=0.8, color="grey")

ggsave(contribution1617, filename = "contribution1617.png", bg = "transparent")

# binom1718
contribution1718 <- 
  eda1718 %>% filter(play_min >= 10, !is.na(binom)) %>% mutate(binom = as.character(binom)) %>% 
  ggplot(aes(x = binom)) + geom_bar(width = 0.5, alpha = 0.8, aes(fill = binom)) +
  theme(plot.title = element_text(hjust=0.5,size = 15,face = 'bold'),
        axis.title = element_text(hjust=0.5,size= 10,face='bold'),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent" , colour = 'transparent'))+
  labs(title = '17/18 전 경기 대비 공헌도 상승여부', x = 'Binom', y = 'Count')+
  geom_hline(yintercept=0, size=0.8, color="grey")

ggsave(contribution1718, filename = "contribution1718.png", bg = "transparent")

