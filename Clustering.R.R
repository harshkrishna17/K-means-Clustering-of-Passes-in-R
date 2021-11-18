#' Loading necessary libraries.
#' Run install.packages("name-of-package") to install 
#' if package doesn't appear to be in your system. 

library(tidyverse)
library(ggfortify)
library(ggsoccer)
library(ggtext)
library(gghighlight)
library(ggpubr)
library(cowplot)
library(StatsBombR)

#' Data Manipulation :-
#' 1. Pulling the data from a specific competition
#' 2. Calculating Progressive passes
#' 3. Filtering and selecting specific columns

dataframe <- FreeCompetitions() %>%
  filter(competition_id == 37 & season_name == "2020/2021")
df <- FreeMatches(dataframe)
StatsBombData <- StatsBombFreeEvents(MatchesDF = df, Parallel = T)
data <- allclean(StatsBombData)

data1 <- data %>%
  filter(type.name == "Pass")

data1$sdist <- sqrt((120 - data1$location.x)**2 + (40 - data1$location.y)**2)
data1$edist <- sqrt((120 - data1$pass.end_location.x)**2 + (40 - data1$pass.end_location.y)**2)
data1$progcalc <- (data1$sdist - data1$edist)/ data1$sdist
data1$isProg <- ifelse(data1$progcalc >= 0.25, "True", "False")

data1 <- data1 %>%  
  filter(player.name == "Chloe Kelly") %>%
  filter(isProg == "True") %>%
  rename(outcomeKey = pass.recipient.id)

data1$outcomeKey[is.na(data1$outcomeKey)] <- 0
data1$outcomeKey[!data1$outcomeKey == 0] <- 1
outcomeKey <- data1$outcomeKey

data1 <- data1[, c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y")]

#' Using the elbow method to find optimal number of clusters for k-means.
#' The point on the x-axis where the sharp bend in the plot occurs shows the optimal 
#' number of clusters. If the correct nukber of clusters aren't selected then the 
#' result of the model would be incorrect (eg. number of passes in cluster 2 > number of passes in cluster 1).

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(data1)

#' Implementing k-means

kmean <- kmeans(data1, 3)

data1$cluster <- as.character(kmean$cluster)
data1$outcomeKey <- as.character(outcomeKey)
data1$outcomeKey[data1$outcomeKey == 1] <- "Successful"
data1$outcomeKey[data1$outcomeKey == 0] <- "Unsuccessful"

df <- split(data1, data1$cluster)

# Creating custom theme function 

theme_custom <- function(){
  theme(plot.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D"),
        panel.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D")) +
    theme(plot.title = element_text(colour = "white", size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(colour = "#2171b5", size = 15, hjust = 0.5))
}

#' Plotting 
#' The following parts shall demonstrate two designs of plots
#' inspired by different people.
#'  
#' Method 1. Inspired by @amonizfootball

p1 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["1"]], aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = outcomeKey),
               lineend = "butt", size = 1, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  scale_colour_manual(values = "#2171b5") +
  gghighlight(outcomeKey == "Successful",
              unhighlighted_colour = "#525252") +
  labs(title = "Cluster 1",
       subtitle = "Successful Passes = 94/115") +
  theme_custom() +
  theme(legend.position = "none")
p1

p2 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["2"]], aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = outcomeKey),
               lineend = "butt", size = 1, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  scale_colour_manual(values = "#2171b5") +
  gghighlight(outcomeKey == "Successful",
              unhighlighted_colour = "#525252") +
  labs(title = "Cluster 2",
       subtitle = "Successful Passes = 44/45") +
  theme_custom() +
  theme(legend.position = "none")
p2

p3 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["3"]], aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = outcomeKey),
               lineend = "butt", size = 1, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  scale_colour_manual(values = "#2171b5") +
  gghighlight(outcomeKey == "Successful",
              unhighlighted_colour = "#525252") +
  labs(title = "Cluster 3",
       subtitle = "Successful Passes = 35/53") +
  theme_custom() +
  theme(legend.position = "none")
p3

fig <- ggarrange(p1, p2, p3,
                 nrow = 1, ncol = 3)
fig1 <- fig +
  labs(title = "Chloe Kelly - Progressive Pass Clusters",
       subtitle = "NWSL Games [2020/21]",
       caption = "Data from StatsBomb
       Inspired by @amonizfootball 
       by @placeholder2004") +
  theme(plot.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D"),
        panel.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D")) +
  theme(plot.title = element_text(colour = "white", size = 25, face = "bold"),
        plot.subtitle = element_text(colour = "white", size = 20),
        plot.caption = element_text(colour = "white", size = 12, hjust = 1))
fig1

#' Draw arrow denoting attacking direction

ggdraw(fig1, xlim = c(0,1), ylim = c(0,1))+
  draw_line(x = c(0.4, 0.6),
            y = c(0.06, 0.06),
            color = "white", size = 1.2,
            arrow = arrow(length = unit(0.12, "inches"), type = "closed"))+
  draw_text("Attacking Direction", x=0.5, y=0.1, colour = "white", size = 12)

#' Save

setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents/Python Scripts")
ggsave("chloe1.png", bg = "black", width = 3000, height = 1100, units = "px")

#' Method 2. Inspired by @pwawrzynow and @jonollington

df1 <- df[["1"]]
df1 <- df1 %>%
  mutate(X = mean(location.x)) %>%
  mutate(Y = mean(location.y)) %>%
  mutate(endX = mean(pass.end_location.x)) %>%
  mutate(endY = mean(pass.end_location.y))

x1 <- df1$X
y1 <- df1$Y
endx1 <- df1$endX
endy1 <- df1$endY

df2 <- df[["2"]]
df2 <- df2 %>%
  mutate(X = mean(location.x)) %>%
  mutate(Y = mean(location.y)) %>%
  mutate(endX = mean(pass.end_location.x)) %>%
  mutate(endY = mean(pass.end_location.y))

x2 <- df2$X
y2 <- df2$Y
endx2 <- df2$endX
endy2 <- df2$endY

df3 <- df[["3"]]
df3 <- df3 %>%
  mutate(X = mean(location.x)) %>%
  mutate(Y = mean(location.y)) %>%
  mutate(endX = mean(pass.end_location.x)) %>%
  mutate(endY = mean(pass.end_location.y))

x3 <- df3$X
y3 <- df3$Y
endx3 <- df3$endX
endy3 <- df3$endY

custom_theme <- function(){
  theme(plot.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D"),
        panel.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D")) +
    theme(plot.title = element_text(colour = "white", size = 12, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(colour = "white", size = 10, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 8))
}

figure <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["1"]], aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = "#fec44f"),
               lineend = "butt", size = 0.5, alpha = 0.08, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  annotate("segment", x = x1, y = y1, 
           xend = endx1, yend = endy1, 
           colour = "#2171b5", size = 2, arrow = arrow()) +
  geom_segment(data = df[["2"]], aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = "#41ab5d"),
               lineend = "butt", size = 0.5, alpha = 0.08, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  annotate("segment", x = x2, y = y2, 
           xend = endx2, yend = endy2, 
           colour = "#41ab5d", size = 2, arrow = arrow()) +
  geom_segment(data = df[["3"]], aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = "#2171b5"),
               lineend = "butt", size = 0.5, alpha = 0.08, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  annotate("segment", x = x3, y = y3, 
           xend = endx3, yend = endy3, 
           colour = "#fec44f", size = 2, arrow = arrow()) +
  scale_colour_manual(values = c("#fec44f", "#41ab5d", "#2171b5")) +
  labs(title = "Chloe Kelly - Top 3 Progressive Pass Clusters",
       subtitle = "WSL Games [2020/21]",
       caption = "Inspired by
       @pwawrzynow &
       @jonollington") +
  custom_theme() +
  theme(legend.position = "none")
figure

#' Draw arrow denoting attacking direction

ggdraw(figure, xlim = c(0,1), ylim = c(0,1))+
  draw_line(x = c(0.4, 0.6),
            y = c(0.06, 0.06),
            color = "white", size = 1.2,
            arrow = arrow(length = unit(0.12, "inches"), type = "closed"))+
  draw_text("Attacking Direction", x=0.5, y=0.035, colour = "white", size = 12)

#' Save

setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents/Python Scripts")
ggsave("jon prog.png", bg = "#0D0D0D", width = 1300, height = 1100, units = "px")