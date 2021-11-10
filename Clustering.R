#' Loading neccessary libraries.
#' Run install.packages("name-of-package") to install 
#' if package doesn't appear to be in your system. 

library(tidyverse)
library(ggfortify)
library(ggsoccer)
library(ggtext)
library(gghighlight)
library(ggpubr)
library(cowplot)

#' Data Manipulation :-
#' 1. Calculating Progressive passes
#' 2. Filtering
#' 3. Selecting specific columns

setwd("C:/Users/harsh_1mwi2o4/Downloads")
data <- read.csv("MUProg2021.csv")

data <- data %>%
  mutate(X = x * 120/104) %>%
  mutate(Y = y * 80/68) %>%
  mutate(endX = endX * 120/104) %>%
  mutate(endY = endY * 80/68) %>%
  filter(isProg == "True") %>%
  filter(playerName == "Paul Pogba") %>%
  filter(etypename == "Pass") %>%
  na.omit()

data1 <- data[, c("X", "Y", "endX", "endY", "outcomeKey")]

#' Using the elbow method to find optimal number of clusters for k-means.
#' The point on the x-axis where the sharp bend in the plot occurs shows the optimal 
#' number of plots.

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
data1$outcomeKey[data$outcomeKey == 1] <- "Successful"
data1$outcomeKey[data$outcomeKey == 0] <- "Unsuccessful"

df <- split(data1, data1$cluster)

# Creating custom theme function 

theme_custom <- function(){
  theme(plot.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D"),
        panel.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D")) +
    theme(plot.title = element_text(colour = "white", size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(colour = "#2171b5", size = 15, hjust = 0.5))
}

#' Method 1. Inspired by @amonizfootball

p1 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["1"]], aes(x = X, y = Y,
                                  xend = endX, yend = endY,
                                 colour = outcomeKey),
               lineend = "butt", size = 1, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  scale_colour_manual(values = "#2171b5") +
  gghighlight(outcomeKey == "Successful",
              unhighlighted_colour = "#525252") +
  labs(title = "Cluster 1",
       subtitle = "Successful Passes = 28/68") +
  theme_custom() +
  theme(legend.position = "none")
p1

p2 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["2"]], aes(x = X, y = Y,
                                     xend = endX, yend = endY,
                                     colour = outcomeKey),
               lineend = "butt", size = 1, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  scale_colour_manual(values = "#2171b5") +
  gghighlight(outcomeKey == "Successful",
              unhighlighted_colour = "#525252") +
  labs(title = "Cluster 2",
       subtitle = "Successful Passes = 25/58") +
  theme_custom() +
  theme(legend.position = "none")
p2

p3 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["3"]], aes(x = X, y = Y,
                                     xend = endX, yend = endY,
                                     colour = outcomeKey),
               lineend = "butt", size = 1, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  scale_colour_manual(values = "#2171b5") +
  gghighlight(outcomeKey == "Successful",
              unhighlighted_colour = "#525252") +
  labs(title = "Cluster 3",
       subtitle = "Successful Passes = 24/53") +
  theme_custom() +
  theme(legend.position = "none")
p3

fig <- ggarrange(p1, p2, p3,
                 nrow = 1, ncol = 3)
fig1 <- fig +
  labs(title = "Paul Pogba - Progressive Pass Clusters",
       subtitle = "Premier League Games [2020/21]",
       caption = "@placeholder2004") +
  theme(plot.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D"),
        panel.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D")) +
  theme(plot.title = element_text(colour = "white", size = 25, face = "bold"),
        plot.subtitle = element_text(colour = "white", size = 20),
        plot.caption = element_text(colour = "white", size = 12, hjust = 1))
fig1

ggdraw(fig1, xlim = c(0,1), ylim = c(0,1))+
  draw_line(x = c(0.4, 0.6),
            y = c(0.06, 0.06),
            color = "white", size = 1.2,
            arrow = arrow(length = unit(0.12, "inches"), type = "closed"))+
  draw_text("Attacking Direction", x=0.5, y=0.1, colour = "white", size = 12)

  
setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents/Python Scripts")
ggsave("prog.png", bg = "black", width = 3000, height = 1100, units = "px")

#' Method 2. Inspired by @pwawrzynow and @jonollington

df1 <- df[["1"]]
df1 <- df1 %>%
  mutate(X = mean(X)) %>%
  mutate(Y = mean(Y)) %>%
  mutate(endX = mean(endX)) %>%
  mutate(endY = mean(endY))

df2 <- df[["2"]]
df2 <- df2 %>%
  mutate(X = mean(X)) %>%
  mutate(Y = mean(Y)) %>%
  mutate(endX = mean(endX)) %>%
  mutate(endY = mean(endY))

df3 <- df[["3"]]
df3 <- df3 %>%
  mutate(X = mean(X)) %>%
  mutate(Y = mean(Y)) %>%
  mutate(endX = mean(endX)) %>%
  mutate(endY = mean(endY))

custom_theme <- function(){
  theme(plot.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D"),
        panel.background = element_rect(colour = "#0D0D0D", fill = "#0D0D0D")) +
    theme(plot.title = element_text(colour = "white", size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(colour = "white", size = 15, hjust = 0.5))
}

figure <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, 
                 fill = "black", colour = "white") +
  theme_pitch() +
  geom_segment(data = df[["1"]], aes(x = X, y = Y,
                                     xend = endX, yend = endY,
                                     colour = "#88419d"),
               lineend = "butt", size = 0.7, alpha = 0.2, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  geom_segment(data = df1, aes(x = X, y = Y,
                               xend = endX, yend = endY,
                               colour = "#88419d"),
               lineend = "butt", size = 3, alpha = 1, arrow =
                 arrow(length = unit(0.05, "inches"), ends = "last", type = "closed")) +
  geom_segment(data = df[["2"]], aes(x = X, y = Y,
                                     xend = endX, yend = endY,
                                     colour = "#41ab5d"),
               lineend = "butt", size = 0.7, alpha = 0.2, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  geom_segment(data = df2, aes(x = X, y = Y,
                               xend = endX, yend = endY,
                               colour = "#41ab5d"),
               lineend = "butt", size = 3, alpha = 1, arrow =
                 arrow(length = unit(0.05, "inches"), ends = "last", type = "closed")) +
  geom_segment(data = df[["3"]], aes(x = X, y = Y,
                                     xend = endX, yend = endY,
                                     colour = "#2171b5"),
               lineend = "butt", size = 0.7, alpha = 0.2, arrow =
                 arrow(length = unit(0.04, "inches"), ends = "last", type = "open")) +
  geom_segment(data = df3, aes(x = X, y = Y,
                               xend = endX, yend = endY,
                               colour = "#2171b5"),
               lineend = "butt", size = 3, alpha = 1, arrow =
                 arrow(length = unit(0.05, "inches"), ends = "last", type = "closed")) +
  scale_colour_manual(values = c("#88419d", "#41ab5d", "#2171b5")) +
  labs(title = "Paul Pogba - Top 3 Progressive Pass Clusters",
       subtitle = "Premier League Games [2020/21]") +
  custom_theme() +
  theme(legend.position = "none")
figure

ggdraw(figure, xlim = c(0,1), ylim = c(0,1))+
  draw_line(x = c(0.4, 0.6),
            y = c(0.06, 0.06),
            color = "white", size = 1.2,
            arrow = arrow(length = unit(0.12, "inches"), type = "closed"))+
  draw_text("Attacking Direction", x=0.5, y=0.035, colour = "white", size = 12)


setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents/Python Scripts")
ggsave("jon prog.png", bg = "#0D0D0D", width = 1850, height = 1100, units = "px")
