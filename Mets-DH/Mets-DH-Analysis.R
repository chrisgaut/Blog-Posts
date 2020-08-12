## "Will the Mets Benefit from the New DH Rule?" Analysis
## Christopher Gauthier - July 10, 2020

# Load libraries
library(tidyverse)
library(rvest)
library(ggrepel)

# Scrape Yoenis Cespedes Statcast stats from Baseball Savant
savant <- "https://baseballsavant.mlb.com/savant-player/yoenis-cespedes-493316?stats=statcast-r-hitting-mlb"
cespedesSav <- savant %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="statcast_glance_batter"]/table') %>%
  html_table() %>%
  data.frame()

# Plot Yoenis Cespedes xStats
xStatsTrends <- ggplot(cespedesSav[1:4,], aes(x = Season, group = 1)) +
  geom_line(aes(y = XBA), color = "black") + geom_point(aes(y = XBA), color = "black") +
  geom_line(aes(y = XSLG), color = "blue") + geom_point(aes(y = XSLG), color = "blue") +
  geom_line(aes(y = XWOBA), color = "orange") + geom_point(aes(y = XWOBA), color = "orange") +
  ggtitle("Yoenis Cespedes: xStat Trends") +
  xlab("Season") +
  ylab("xStat Values") +
  theme_minimal() +
  geom_text_repel(data = filter(cespedesSav, Season == 2018), aes(y = XBA, label = XBA)) +
  geom_text_repel(data = filter(cespedesSav, Season == 2018), aes(y = XSLG, label = XSLG)) +
  geom_text_repel(data = filter(cespedesSav, Season == 2018), aes(y = XWOBA, label = XWOBA)) +
  annotate(geom = "text", x = .7, y = .54, label = "xSLG", size = 10, color = "blue") + 
  annotate(geom = "text", x = .7, y = .37, label = "xwOBA", size = 10, color = "orange") +
  annotate(geom = "text", x = .7, y = .3, label = "xBA", size = 10, color = "black")
xStatsTrends

# Load 2019 NL Pitchers' Hitting Stats from FanGraphs
pitchersFG <- read_csv("FanGraphs Leaderboard.csv")

# Analyze difference in 2019 staff wOBA and 2020 projected DH wOBA
wOBAs <- pitchersFG %>%
  select(Team, wOBA) %>%
  mutate(DHwOBA = c(.319, .340, .321, .335, .324, .342, .308, .326, .327, .341, .330, .328, .317, .333, .332),
         increase = DHwOBA - wOBA,
         Mets = ifelse(Team == "Mets", T, F))

# Plot wOBA differences
wOBAdiff <- ggplot(wOBAs, aes(x = reorder(Team, increase), y = increase)) +
  geom_bar(aes(fill = Mets), stat = "identity") +
  xlab("Team") +
  ylab("Increase in wOBA After Replacement") +
  ggtitle("Teams' Projected wOBA Increases After Replacing P Spot with DH Spot") + 
  theme_minimal() +
  scale_fill_manual(values = c('grey', 'blue')) +
  theme(legend.position = "none")
wOBAdiff
