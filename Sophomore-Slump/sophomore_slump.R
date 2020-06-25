## ----------------------------------------------------------------------------
## Personal Work - Sophomore Slump?
## Twitter Chart turned Article
## Christopher Gauthier - 02/06/2020
## ----------------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(readxl)

# PNG 900 x 550

# Read in hitter and pitcher data
hitters <- read_excel("/Users/chrisgauthier/ChrisDocs/Data Work/New Data Projects/Proj2_SophomoreSlump/Rookie2Sophomore.xlsx", sheet = 1)
pitchers <- read_excel("/Users/chrisgauthier/ChrisDocs/Data Work/New Data Projects/Proj2_SophomoreSlump/Rookie2Sophomore.xlsx", sheet = 2)

# Create change line plots
OPSplot <- ggplot(hitters, aes(x = Season, y = OPSplus, group = Player, color = Player)) +
  geom_point(stat = "summary", fun.y = sum) +
  stat_summary(fun.y = sum, geom = "line") + 
  ggtitle("Change in OPS+ from Rookie to Sophomore Season, ROTY Winners Since 2010") +
  ylab("OPS+")

ERAplot <- ggplot(pitchers, aes(x = Season, y = ERAplus, group = Player, color = Player)) +
  geom_point(stat = "summary", fun.y = sum) +
  stat_summary(fun.y = sum, geom = "line") + 
  ggtitle("Change in ERA+ from Rookie to Sophomore Season, ROTY Winners Since 2010") +
  ylab("ERA+")

# Create new metrics
hitters <- hitters %>%
  mutate(bWARper100 = (bWAR * 100)/PA)

pitchers <- pitchers %>%
  mutate(bWARper100 = (bWAR * 100)/BF)

# Create change line plots (WAR)
WARperPAplot <- ggplot(hitters, aes(x = Season, y = bWARper100, group = Player, color = Player)) +
  geom_point(stat = "summary", fun.y = sum) +
  stat_summary(fun.y = sum, geom = "line") + 
  ggtitle("Change in WAR per 100 PA from Rookie to Sophomore Season, ROTY Winners Since 2010") +
  ylab("WAR/100 PA")

WARperBFplot <- ggplot(pitchers, aes(x = Season, y = bWARper100, group = Player, color = Player)) +
  geom_point(stat = "summary", fun.y = sum) +
  stat_summary(fun.y = sum, geom = "line") + 
  ggtitle("Change in WAR per 100 BF from Rookie to Sophomore Season, ROTY Winners Since 2010") +
  ylab("WAR/100 BF")

# Average Hitter Triple Slash
averageTripleSlash <- hitters %>%
  group_by(Season) %>%
  summarize(AVG = sum(H)/sum(AB),
            OBP = (sum(H) + sum(BB) + sum(HBP))/(sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
            SLG = sum(TB)/sum(AB))

averagePitcherStats <- pitchers %>%
  group_by(Season) %>%
  summarize(ERA = sum(ER)/sum(IP) * 9,
            Kperc = sum(SO)/sum(BF),
            BBperc = sum(BB)/sum(BF))
