library(juggerdata)
library(dplyr)

# prepare
JTRHomePlay <- JTR.jtr %>% mutate(Home = TournamentCity == TeamCity & TournamentCountry == TeamCountry, relRank = (Rank - 1) / (nParticipants - 1))

# first identify teams which had at least one home-tournament and at least one out-tournament
JTRHomeTeams <- JTRHomePlay %>%
  group_by(TeamID, TeamName, TeamCity, TeamCountry, nParticipations) %>%
  summarise(nHome = sum(Home), nNonHome = sum(!Home),
            relRankHome = mean(relRank[Home]), relRankNonHome = mean(relRank[!Home]),
            HomeGain = relRankNonHome - relRankHome) %>%
  filter(nNonHome > 0, nHome > 0) %>%
  arrange(desc(nParticipations))

## reduce the JTR dataset to those teams (Note high number of participations - most teams also host tournaments)
JTRHomePlay <- JTRHomePlay %>% filter(TeamID %in% JTRHomeTeams$TeamID)

# tests
# hist(JTRHomeTeams$HomeGain)
wilcox.test(JTRHomeTeams$HomeGain, alternative = "greater")

# hist((JTRHomeTeams %>% filter(nHome > 1, nNonHome > 2))$HomeGain)
wilcox.test((JTRHomeTeams %>% filter(nHome > 1, nNonHome > 2))$HomeGain, alternative = "greater")

JTRHomeTeams %>% filter(nHome > 1, nNonHome > 2) %>% arrange(desc(HomeGain))

## cool vis
library(ggplot2)

ggplot(JTRHomeTeams %>% filter(nHome > 1, nNonHome > 2)) +
  geom_histogram(aes(x = HomeGain)) +
  theme_minimal()

ggplot(JTRHomeTeams %>% filter(nHome > 1, nNonHome > 2), aes(x = reorder(TeamName, HomeGain), y = HomeGain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  xlab(label = "")
