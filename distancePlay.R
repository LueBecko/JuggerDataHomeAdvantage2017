library(juggerdata)
library(dplyr)
library(geosphere)
library(ggplot2)

# prepare
JTRDistance <- JTR.jtr %>% mutate(Home = TournamentCity == TeamCity & TournamentCountry == TeamCountry,
                                  relRank = (Rank - 1) / (nParticipants - 1),
                                  geoDist = distGeo(p1 = cbind(TeamLongitude, TeamLatitude), p2 = cbind(TournamentLongitude, TournamentLatitude)),
                                  Season = 1900 + as.POSIXlt(TournamentStart)$year ) %>%
    filter(!is.na(geoDist)) # keep only those lines that contain valid geo data

# keep only teams with more than 3 participations within a season
JTRDistance <- JTRDistance %>% 
  group_by(TeamID, Season) %>%
  filter(n() > 3)
  
# rank correlation of a teams participations results and traveling distance
JTRDistanceResults <- JTRDistance %>% 
  group_by(TeamID, TeamName, Season) %>%
  summarise(rDistRank = cor(x = geoDist, y = relRank, method = "spearman"),
            pDistRank = cor.test(x = geoDist, y = relRank, method = "spearman")$p.value,
            nParticipations = n())
# Two warnings: Rigor Mortis 2011 and 2012 were first place in all of their away tournaments (2011 4, 2012 7), so no coeefficient could be computed for them

# stats
mean(JTRDistanceResults$rDistRank, na.rm = TRUE) # = 0.0752394

ggplot(JTRDistanceResults) + geom_histogram(aes(x = rDistRank), bins = 15) +
  theme_minimal() # optical inspection -> positive effect might be possible

wilcox.test(JTRDistanceResults$rDistRank, alternative = "greater", na.rm = TRUE)

ggplot(JTRDistanceResults) + geom_histogram(aes(x = rDistRank), bins = 15) +
  facet_wrap(~ Season) +
  theme_minimal() # optical inspection -> it changes with time

# z transform r?
JTRDistanceResults %>% group_by(Season) %>% summarise(pWilcox = wilcox.test(rDistRank, alternative = "greater", na.rm = TRUE)$p.value) %>%
  mutate(sig = pWilcox < 0.05)

# individual slope
JTRDistanceSlope <- JTRDistance %>%
  group_by(Season, TeamName) %>%
  do(lm.model = lm(relRank ~ geoDist, data = .))

JTRDistanceSlope <- JTRDistanceSlope %>%
  summarise(Season = first(Season), TeamName = first(TeamName),
            slope = summary(lm.model)$coefficient[2,1],
            p.value = summary(lm.model)$coefficient[2,4])

JTRDistanceSlope %>% filter(p.value < 0.05) %>% arrange(slope)

t.test(JTRDistanceSlope$slope)
t.test((JTRDistanceSlope %>% filter(p.value < 0.05))$slope)


# reisemeister
JTRReisemeister <- JTRDistance %>% group_by(TeamID, TeamName, Season) %>%
  summarise(totalDistance = sum(geoDist), nParticipations = n()) %>%
  arrange(Season, desc(totalDistance))

for (s in unique(JTRReisemeister$Season)) {
  ggplot(JTRReisemeister %>% filter(Season == s), aes(x = reorder(TeamName, totalDistance), y = totalDistance / 1000)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    # facet_wrap(~ Season, nrow = 1) +
    theme_minimal() +
    theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
    xlab(label = "") + ylab(label = paste("Reise-Distanz (km) im Jahr", toString(s), sep = " "))
  
  ggplot(JTRDistance %>% mutate(totalDistance = sum(geoDist)) %>% filter(Season == s), aes(x = reorder(TeamName, totalDistance), y = geoDist / 1000, fill = TournamentID)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    # facet_wrap(~ Season, nrow = 1) +
    theme_minimal() + guides(fill = FALSE) +
    theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
    xlab(label = "") + ylab(label = paste("Reise-Distanz (km) im Jahr", toString(s), sep = " "))
}
