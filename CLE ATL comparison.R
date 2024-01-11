library(tidyverse)
library(Lahman)
library(ggrepel)

#Grab players for ATL 1995
braves_95_offense <- Batting %>% 
  filter(yearID == 1995 & teamID == 'ATL') %>% 
  select(playerID, yearID, teamID, R, AB, H, X2B, X3B, HR, BB, SO, SB, CS, HBP, SF) %>% 
  mutate(BA = H/AB, OBP = (H + BB + HBP)/(AB + BB + HBP + SF)) %>% 
  mutate(singles = H - X2B- X3B - HR) %>% 
  mutate(SLG = (singles + 2*X2B + 3*X3B + 4*HR)/(AB)) %>% 
  mutate(OPS = OBP + SLG)

#Replace NaN values
braves_95_offense$BA[is.nan(braves_95_offense$BA)] <- 0
braves_95_offense$OBP[is.nan(braves_95_offense$OBP)] <- 0
braves_95_offense$SLG[is.nan(braves_95_offense$SLG)] <- 0
braves_95_offense$OPS[is.nan(braves_95_offense$OPS)] <- 0

#Rounding
braves_95_offense <- braves_95_offense %>% 
  mutate(BA = round(BA, digits = 3)) %>% 
  mutate(OBP = round(OBP, digits = 3)) %>% 
  mutate(SLG = round(SLG, digits = 3)) %>% 
  mutate(OPS = round(OPS, digits = 3))

#Following code brings in player names from People and places at front of table

player_names <- People %>% 
  select(playerID, nameFirst, nameLast)
braves_95_offense <- merge(braves_95_offense, player_names, by = 'playerID')

#Created nameLast, nameFirst columns. Needed to position new columns at front of table.

braves_95_offense <- braves_95_offense[, c("nameLast", names(braves_95_offense)[names(braves_95_offense) != "nameLast"])]
braves_95_offense <- braves_95_offense[, c("nameFirst", names(braves_95_offense)[names(braves_95_offense) != "nameFirst"])]


#Filter out players with AB < 50
braves_95_offense <- braves_95_offense %>% 
  filter(AB >= 50)

#Scatter Plot of CLE and ATL hitters? labelled as CLE or ATL?
#Grab players for CLE 1995
cle_95_offense <- Batting %>% 
  filter(yearID == 1995 & teamID == 'CLE') %>% 
  select(playerID, yearID, teamID, R, AB, H, X2B, X3B, HR, BB, SO, SB, CS, HBP, SF) %>% 
  mutate(BA = H/AB, OBP = (H + BB + HBP)/(AB + BB + HBP + SF)) %>% 
  mutate(singles = H - X2B- X3B - HR) %>% 
  mutate(SLG = (singles + 2*X2B + 3*X3B + 4*HR)/(AB)) %>% 
  mutate(OPS = OBP + SLG)

#Replace NaN values - CLE
cle_95_offense$BA[is.nan(cle_95_offense$BA)] <- 0
cle_95_offense$OBP[is.nan(cle_95_offense$OBP)] <- 0
cle_95_offense$SLG[is.nan(cle_95_offense$SLG)] <- 0
cle_95_offense$OPS[is.nan(cle_95_offense$OPS)] <- 0

#Rounding
cle_95_offense <- cle_95_offense %>% 
  mutate(BA = round(BA, digits = 3)) %>% 
  mutate(OBP = round(OBP, digits = 3)) %>% 
  mutate(SLG = round(SLG, digits = 3)) %>% 
  mutate(OPS = round(OPS, digits = 3))

#Following code brings in player names from People and places at front of table - CLE

player_names <- People %>% 
  select(playerID, nameFirst, nameLast)
cle_95_offense <- merge(cle_95_offense, player_names, by = 'playerID')

#Created nameLast, nameFirst columns. Needed to position new columns at front of table.

cle_95_offense <- cle_95_offense[, c("nameLast", names(cle_95_offense)[names(cle_95_offense) != "nameLast"])]
cle_95_offense <- cle_95_offense[, c("nameFirst", names(cle_95_offense)[names(cle_95_offense) != "nameFirst"])]

#Filter out players with AB < 50
cle_95_offense <- cle_95_offense %>% 
  filter(AB >= 50)

#Bind ATL and CLE players together
cle_atl_offense <- rbind(cle_95_offense, braves_95_offense)

#Scatter plot of BA and OPS
ggplot(data = cle_atl_offense, mapping = aes( x = BA, y = OPS, color = teamID)) +
  geom_point() +
  geom_text_repel(color = "blue3", 
                  aes(label = paste(nameLast)))
