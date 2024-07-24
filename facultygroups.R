## Replication script (R) for the paper entitled 
# Evaluating the Effectiveness of Intergovernmental Earmarking Grants:
# Insights from the German Higher-Education Pact
# by Salvatore Barbaro
# Published in State and Local Government Review



## This part:  Mann-Kendall Tests (STEM)
library(wiesbaden)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)


stud <- wiesbaden::retrieve_datalist(tableseries="21311*", 
                                     genesis=c(db="de") )
#meta.df <- retrieve_metadata(tablename = "21311LS006", genesis=c(db="de") )
#varinfo <- retrieve_varinfo(variablename = "BILSF1", genesis=c(db="de"))
metadata <- retrieve_valuelabel(variablename="BILSF1", genesis=c(db="de") )
laender  <- retrieve_valuelabel(variablename="DLAND", genesis=c(db="de") ) %>%
  rename(State = description)
fachgruppen <- read_excel("FG") %>% mutate(BILSF1 = paste0("SF",SF), .before = "SF") %>%
  select(., c("BILSF1", "Area"))
stud.df <- wiesbaden::retrieve_data(tablename = "21311LS006", 
                                    genesis=c(db="de") ) %>%
  left_join(x = ., y = fachgruppen, by = "BILSF1") %>%
  left_join(x = ., y = laender, by = "DLAND")  %>%
  select(., -c("id21311", "BIL002_qual")) 

stud.g.df <- stud.df %>% group_by(DLAND, BILSF1, SEMEST, Area, State) %>% 
  summarise(value = sum(BIL002_val)) %>% 
  group_by(Area, SEMEST) %>% reframe(value = sum(value, na.rm = T))

ggplot(data = stud.g.df, 
       aes(x = SEMEST, y = value, group = Area, colour = Area)) +
  geom_line()


NAT <- retrieve_valuelabel(variablename="NAT", genesis=c(db="de") ) # Nationalität
sta <- wiesbaden::retrieve_data(tablename = "21311LS108", 
                                    genesis=c(db="de") ) 

sta.df <- sta %>% left_join(x = ., y = fachgruppen, by = "BILSF1") %>%
  left_join(x = ., y = laender, by = "DLAND")  %>%
  select(., -c("id21311", "BIL016_qual")) 
#
sta.g.df <- sta.df %>% group_by(DLAND, BILSF1, SEMEST, Area, State) %>% 
  summarise(value = sum(BIL016_val)) %>% 
  group_by(Area, SEMEST) %>% reframe(value = sum(value, na.rm = T))
#
ggplot(data = sta.g.df, 
       aes(x = SEMEST, y = value, group = Area, colour = Area)) +
  geom_line()


sta.short <- sta.g.df %>% group_by(SEMEST) %>% 
  reframe(sum = sum(value, na.rm= T))
sta.long <- sta.g.df %>% left_join(x = ., y = sta.short, by = "SEMEST") %>%
  mutate(share = value / sum)
sta.mint <- sta.long %>% filter(., Area == "MINT") %>%
  mutate(Year = 1998:2022) %>%
  mutate(trend = predict(loess(share ~ Year))) %>%
  select(., c("share", "trend", "Year")) 
sta.mint.pivot <- sta.mint %>%  pivot_longer(data = ., cols = c("share", "trend"))
#
sta.mint.hsp <- sta.mint %>% filter(., Year %in% 2007:2020) %>% 
  select(., -c("trend")) %>% mutate(trend.hsp = predict(lm(share ~ Year))) %>%
  pivot_longer(data = ., cols = c("share", "trend.hsp"))

sta.mint.before <- sta.mint %>% filter(., Year %in% 1998:2006) %>% 
  select(., -c("trend")) %>% mutate(trend.hsp = predict(lm(share ~ Year))) %>%
  pivot_longer(data = ., cols = c("share", "trend.hsp"))
  


  #mutate(trend = predict(loess(sta.mint$share ~ sta.mint$SEMEST)))
ggplot(data = sta.mint.pivot, 
       aes(x = Year, y = value, group = name , colour = name)) + 
  geom_line() +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  geom_line(data = sta.mint.hsp %>% filter(., name == "trend.hsp"),
            aes(x = Year, y = value), col = "steelblue") +
  geom_line(data = sta.mint.before %>% filter(., name == "trend.hsp"),
            aes(x = Year, y = value), col = "forestgreen") 
  




MannKendall(share)
cor.test(x=1998:2022,y=share, meth="kendall", continuity = TRUE)
mk.test(sta.mint$share, alternative = "two.sided", continuity = TRUE)
before.share <- sta.mint.before %>% filter(., name == "share")
mk.test(before.share$value, alternative = "two.sided", continuity = T)


