## Replication script (R) for the paper entitled 
#  Evaluating the Effectiveness of Intergovernmental Earmarking
#  Grants: Insights from the German Higher-Education Pact
#  by Salvatore Barbaro
#  State and Local Government Review 2024
###################################################################
## Here: Entrants in STEM (in German: MINT-faculties) (Trend analysis and Regressions)
library(wiesbaden)  # access to the data of the German statistical office
library(dplyr)      # obvious
library(ggplot2)    # obvious
library(readxl)     # We need to import an excel-sheet
library(tidyr)      # Needed for the function pivot_longer
library(trend)      # For model prediction and trend estimate (MK-Test)
library(zyp)        # For MK-Test as MannKendall()

NAT <- retrieve_valuelabel(variablename="NAT", genesis=c(db="de") ) # Nationality
metadata <- retrieve_valuelabel(variablename="BILSF1", 
                                genesis=c(db="de") )  
laender  <- retrieve_valuelabel(variablename="DLAND", 
                                genesis=c(db="de") ) %>%
  rename(Name = description) %>%
  mutate(State = c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BR", "MV", "SN", "ST", "TH"))
fachgruppen <- read_excel("FG.xlsx") %>% mutate(BILSF1 = paste0("SF",SF), .before = "SF") %>%
  select(., c("BILSF1", "Area"))  # The excel sheet indicates which study area 
                                  # belongs to STEM (MINT), Humanities, Social Sciences
######################################################################
## Get Data on Entrants
sta <- wiesbaden::retrieve_data(tablename = "21311LS108", 
                                genesis=c(db="de") ) ## total data
sta.df <- sta %>% left_join(x = ., y = fachgruppen, by = "BILSF1") %>%
  left_join(x = ., y = laender, by = "DLAND")  %>%
  select(., -c("id21311", "BIL016_qual")) # merged with area-data and with data on States
#
## Because in this part of the evaluation we are not interested
# in differences among states, and faculties, we group them
sta.g.df <- sta.df %>% group_by(DLAND, BILSF1, SEMEST, Area, State) %>% 
  summarise(value = sum(BIL016_val)) %>% 
  group_by(Area, SEMEST) %>% reframe(value = sum(value, na.rm = T))
#########################################################################
## Restrict Data to Mint
sta.short <- sta.g.df %>% group_by(SEMEST) %>% 
  reframe(sum = sum(value, na.rm= T))
sta.long <- sta.g.df %>% left_join(x = ., y = sta.short, by = "SEMEST") %>%
  mutate(share = value / sum)
sta.mint <- sta.long %>% filter(., Area == "MINT") %>%
  mutate(Year = 1998:2022) %>%
  mutate(trend = predict(loess(share ~ Year))) %>%
  select(., c("share", "trend", "Year")) 
sta.mint.pivot <- sta.mint %>%  
  pivot_longer(data = ., cols = c("share", "trend"))
#
sta.mint.hsp <- sta.mint %>% filter(., Year %in% 2007:2020) %>% 
  select(., -c("trend")) %>% mutate(trend = predict(lm(share ~ Year))) %>%
  pivot_longer(data = ., cols = c("share", "trend"))

sta.mint.before <- sta.mint %>% filter(., Year %in% 1998:2006) %>% 
  select(., -c("trend")) %>% mutate(trend = predict(lm(share ~ Year))) %>%
  pivot_longer(data = ., cols = c("share", "trend"))
########################################################
# Mann-Kendall-Test
# For the entire time span:
mk01 <- mk.test(sta.mint$share, alternative = "two.sided", continuity = T)
## p-value = 0.006 --> there is a trend
df02 <- sta.mint %>% filter(., Year %in% 2007:2020) %>% 
  select(., -c("trend")) %>% mutate(trend = predict(lm(share ~ Year)))
mk02 <- mk.test(df02$share, alternative = "two.sided", continuity = T)
df03 <- sta.mint %>% filter(., Year %in% 1998:2006) %>% 
  select(., -c("trend")) %>% mutate(trend = predict(lm(share ~ Year)))
mk03 <- mk.test(df03$share, alternative = "two.sided", continuity = F)
mk03
MK01 <- MannKendall(sta.mint$share)
MK02 <- MannKendall(df02$share)
MK03 <- MannKendall(df03$share)
########################################################
ggplot(data = sta.mint.pivot, 
       aes(x = Year, y = value, group = name , colour = name)) + 
  geom_line() +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  geom_line(data = sta.mint.hsp %>% filter(., name == "trend"),
            aes(x = Year, y = value), col = "steelblue") +
  geom_line(data = sta.mint.before %>% filter(., name == "trend"),
            aes(x = Year, y = value), col = "steelblue") +
  scale_color_viridis_d(end = 0.7) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "STEM shares", x = "Year", col = "variables") +
  theme_gray(base_size = 22) + 
  theme(legend.position = "inside", legend.position.inside  = c(0.8, 0.2))
ggsave("stem.pdf", width = 16, height = 9)

#### Regressions
semyear.df <- data.frame(SEMEST = sort(unique(sta.df$SEMEST)), 
                         Year = 1998:2022)
sta2.df <- sta.df %>% group_by(State, Area, SEMEST) %>% 
  reframe(value = sum(BIL016_val, na.rm = T)) %>% filter(., is.na(Area) == F) %>%
  left_join(x = ., y = semyear.df, by = "SEMEST") %>% select(., -c("SEMEST")) #%>%

total.sta <- sta2.df %>% group_by(Year, State) %>% reframe(total = sum(value, na.rm = T))
#
sta3.df <- sta2.df %>% left_join(x = ., y = total.sta, by = c("Year", "State")) %>%
  mutate(humanity.share = ifelse(Area == "Humanities", value / total, NA),
         mint.share =     ifelse(Area == "MINT", value/total, NA),
         social.share =   ifelse(Area == "Social", value/total, NA))

sta4.df <- sta3.df %>% filter(., Year %in% c(2010, 2015, 2020)) %>% 
  filter(., !State == "TH") 
sta4.hum <- sta4.df %>% filter(., Area == "Humanities")
sta4.min <- sta4.df %>% filter(., Area == "MINT")
sta4.soc <- sta4.df %>% filter(., Area == "Social")
share.tb <- data.frame(Year = sta4.soc$Year,
                       State = sta4.soc$State,
                       soc.share = sta4.soc$social.share,
                       mint.share = sta4.min$mint.share,
                       hum.share = sta4.hum$humanity.share)
regtab.fac <- data.frame(State = rep(unique(sta4.df$State), 3),
                         Year  = rep(c(2010, 2015, 2020), each = 15) ) %>%
  left_join(x = ., y = share.tb, by = c("Year", "State"))


