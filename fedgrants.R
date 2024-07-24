## Replication script (R) for the paper entitled 
# Evaluating the Effectiveness of Intergovernmental Earmarking Grants:
# Insights from the German Higher-Education Pact
# by Salvatore Barbaro
# Published in State and Local Government Review



## This part:  Distribution of Federal Grants
library(readxl)
library(dplyr)
library(wiesbaden)
library(ggplot2)

BUMI <- read_excel("BUMI.xlsx") %>%
  setNames(c("State", paste0("y",2007:2020), "FedGrants10", "FedGrants15", "FedGrants20"))
## Get Population data
# 12211LJ019 Bevölkerung (ab 15 Jahren), Bundesländer, Jahr
pop.big <- wiesbaden::retrieve_data(tablename = "12211LJ019", 
                                  genesis=c(db="de") ) ## total data
#
laender  <- retrieve_valuelabel(variablename="DLAND", 
                                genesis=c(db="de") ) %>%
  rename(Name = description) %>%
  mutate(State = c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BR", "MV", "SN", "ST", "TH"))
#
pop.df <- pop.big %>% select(., -c("id12211", "BEV071_qual")) %>%
  left_join(x = ., y = laender, by = "DLAND") %>%
  filter(., JAHR %in% c(2010, 2015, 2019)) %>%
  select(., c("JAHR", "BEV071_val", "State")) %>% 
  mutate(Year = ifelse(JAHR == 2019, 2020, JAHR)) %>%
  select(., -c("JAHR"))

bumi.reg <- BUMI %>% select(., !starts_with("y")) %>% 
  filter(., !State == "TH") %>%
  pivot_longer(data = ., values_to = "fedgrants", cols = starts_with("Fed") ) %>%
  mutate(Year = rep(c(2010, 2015, 2020), 15 )) %>% select(., -c("name")) %>%
  left_join(x = ., y = pop.df, by = c("State", "Year")) %>%
  mutate(grants.pc = fedgrants / BEV071_val)

zsta <- data.frame(State = c("BW", "BY", "BE", "BR", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH"),
                   zsta20 = c(213102, 279547, 150819, 18314, 19557, 65621, 
                              136476, 9182, 122474, 437951, 64162, 20842, 10074, 14785, 32775, 31298))


fedgrants.zsta <- BUMI %>% select(., !starts_with("y")) %>%
  left_join(x = ., y = zsta, by = "State") %>%
  mutate(bumizsta = FedGrants20 * 1000 / zsta20) %>%
  mutate(state.type = c("W", "W", "C", "E", "C", "C", "W", "E", "W", "W", "W", "W", "E", "E", "W", "E"))

ggplot(data = fedgrants.zsta, 
       aes(x = FedGrants20, y = zsta20)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()+
  geom_smooth(method = "lm", se = F) +
  ggrepel::geom_label_repel(aes(label = State))

avg.value = sum(fedgrants.zsta$FedGrants20 * 1000) / sum(fedgrants.zsta$zsta20)

ggplot(data = fedgrants.zsta,
       aes(x = zsta20, y = bumizsta, colour = state.type)) +
  geom_hline(yintercept = avg.value, linetype = "dashed", col = "red", linewidth = 2, alpha = 0.8) +
  geom_point(size = 5) +
  scale_x_log10(labels = scales::comma) + scale_y_log10(labels = scales::comma) +
  ggrepel::geom_label_repel(aes(label = State), show.legend = F, size = 8) +
  scale_color_viridis_d(begin = 0.3, end = 0.7) +
  theme_gray(base_size = 22) + 
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8)) +
  labs(x = "Additional Entrants", y = "Average federal grant (in EUR)", col = "State Type") 
ggsave("AvgFedGrants.pdf", width = 16, height = 9)

