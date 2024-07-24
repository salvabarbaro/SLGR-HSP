## Replication script (R) for the paper entitled 
# Evaluating the Effectiveness of Intergovernmental Earmarking Grants:
# Insights from the German Higher-Education Pact
# by Salvatore Barbaro
# Published in State and Local Government Review



## This part:  Advising Relationship
library(wiesbaden)  # access to the data of the German statistical office
library(dplyr)      # obvious
library(ggplot2)    # obvious
library(readxl)     # We need to import an excel-sheet
library(tidyr)      # Needed for the function pivot_longer
library(trend)      # For model prediction and trend estimate (MK-Test)
####################################################################
## credentials to include here
#####################################################################
overview <- wiesbaden::retrieve_datalist(tableseries="21354*", 
                                     genesis=c(db="de") )
##
advrel <- retrieve_data(tablename = "21354BJ004",
                                   genesis = c(db = "de") )

laender  <- retrieve_valuelabel(variablename="DLAND", 
                                genesis=c(db="de") ) %>%
  rename(State = description)

advrel.df <- advrel %>% left_join(x = ., y = laender, by = "DLAND") %>%
  select(., -c("id21354", "BIL003_qual")) %>% rename(value = BIL003_val)

###########
#load(url("https://zenodo.org/record/5783562/files/hsp.RData?download=1")) ## see dataset description
costs <- retrieve_data(tablename = "21371BJ0001",
                        genesis = c(db = "de") )
### UNI VS UAS
overview <- wiesbaden::retrieve_datalist(tableseries="2137*", 
                                         genesis=c(db="de") )
# 21371BJ002
costs <- retrieve_data(tablename = "21371BJ002",
                       genesis = c(db = "de") )
hsart  <- retrieve_valuelabel(variablename="BILHA2", 
                                genesis=c(db="de") )
varinfo <- retrieve_varinfo(variablename = "ASG026", genesis=c(db="de"))
costs.df <- costs %>% 
  left_join(x = ., y = hsart, by = "BILHA2") %>%
  select(., c("BILHA2", "JAHR", "ASG026_val", "description")) %>%
  rename(Type = BILHA2, Year = JAHR, value = ASG026_val)

ggplot(data = costs.df,
       aes(x = Year, y = value, colour = Type, group = Type)) +
  geom_line()

costs22 <- costs.df %>% filter(., Type %in% c("HSLART011", 
                                              "HSLART08", 
                                              "HSLART082"), 
                               Year == 2022) %>%
  mutate(Typ2 = c("Uni", "UAS", "UAS")) %>% 
  group_by(Typ2) %>% 
  reframe(costs = sum(value)) %>%
  mutate(studws22 = c((1095425 + 59639), 1699938)) %>%
  mutate(cpc = costs * 1000 / studws22)



