## Replication script (R) for the paper entitled 
#  Evaluating the Effectiveness of Intergovernmental Earmarking
#  Grants: Insights from the German Higher-Education Pact
#  by Salvatore Barbaro
#  State and Local Government Review 2024
###################################################################
## Here: Regression analysis
library(dplyr)
library(plm)
library(texreg)
library(ggplot2)
library(ggpubr)

load("regr.RData")
###############################
df <- regtabA %>% left_join(x = ., y = regtab.fac, by = c("State", "Year")) %>%
  left_join(x = ., y = adv.regtab, by = c("State", "Year")) %>%
  left_join(x = ., y = vza.regtab, by = c("State", "Year")) %>%
  left_join(x = ., y = bumi.reg, by = c("State", "Year"))

# Base model
model01 <- arrc ~  UNI + hum.share + vzrc + grants.pc
# reg01: simple OLS without FE
reg01 <- lm(formula = model01, data = df)
summary(reg01)
# reg02: Time FE OLS
reg02 <- plm(formula = model01, data = df, index= c("Year", "State"), 
             effect = "time", model = "within")
summary(reg02)
fixef(reg02)
# reg03: Two-way FE OLS
reg03 <- plm(formula = model01, data = df, 
             index = c("Year", "State"), model = "within",
             effect = "twoways")
summary(reg03)
#################################
model02 <- arrc ~  UNI + vzrc + grants.pc
####
# reg 04: Time FE with model 02
reg04 <- plm(formula = model02, data = df, 
             index= c("Year", "State"), 
             effect = "time", model = "within")
summary(reg04)
###############################################
# model 03
model03 <- arrc ~  UNI + hum.share +  grants.pc
reg05 <- plm(formula = model03, data = df, 
             index= c("Year", "State"), 
             effect = "time", model = "within")
summary(reg05)
################################################
reg.list <- list(reg02, reg03, reg04, reg05)
texreg(l = reg.list,
       file = "olsregfull.tex",
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = c("Time FE", "Two-way FE", "Model 02", "Model 03"),
       custom.coef.names = c("University", "Humanities", "Staff expansion", "Fed. grants"),
       digits = 3, 
       leading.zero = TRUE,
       caption = "OLS regression results"
)



plotreg(l = reg.list,
       file = "olsregfull.pdf",
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = c("Time FE", "Two-way FE", "Model 02", "Model 03"),
       digits = 3, 
       leading.zero = TRUE, 
       ci.level = 0.9
)



var.list <- list("UNI", "hum.share", "vzrc", "grants.pc")
plotfun <- function(v){
ggplot(data = df,
       aes(y = arrc, x = get(v), colour = as.factor(Year), group = as.factor(Year))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
    theme_gray(base_size = 22) +
    labs(y = "\u0394 Advising Relationship", x = v, col = "Year") +
    scale_colour_viridis_d(end = 0.7)
}
plots <- lapply(var.list, plotfun)
all.plots <- ggarrange(plotlist = plots, ncol = 2, nrow = 2, common.legend = T)
all.plots



ggplot(data = df, aes(x = mint.share, y = arrc, colour = as.factor(Year), group = as.factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", se = T)






