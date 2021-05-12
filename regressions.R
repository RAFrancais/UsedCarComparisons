library(ggplot2)
library(dplyr)
library(Cairo)
library(gtsummary)

options(scipen = 999999)


##Regression analysis 
cartotal <- read.csv("D:/RLearning/usedcarcomp/cartotal.csv", header = TRUE, sep = ",")
cartotal$perthousand_mileage <- cartotal$mileage / 1000

#separating by car
xbreg <- subset(cartotal, car_id == "1")
tcreg <- subset(cartotal, car_id == "2")
avalonreg <- subset(cartotal, car_id == "3")

#removing any potential NULL rows, I was having a problem with this earlier, 
#so this is more of a prophylactic function than solving an actual problem

xbreg <- na.omit(xbreg)
tcreg <- na.omit(tcreg)
avalonreg <- na.omit(avalonreg)


##Turning years to factors for regression
xbreg$year <- as.factor(xbreg$year)
tcreg$year <- as.factor(tcreg$year)
avalonreg$year <- as.factor(avalonreg$year)


##xB regression

lmxb <- lm(price ~ perthousand_mileage + year, data = xbreg)
summary(lmxb)


xbreg$predicted <- predict(lmxb)
xbreg$residuals <- residuals(lmxb)

##tC regression

lmtc <- lm(price ~ perthousand_mileage + year, data = tcreg)
summary(lmtc)


tcreg$predicted <- predict(lmtc)
tcreg$residuals <- residuals(lmtc)

##avalon regression

lmavalon <- lm(price ~ perthousand_mileage + year, data = avalonreg)
summary(lmavalon)


avalonreg$predicted <- predict(lmavalon)
avalonreg$residuals <- residuals(lmavalon)



####gtsummary stuff
####summary statistics
cartotalsumm <- cartotal[c("price", "mileage", "car_id")]

summarystats <- tbl_summary(cartotalsumm, by = car_id, statistic = list(all_continuous() ~ "{mean}, {median}, ({sd})")) %>% 
  modify_header(list(
  stat_1 ~ "**Scion xB**, N = {n}",
  stat_2 ~ "**Scion tC**, N = {n}",
  stat_3 ~ "**Toyota Avalon**, N = {n}"
))



summarystats %>%
  as_gt() %>%
  gt::gtsave(filename = "D:/RLearning/usedcarcomp/summarystats.html")


#Regression tables
xbt <- lmxb %>% tbl_regression(intercept = TRUE) %>% 
  bold_labels() %>% 
  add_significance_stars(hide_p = FALSE)
 

tct <- lmtc %>% tbl_regression(intercept = TRUE) %>% 
  bold_labels() %>%
  add_significance_stars(hide_p = FALSE)
  

avalont <- lmavalon %>% tbl_regression(intercept = TRUE) %>% 
  bold_labels() %>%
  add_significance_stars(hide_p = FALSE)
 


regtable <- tbl_merge(
  tbls = list(xbt, tct, avalont),
  tab_spanner = c("**Scion xB**", "**Scion tC**", "**Toyota Avalon**"))

regtable %>%
  as_gt() %>%
  gt::gtsave(filename = "D:/RLearning/usedcarcomp/regtable.html")

#saving xbreg csv for Shiny App
write.csv(xbreg, "D:/RLearning/usedcarcomp/xbreg.csv", row.names = FALSE)

