
library(ggplot2)
library('Cairo')
library(dplyr)
CairoWin()

options(scipen = 999999)


#car totals

load("D:/RLearning/usedcarcomp/xb121720.Rda")
load("D:/RLearning/usedcarcomp/xb012821.Rda")
load("D:/RLearning/usedcarcomp/xb021721.Rda")
load("D:/RLearning/usedcarcomp/tc121720.Rda")
load("D:/RLearning/usedcarcomp/tc012821.Rda")
load("D:/RLearning/usedcarcomp/tc021721.Rda")
load("D:/RLearning/usedcarcomp/avalon_noAT121720.Rda")
load("D:/RLearning/usedcarcomp/avalon012821.Rda")
load("D:/RLearning/usedcarcomp/avalon021721.Rda")

#xb total
xbtotal <- rbind(xb1217, xb0128, xb0217)

#converting January 28ths dates to a standard format
tc0128$date <- as.Date(tc0128$date, format = "%m/%d/%Y")
tctotal <- rbind(tc1217, tc0128, tc0217)

#avalon total
avalontotal <- rbind(avalon0128, avalon0217, avalon1217_noAT)


##dropping dummy variables to rbind all the car sets
##Even though I'm removing dummy variables I created in a previous step, 
##Each model having different years prevents the datasets from being combined.

xbtotal <- subset(xbtotal, select = -c(d2005,d2006,d2007,
                                  d2008,d2009,d2010,
                                  d2011,d2012,d2013,
                                  d2014,d2015
                       )
       )





tctotal <- subset(tctotal, select = -c(d2005,d2006,d2007,
                                                   d2008,d2009,d2010,
                                                   d2011,d2012,d2013,
                                                   d2014,d2015,d2016
                                                  )
                      )


avalontotal <- subset(avalontotal, select = -c(d2005,d2006,d2007,
                                             d2008,d2009,d2010,
                                             d2011,d2012
                                            )
                )






#duplicate removal
xbnoduplicate <- xbtotal[!duplicated(xbtotal[,c('mileage','year','origin')]),]
xbduplicate <- xbtotal[duplicated(xbtotal[,c('mileage','year','origin')]),]

tcnoduplicate <- tctotal[!duplicated(tctotal[,c('mileage','year','origin')]),]
tcduplicate <- tctotal[duplicated(tctotal[,c('mileage','year','origin')]),]

avalonnoduplicate <- avalontotal[!duplicated(avalontotal[,c('mileage','year','origin')]),]
avalonduplicate <- avalontotal[duplicated(avalontotal[,c('mileage','year','origin')]),]



##Generating key IDs for website and car, then converting the characters into integers
##########Scion xB

xbnoduplicate$website_id <- case_when(
  xbnoduplicate$origin == 'autotrader' ~ '1', 
  xbnoduplicate$origin == 'cars' ~ '2',
  xbnoduplicate$origin == 'truecar' ~ '3', TRUE ~ 'w'
)

xbnoduplicate$website_id <- as.integer(xbnoduplicate$website_id) 

xbnoduplicate$car_id = '1'
xbnoduplicate$car_id <- as.integer(xbnoduplicate$car_id) 


##########Scion TC 

tcnoduplicate$website_id <- case_when(
  tcnoduplicate$origin == 'autotrader' ~ '1', 
  tcnoduplicate$origin == 'cars' ~ '2',
  tcnoduplicate$origin == 'truecar' ~ '3', TRUE ~ 'w'
)

tcnoduplicate$website_id <- as.integer(tcnoduplicate$website_id) 

tcnoduplicate$car_id = '2'
tcnoduplicate$car_id <- as.integer(tcnoduplicate$car_id) 

##########Toyota Avalon

avalonnoduplicate$website_id <- case_when(
  avalonnoduplicate$origin == 'autotrader' ~ '1', 
  avalonnoduplicate$origin == 'cars' ~ '2',
  avalonnoduplicate$origin == 'truecar' ~ '3', TRUE ~ 'w'
)

avalonnoduplicate$website_id <- as.integer(avalonnoduplicate$website_id) 

avalonnoduplicate$car_id = '3'
avalonnoduplicate$car_id <- as.integer(avalonnoduplicate$car_id) 


##combining all car sets into a single set



cartotal <- rbind(xbnoduplicate, tcnoduplicate, avalonnoduplicate)
cartotal <- na.omit(cartotal)





##writing to CSV
write.csv(xbnoduplicate, "D:/RLearning/usedcarcomp/xbfinal.csv", row.names = FALSE)
write.csv(tcnoduplicate, "D:/RLearning/usedcarcomp/tcfinal.csv", row.names = FALSE)
write.csv(avalonnoduplicate, "D:/RLearning/usedcarcomp/avalonfinal.csv", row.names = FALSE)
write.csv(cartotal, "D:/RLearning/usedcarcomp/cartotal.csv", row.names = FALSE)


##Exploratory Data Analysis

#mean price, mileage of xb's sold on autotrader
mean(xbnoduplicate$price[xbtotal$origin == "autotrader"])
mean(xbnoduplicate$mileage[xbtotal$origin == "autotrader"])



xbmodel <- na.omit(xbnoduplicate)


lmxb <- lm(price ~ mileage, data = xbmodel)
summary(lmxb)


xbmodel$predicted <- predict(lmxb)
xbmodel$residuals <- residuals(lmxb)



ggplot(xbmodel, aes(x = mileage, y = price)) + 
  geom_segment(aes(xend = mileage, yend = predicted), alpha = .2) +
  geom_point() + 
  geom_point(aes(y=predicted), shape = 1) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightgray") + 
  theme_classic()


ggplot(subset(cartotal, car_id == 1), aes(x = mileage, y = price)) + 
  geom_segment(aes(xend = mileage, yend = predicted), alpha = .2) +
  geom_point() + 
  geom_point(aes(y=predicted), shape = 1) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightgray") + 
  theme_classic()



#Mileage Price scatter
ggcolors <- c("#E69F00", "#56B4E9", "#009E73")

MPscatter <- ggplot(data = cartotal, aes(x=mileage, y = price, color = factor(car_id))) + 
  geom_jitter() + 
  stat_smooth(method = 'lm', se = FALSE) +
  theme_classic() +
  theme(legend.position = c(0.8,0.8), legend.background = element_rect(size = 0.2, linetype = "solid", color = "black")) +
  labs(title = "Relationship Between Mileage and Price") +
  scale_colour_manual(values = ggcolors, name = "Car Models", labels = c("Scion xB","Scion tC","Toyota Avalon"))

ggsave(MPscatter, filename = "MPscatter.png", dpi = 90, type = "cairo")

#Boxplot

carboxplot <- ggplot(data = cartotal, aes(x = factor(car_id), y = price, fill = factor(car_id))) + 
  geom_boxplot(lwd = 0.9) + 
  theme_classic() + 
  theme(legend.position = c(0.91,0.9), legend.background = element_rect(size = 0.2, linetype = "solid", color = "black")) + 
  labs(title = "Distribution of Car Model Prices", x = "Car Models", y = "Price") + 
  scale_fill_manual(name = "Car Models", labels = c("Scion xB", "Scion tC", "Toyota Avalon"), values = ggcolors)

ggsave(carboxplot, filename = "carboxplot.png", dpi = 90, type = "cairo")
  
  #Website Density Plots

pricedensplot <- ggplot(data = cartotal, aes(x = price, fill = factor(website_id))) + 
  geom_density(alpha = 0.3, size = 1) + 
  theme_classic() + 
  theme(legend.position = c(0.83,0.8), legend.background = element_rect(size = 0.2, linetype = "solid", color = "black")) + 
  labs(title = "Density Plot", subtitle = "Distribution of listing prices on each website") + 
  scale_fill_manual(name = "Car Models", labels = c("Autotrader", "Cars.com", "Truecar"), values = ggcolors)
 
ggsave(pricedensplot, filename = "pricedensplot.png", dpi = 90, type = "cairo")

##Density plot overlayed histogram
histodensplot <- ggplot(data = cartotal, aes(x = price)) + 
  geom_density(alpha = 0.3, size = 1, aes(y = ..density.., color = factor(website_id)))+ 
  geom_histogram(aes(y = ..density..), bins = 16, alpha = 0.3, color = 'black', fill = 'grey') +
  theme_classic() + 
  theme(legend.position = c(0.83,0.8), legend.background = element_rect(size = 0.2, linetype = "solid", color = "black")) + 
  labs(title = "Mixed Density Plot and Histogram", subtitle = "Distribution of listing prices on each website") + 
  scale_color_manual(name = "Car Models", labels = c("Autotrader", "Cars.com", "Truecar"), values = ggcolors)

ggsave(histodensplot, filename = "histodensplot.png", dpi = 90, type = "cairo")


##Price Year scatterplot

PYscatter <- ggplot(cartotal, aes(x = year, y = price)) + geom_jitter()

ggplot(data = cartotal, aes(x=year, y = price, color = factor(car_id))) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  theme_classic() +
  theme(legend.position = c(0.8,0.2), legend.background = element_rect(size = 0.2, linetype = "solid", color = "black")) +
  labs(title = "Relationship Between Model Year and Price") +
  scale_colour_manual(values = ggcolors, name = "Car Models", labels = c("Scion xB","Scion tC","Toyota Avalon"))



#Residual Plot
residualplot <- ggplot(xbreg, aes(x = mileage, y = price)) + 
  geom_segment(aes(xend = mileage, yend = predicted), alpha = .2) +
  geom_point() + 
  geom_point(aes(y=predicted), shape = 1) + 
  theme_classic() + 
  labs(title = "Scion xB Residual Plot")


