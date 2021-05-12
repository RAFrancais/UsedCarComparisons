#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
cars_camry <- read_html("https://www.cars.com/for-sale/searchresults.action/?mdId=22402&mkId=20085&page=1&perPage=100&rd=500&searchSource=PAGINATION&sort=relevance&stkTypId=28881&transTypeId=28113&zc=92845")
cars_camryprice <- cars_camry %>% html_nodes(".listing-row__price") %>% html_text()
cars_camrytitle <- cars_camry %>% html_nodes(".listing-row__title") %>% html_text()
cars_camrymileage <- cars_camry %>% html_nodes(".listing-row__mileage") %>% html_text()
cars_camrydf <- data.frame(cars_camryprice, cars_camrytitle, cars_camrymileage)



#removing whitespace from the strings does this do anything?
cars_camrydf[,1] <- str_trim(cars_camrydf[,1])
cars_camrydf[,2] <- str_trim(cars_camrydf[,2])
cars_camrydf[,3] <- str_trim(cars_camrydf[,3])



#Removing extranenous characters
cars_camrydf[,1] <- gsub("," , "" , cars_camrydf[,1])
cars_camrydf[,1] <- gsub("\\$" , "" , cars_camrydf[,1])

cars_camrydf[,3] <- gsub("miles" , "" , cars_camrydf[,3])
cars_camrydf[,3] <- gsub("mi." , "" , cars_camrydf[,3])
cars_camrydf[,3] <- gsub("," , "" , cars_camrydf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model, then arranging the columns to follow autotrader's pattern
cars_camrydf[,c(2,4)] <- str_split_fixed(cars_camrydf[,2], " ", 2)
cars_camrydf <- cars_camrydf[, c(1,3,2,4)]

#cleaning up names to be merged with the other datasets
names(cars_camrydf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
cars_camrydf$date <- Sys.Date()
cars_camrydf$origin <- "cars"

#dummy variables for year

cars_camrydf[,"d2007"] <- ifelse(cars_camrydf$year == 2007, 1, 0)
cars_camrydf[,"d2008"] <- ifelse(cars_camrydf$year == 2008, 1, 0)
cars_camrydf[,"d2009"] <- ifelse(cars_camrydf$year == 2009, 1, 0)
cars_camrydf[,"d2010"] <- ifelse(cars_camrydf$year == 2010, 1, 0)
cars_camrydf[,"d2011"] <- ifelse(cars_camrydf$year == 2011, 1, 0)
cars_camrydf[,"d2012"] <- ifelse(cars_camrydf$year == 2012, 1, 0)
cars_camrydf[,"d2013"] <- ifelse(cars_camrydf$year == 2013, 1, 0)
cars_camrydf[,"d2014"] <- ifelse(cars_camrydf$year == 2014, 1, 0)
cars_camrydf[,"d2015"] <- ifelse(cars_camrydf$year == 2015, 1, 0)
cars_camrydf[,"d2016"] <- ifelse(cars_camrydf$year == 2016, 1, 0)
cars_camrydf[,"d2017"] <- ifelse(cars_camrydf$year == 2017, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
cars_camrynoprice <- subset(cars_camrydf, price == "Not Priced")
cars_camryprice <- subset(cars_camrydf, price != "Not Priced")

#After separating the character string, the number columns can be converted to numeric.
cars_camryprice[,1] <- as.numeric(cars_camryprice[,1])  
cars_camryprice[,2] <- as.numeric(cars_camryprice[,2])


#dynamically naming and saving data frames
cars_camryprice_rda = paste0("cars_camryprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_camryprice, file=cars_camryprice_rda)


cars_camrynoprice_rda = paste0("cars_camrynoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_camrynoprice, file=cars_camrynoprice_rda)