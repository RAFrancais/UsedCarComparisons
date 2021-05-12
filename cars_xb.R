#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
cars_xb <- read_html("https://www.cars.com/for-sale/searchresults.action/?mdId=22402&mkId=20085&page=1&perPage=100&rd=500&searchSource=PAGINATION&sort=relevance&stkTypId=28881&transTypeId=28113&zc=92845")
cars_xb2 <- read_html("https://www.cars.com/for-sale/searchresults.action/?mdId=22402&mkId=20085&page=2&perPage=100&rd=500&searchSource=PAGINATION&sort=relevance&stkTypId=28881&transTypeId=28113&zc=92845")

cars_xbprice <- cars_xb %>% html_nodes(".listing-row__price") %>% html_text()
cars_xbprice2 <- cars_xb2 %>% html_nodes(".listing-row__price") %>% html_text()

cars_xbtitle <- cars_xb %>% html_nodes(".listing-row__title") %>% html_text()
cars_xbtitle2 <- cars_xb2 %>% html_nodes(".listing-row__title") %>% html_text()

cars_xbmileage <- cars_xb %>% html_nodes(".listing-row__mileage") %>% html_text()
cars_xbmileage2 <- cars_xb2 %>% html_nodes(".listing-row__mileage") %>% html_text()


cars_xbdf <- data.frame(cars_xbprice, cars_xbtitle, cars_xbmileage)
cars_xbdf2 <- data.frame(cars_xbprice2, cars_xbtitle2, cars_xbmileage2)



names(cars_xbdf) <- c("price", "title", "mileage")
names(cars_xbdf2) <- c("price", "title", "mileage")

#removing whitespace from the strings does this do anything?
cars_xbdf[,1] <- str_trim(cars_xbdf[,1])
cars_xbdf[,2] <- str_trim(cars_xbdf[,2])
cars_xbdf[,3] <- str_trim(cars_xbdf[,3])



#Removing extranenous characters
cars_xbdf[,1] <- gsub("," , "" , cars_xbdf[,1])
cars_xbdf[,1] <- gsub("\\$" , "" , cars_xbdf[,1])

cars_xbdf[,3] <- gsub("miles" , "" , cars_xbdf[,3])
cars_xbdf[,3] <- gsub("mi." , "" , cars_xbdf[,3])
cars_xbdf[,3] <- gsub("," , "" , cars_xbdf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model, then arranging the columns to follow autotrader's pattern
cars_xbdf[,c(2,4)] <- str_split_fixed(cars_xbdf[,2], " ", 2)
cars_xbdf <- cars_xbdf[, c(1,3,2,4)]

#cleaning up names to be merged with the other datasets
names(cars_xbdf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
cars_xbdf$date <- Sys.Date()
cars_xbdf$origin <- "cars"

#dummy variables for year
cars_xbdf[,"d2005"] <- ifelse(cars_xbdf$year == 2005, 1, 0)
cars_xbdf[,"d2006"] <- ifelse(cars_xbdf$year == 2006, 1, 0)
cars_xbdf[,"d2007"] <- ifelse(cars_xbdf$year == 2007, 1, 0)
cars_xbdf[,"d2008"] <- ifelse(cars_xbdf$year == 2008, 1, 0)
cars_xbdf[,"d2009"] <- ifelse(cars_xbdf$year == 2009, 1, 0)
cars_xbdf[,"d2010"] <- ifelse(cars_xbdf$year == 2010, 1, 0)
cars_xbdf[,"d2011"] <- ifelse(cars_xbdf$year == 2011, 1, 0)
cars_xbdf[,"d2012"] <- ifelse(cars_xbdf$year == 2012, 1, 0)
cars_xbdf[,"d2013"] <- ifelse(cars_xbdf$year == 2013, 1, 0)
cars_xbdf[,"d2014"] <- ifelse(cars_xbdf$year == 2014, 1, 0)
cars_xbdf[,"d2015"] <- ifelse(cars_xbdf$year == 2015, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
cars_xbnoprice <- subset(cars_xbdf, price == "Not Priced")
cars_xbprice <- subset(cars_xbdf, price != "Not Priced")

#After separating the character string, the number columns can be converted to numeric.
cars_xbprice[,1] <- as.numeric(cars_xbprice[,1])  
cars_xbprice[,2] <- as.numeric(cars_xbprice[,2])


#dynamically naming and saving data frames
cars_xbprice_rda = paste0("cars_xbprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_xbprice, file=cars_xbprice_rda)


cars_xbnoprice_rda = paste0("cars_xbnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_xbnoprice, file=cars_xbnoprice_rda)