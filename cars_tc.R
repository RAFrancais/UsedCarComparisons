#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
cars_tc1 <- read_html("https://www.cars.com/for-sale/searchresults.action/?mdId=22401&mkId=20085&page=1&perPage=100&rd=500&searchSource=GN_REFINEMENT&sort=relevance&stkTypId=28881&transTypeId=28113&zc=92845")
cars_tc2 <- read_html("https://www.cars.com/for-sale/searchresults.action/?mdId=22401&mkId=20085&page=2&perPage=100&rd=500&searchSource=PAGINATION&sort=relevance&stkTypId=28881&transTypeId=28113&zc=92845")


cars_tcprice1 <- cars_tc1 %>% html_nodes(".listing-row__price") %>% html_text()
cars_tcprice2 <- cars_tc2 %>% html_nodes(".listing-row__price") %>% html_text()


cars_tctitle1 <- cars_tc1 %>% html_nodes(".listing-row__title") %>% html_text()
cars_tctitle2 <- cars_tc2 %>% html_nodes(".listing-row__title") %>% html_text()


cars_tcmileage1 <- cars_tc1 %>% html_nodes(".listing-row__mileage") %>% html_text()
cars_tcmileage2 <- cars_tc2 %>% html_nodes(".listing-row__mileage") %>% html_text()


cars_tcdf1 <- data.frame(cars_tcprice1, cars_tctitle1, cars_tcmileage1)
cars_tcdf2 <- data.frame(cars_tcprice2, cars_tctitle2, cars_tcmileage2)


names(cars_tcdf1) <- c("price", "model", "mileage")
names(cars_tcdf2) <- c("price", "model", "mileage")


cars_tcdf <- rbind(cars_tcdf1,cars_tcdf2)


#removing whitespace from the strings does this do anything?
cars_tcdf[,1] <- str_trim(cars_tcdf[,1])
cars_tcdf[,2] <- str_trim(cars_tcdf[,2])
cars_tcdf[,3] <- str_trim(cars_tcdf[,3])



#Removing extranenous characters
cars_tcdf[,1] <- gsub("," , "" , cars_tcdf[,1])
cars_tcdf[,1] <- gsub("\\$" , "" , cars_tcdf[,1])

cars_tcdf[,3] <- gsub("miles" , "" , cars_tcdf[,3])
cars_tcdf[,3] <- gsub("mi." , "" , cars_tcdf[,3])
cars_tcdf[,3] <- gsub("," , "" , cars_tcdf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model, then arranging the columns to follow autotrader's pattern
cars_tcdf[,c(2,4)] <- str_split_fixed(cars_tcdf[,2], " ", 2)
cars_tcdf <- cars_tcdf[, c(1,3,2,4)]

#cleaning up names to be merged with the other datasets
names(cars_tcdf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
cars_tcdf$date <- Sys.Date()
cars_tcdf$origin <- "cars"

#dummy variables for year

cars_tcdf[,"d2005"] <- ifelse(cars_tcdf$year == 2005, 1, 0)
cars_tcdf[,"d2006"] <- ifelse(cars_tcdf$year == 2006, 1, 0)
cars_tcdf[,"d2007"] <- ifelse(cars_tcdf$year == 2007, 1, 0)
cars_tcdf[,"d2008"] <- ifelse(cars_tcdf$year == 2008, 1, 0)
cars_tcdf[,"d2009"] <- ifelse(cars_tcdf$year == 2009, 1, 0)
cars_tcdf[,"d2010"] <- ifelse(cars_tcdf$year == 2010, 1, 0)
cars_tcdf[,"d2011"] <- ifelse(cars_tcdf$year == 2011, 1, 0)
cars_tcdf[,"d2012"] <- ifelse(cars_tcdf$year == 2012, 1, 0)
cars_tcdf[,"d2013"] <- ifelse(cars_tcdf$year == 2013, 1, 0)
cars_tcdf[,"d2014"] <- ifelse(cars_tcdf$year == 2014, 1, 0)
cars_tcdf[,"d2015"] <- ifelse(cars_tcdf$year == 2015, 1, 0)
cars_tcdf[,"d2016"] <- ifelse(cars_tcdf$year == 2016, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
cars_tcnoprice <- subset(cars_tcdf, price == "Not Priced")
cars_tcprice <- subset(cars_tcdf, price != "Not Priced")

#After separating the character string, the number columns can be converted to numeric.
cars_tcprice[,1] <- as.numeric(cars_tcprice[,1])  
cars_tcprice[,2] <- as.numeric(cars_tcprice[,2])


#dynamically naming and saving data frames
cars_tcprice_rda = paste0("cars_tcprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_tcprice, file=cars_tcprice_rda)


cars_tcnoprice_rda = paste0("cars_tcnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_tcnoprice, file=cars_tcnoprice_rda)