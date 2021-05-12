#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
at_tc1 <- read_html("https://www.autotrader.com/cars-for-sale/all-cars/scion/tc/garden-grove-ca-92845?channel=ATC&relevanceConfig=default&dma=&transmissionCodes=AUT&searchRadius=500&location=&marketExtension=include&isNewSearch=true&showAccelerateBanner=false&sortBy=relevance&numRecords=100")
at_tc2 <- read_html("https://www.autotrader.com/cars-for-sale/all-cars/scion/tc/garden-grove-ca-92845?channel=ATC&relevanceConfig=default&dma=&transmissionCodes=AUT&searchRadius=500&location=&marketExtension=include&isNewSearch=false&showAccelerateBanner=false&sortBy=relevance&numRecords=100&firstRecord=100")

at_tcprice1 <- at_tc1 %>% html_nodes(".pull-right div:nth-child(1) span") %>% html_text()
at_tcprice2 <- at_tc2 %>% html_nodes(".pull-right div:nth-child(1) span") %>% html_text()

at_tctitle1 <- at_tc1 %>% html_nodes("#mountNode .link-unstyled") %>% html_text()
at_tctitle2 <- at_tc2 %>% html_nodes("#mountNode .link-unstyled") %>% html_text()

at_tcmileage1 <- at_tc1 %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()
at_tcmileage2 <- at_tc2 %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()

#autotrader occasionally has a problem with a car listing with blank mileage, leading to incorrect row length.
#i correct this by dropping that listing from the dataset
at_tcdf1 <- data.frame(at_tcprice1, at_tctitle1, at_tcmileage1)
at_tcdf2 <- data.frame(at_tcprice2, at_tctitle2, at_tcmileage2)


names(at_tcdf1) <- c("price", "model", "mileage")
names(at_tcdf2) <- c("price", "model", "mileage")

at_tcdf <- rbind(at_tcdf1,at_tcdf2)


#removing whitespace from the strings does this do anything?
at_tcdf[,1] <- str_trim(at_tcdf[,1])
at_tcdf[,2] <- str_trim(at_tcdf[,2])
at_tcdf[,3] <- str_trim(at_tcdf[,3])



#Removing extranenous characters
at_tcdf[,1] <- gsub("," , "" , at_tcdf[,1])

at_tcdf[,3] <- gsub("miles" , "" , at_tcdf[,3])
at_tcdf[,3] <- gsub("," , "" , at_tcdf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model
at_tcdf[,c(2,4,5)] <- str_split_fixed(at_tcdf[,2], " ", 3)
#Removing the useless Used column
at_tcdf[,2] <- NULL
#cleaning up names to be merged with the other datasets
names(at_tcdf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
at_tcdf$date <- Sys.Date()
at_tcdf$origin <- "autotrader"

#dummy variables for year

at_tcdf[,"d2005"] <- ifelse(at_tcdf$year == 2005, 1, 0)
at_tcdf[,"d2006"] <- ifelse(at_tcdf$year == 2006, 1, 0)
at_tcdf[,"d2007"] <- ifelse(at_tcdf$year == 2007, 1, 0)
at_tcdf[,"d2008"] <- ifelse(at_tcdf$year == 2008, 1, 0)
at_tcdf[,"d2009"] <- ifelse(at_tcdf$year == 2009, 1, 0)
at_tcdf[,"d2010"] <- ifelse(at_tcdf$year == 2010, 1, 0)
at_tcdf[,"d2011"] <- ifelse(at_tcdf$year == 2011, 1, 0)
at_tcdf[,"d2012"] <- ifelse(at_tcdf$year == 2012, 1, 0)
at_tcdf[,"d2013"] <- ifelse(at_tcdf$year == 2013, 1, 0)
at_tcdf[,"d2014"] <- ifelse(at_tcdf$year == 2014, 1, 0)
at_tcdf[,"d2015"] <- ifelse(at_tcdf$year == 2015, 1, 0)
at_tcdf[,"d2016"] <- ifelse(at_tcdf$year == 2016, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
at_tcnoprice <- subset(at_tcdf, price == "Contact Dealer For Price")
at_tcprice <- subset(at_tcdf, price != "Contact Dealer For Price")

#After separating the character string, the number columns can be converted to numeric.
at_tcprice[,1] <- as.numeric(at_tcprice[,1])  
at_tcprice[,2] <- as.numeric(at_tcprice[,2])


#dynamically naming and saving data frames
at_tcprice_rda = paste0("at_tcprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_tcprice, file=at_tcprice_rda)


at_tcnoprice_rda = paste0("at_tcnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_tcnoprice, file=at_tcnoprice_rda)