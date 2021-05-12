#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
at_xb <- read_html("https://www.autotrader.com/cars-for-sale/all-cars/scion/xb/garden-grove-ca-92845?dma=&channel=ATC&relevanceConfig=default&searchRadius=500&transmissionCodes=AUT&isNewSearch=false&marketExtension=include&showAccelerateBanner=false&sortBy=relevance&numRecords=100")
at_xb2 <- read_html("https://www.autotrader.com/cars-for-sale/all-cars/scion/xb/garden-grove-ca-92845?channel=ATC&relevanceConfig=default&dma=&transmissionCodes=AUT&searchRadius=500&location=&marketExtension=include&isNewSearch=false&showAccelerateBanner=false&sortBy=relevance&numRecords=100&firstRecord=100")


at_xbprice <- at_xb %>% html_nodes(".pull-right div:nth-child(1) span") %>% html_text()
at_xbprice2 <- at_xb2 %>% html_nodes(".pull-right div:nth-child(1) span") %>% html_text()


at_xbtitle <- at_xb %>% html_nodes("#mountNode .link-unstyled") %>% html_text()
at_xbtitle2 <- at_xb2 %>% html_nodes("#mountNode .link-unstyled") %>% html_text()


at_xbmileage <- at_xb %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()
at_xbmileage2 <- at_xb2 %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()


at_xbdf <- data.frame(at_xbprice, at_xbtitle, at_xbmileage)
at_xbdf2 <- data.frame(at_xbprice2, at_xbtitle2, at_xbmileage2)

names(at_xbdf) <- c("price", "title", "mileage")
names(at_xbdf2) <- c("price", "title", "mileage")

at_xbdf <- rbind(at_xbdf, at_xbdf2)
#removing potential whitespace from the strings
at_xbdf[,1] <- str_trim(at_xbdf[,1])
at_xbdf[,2] <- str_trim(at_xbdf[,2])
at_xbdf[,3] <- str_trim(at_xbdf[,3])



#Removing extranenous characters
at_xbdf[,1] <- gsub("," , "" , at_xbdf[,1])

at_xbdf[,3] <- gsub("miles" , "" , at_xbdf[,3])
at_xbdf[,3] <- gsub("," , "" , at_xbdf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and "Model"
at_xbdf[,c(2,4,5)] <- str_split_fixed(at_xbdf[,2], " ", 3)

#Removing the useless Used column, all cars listed are used
at_xbdf[,2] <- NULL
#cleaning up names to be merged with the other datasets
names(at_xbdf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
at_xbdf$date <- Sys.Date()
at_xbdf$origin <- "autotrader"

#dummy variables for year

at_xbdf[,"d2005"] <- ifelse(at_xbdf$year == 2005, 1, 0)
at_xbdf[,"d2006"] <- ifelse(at_xbdf$year == 2006, 1, 0)
at_xbdf[,"d2007"] <- ifelse(at_xbdf$year == 2007, 1, 0)
at_xbdf[,"d2008"] <- ifelse(at_xbdf$year == 2008, 1, 0)
at_xbdf[,"d2009"] <- ifelse(at_xbdf$year == 2009, 1, 0)
at_xbdf[,"d2010"] <- ifelse(at_xbdf$year == 2010, 1, 0)
at_xbdf[,"d2011"] <- ifelse(at_xbdf$year == 2011, 1, 0)
at_xbdf[,"d2012"] <- ifelse(at_xbdf$year == 2012, 1, 0)
at_xbdf[,"d2013"] <- ifelse(at_xbdf$year == 2013, 1, 0)
at_xbdf[,"d2014"] <- ifelse(at_xbdf$year == 2014, 1, 0)
at_xbdf[,"d2015"] <- ifelse(at_xbdf$year == 2015, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
at_xbnoprice <- subset(at_xbdf, price == "Contact Dealer For Price")
at_xbprice <- subset(at_xbdf, price != "Contact Dealer For Price")

#After separating the character string, the number columns can be converted to numeric.
at_xbprice[,1] <- as.numeric(at_xbprice[,1])  
at_xbprice[,2] <- as.numeric(at_xbprice[,2])


#dynamically naming and saving data frames into R files
at_xbprice_rda = paste0("at_xbprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_xbprice, file=at_xbprice_rda)


at_xbnoprice_rda = paste0("at_xbnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_xbnoprice, file=at_xbnoprice_rda)