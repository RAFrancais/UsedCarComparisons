#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
at_avalon <- read_html("https://www.autotrader.com/cars-for-sale/toyota/avalon/garden-grove-ca-92845?channel=ATC&relevanceConfig=default&dma=&transmissionCodes=AUT&searchRadius=500&location=&marketExtension=include&startYear=2005&endYear=2012&isNewSearch=true&showAccelerateBanner=false&sortBy=relevance&numRecords=100")
at_avalonprice <- at_avalon %>% html_nodes(".cursor-pointer.panel-default .pull-right div:nth-child(1) span") %>% html_text()
at_avalontitle <- at_avalon %>% html_nodes(".cursor-pointer.panel-default .link-unstyled") %>% html_text()
at_avalonmileage <- at_avalon %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()
at_avalondf <- data.frame(at_avalonprice, at_avalontitle, at_avalonmileage)



#removing whitespace from the strings does this do anything?
at_avalondf[,1] <- str_trim(at_avalondf[,1])
at_avalondf[,2] <- str_trim(at_avalondf[,2])
at_avalondf[,3] <- str_trim(at_avalondf[,3])



#Removing extranenous characters
at_avalondf[,1] <- gsub("," , "" , at_avalondf[,1])

at_avalondf[,3] <- gsub("miles" , "" , at_avalondf[,3])
at_avalondf[,3] <- gsub("," , "" , at_avalondf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model
at_avalondf[,c(2,4,5)] <- str_split_fixed(at_avalondf[,2], " ", 3)
#Removing the useless Used column
at_avalondf[,2] <- NULL
#cleaning up names to be merged with the other datasets
names(at_avalondf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
at_avalondf$date <- Sys.Date()
at_avalondf$origin <- "autotrader"

#dummy variables for year

at_avalondf[,"d2005"] <- ifelse(at_avalondf$year == 2005, 1, 0)
at_avalondf[,"d2006"] <- ifelse(at_avalondf$year == 2006, 1, 0)
at_avalondf[,"d2007"] <- ifelse(at_avalondf$year == 2007, 1, 0)
at_avalondf[,"d2008"] <- ifelse(at_avalondf$year == 2008, 1, 0)
at_avalondf[,"d2009"] <- ifelse(at_avalondf$year == 2009, 1, 0)
at_avalondf[,"d2010"] <- ifelse(at_avalondf$year == 2010, 1, 0)
at_avalondf[,"d2011"] <- ifelse(at_avalondf$year == 2011, 1, 0)
at_avalondf[,"d2012"] <- ifelse(at_avalondf$year == 2012, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
at_avalonnoprice <- subset(at_avalondf, price == "Contact Dealer For Price")
at_avalonprice <- subset(at_avalondf, price != "Contact Dealer For Price")

#After separating the character string, the number columns can be converted to numeric.
at_avalonprice[,1] <- as.numeric(at_avalonprice[,1])  
at_avalonprice[,2] <- as.numeric(at_avalonprice[,2])


#dynamically naming and saving data frames
at_avalonprice_rda = paste0("at_avalonprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_avalonprice, file=at_avalonprice_rda)


at_avalonnoprice_rda = paste0("at_avalonnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_avalonnoprice, file=at_avalonnoprice_rda)