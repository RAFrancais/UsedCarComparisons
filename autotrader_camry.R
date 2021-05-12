#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
at_camry <- read_html("https://www.autotrader.com/cars-for-sale/all-cars/scion/xb/garden-grove-ca-92845?dma=&channel=ATC&relevanceConfig=default&searchRadius=500&transmissionCodes=AUT&isNewSearch=false&marketExtension=include&showAccelerateBanner=false&sortBy=relevance&numRecords=100")
at_camryprice <- at_camry %>% html_nodes(".pull-right div:nth-child(1) span") %>% html_text()
at_camrytitle <- at_camry %>% html_nodes("#mountNode .link-unstyled") %>% html_text()
at_camrymileage <- at_camry %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()
at_camrydf <- data.frame(at_camryprice, at_camrytitle, at_camrymileage)



#removing whitespace from the strings does this do anything?
at_camrydf[,1] <- str_trim(at_camrydf[,1])
at_camrydf[,2] <- str_trim(at_camrydf[,2])
at_camrydf[,3] <- str_trim(at_camrydf[,3])



#Removing extranenous characters
at_camrydf[,1] <- gsub("," , "" , at_camrydf[,1])

at_camrydf[,3] <- gsub("miles" , "" , at_camrydf[,3])
at_camrydf[,3] <- gsub("," , "" , at_camrydf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model
at_camrydf[,c(2,4,5)] <- str_split_fixed(at_camrydf[,2], " ", 3)
#Removing the useless Used column
at_camrydf[,2] <- NULL
#cleaning up names to be merged with the other datasets
names(at_camrydf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
at_camrydf$date <- Sys.Date()
at_camrydf$origin <- "autotrader"

#dummy variables for year
at_camrydf[,"d2007"] <- ifelse(at_camrydf$year == 2007, 1, 0)
at_camrydf[,"d2008"] <- ifelse(at_camrydf$year == 2008, 1, 0)
at_camrydf[,"d2009"] <- ifelse(at_camrydf$year == 2009, 1, 0)
at_camrydf[,"d2010"] <- ifelse(at_camrydf$year == 2010, 1, 0)
at_camrydf[,"d2011"] <- ifelse(at_camrydf$year == 2011, 1, 0)
at_camrydf[,"d2012"] <- ifelse(at_camrydf$year == 2012, 1, 0)
at_camrydf[,"d2013"] <- ifelse(at_camrydf$year == 2013, 1, 0)
at_camrydf[,"d2014"] <- ifelse(at_camrydf$year == 2014, 1, 0)
at_camrydf[,"d2015"] <- ifelse(at_camrydf$year == 2015, 1, 0)
at_camrydf[,"d2016"] <- ifelse(at_camrydf$year == 2016, 1, 0)
at_camrydf[,"d2017"] <- ifelse(at_camrydf$year == 2017, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
at_camrynoprice <- subset(at_camrydf, at_camryprice == "Contact Dealer For Price")
at_camryprice <- subset(at_camrydf, at_camryprice != "Contact Dealer For Price")

#After separating the character string, the number columns can be converted to numeric.
at_camryprice[,1] <- as.numeric(at_camryprice[,1])  
at_camryprice[,2] <- as.numeric(at_camryprice[,2])


#dynamically naming and saving data frames
at_camryprice_rda = paste0("at_camryprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_camryprice, file=at_camryprice_rda)


at_camrynoprice_rda = paste0("at_camrynoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(at_camrynoprice, file=at_camrynoprice_rda)