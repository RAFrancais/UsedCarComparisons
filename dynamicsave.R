#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)




traderlexus <- read_html("https://www.autotrader.com/cars-for-sale/lexus/es-300/garden-grove-ca-92845?showToolbar=true&dma=&searchRadius=500&sellerTypes=b&startYear=1990&endYear=2007&isNewSearch=true&marketExtension=include&showAccelerateBanner=false&sortBy=distanceASC&numRecords=25")
traderprice <- traderlexus %>% html_nodes("#mountNode .pull-right span") %>% html_text()
tradertitle <- traderlexus %>% html_nodes("#mountNode .link-unstyled") %>% html_text()
tradermileage <- traderlexus %>% html_nodes(".text-subdued-lighter .text-bold") %>% html_text()
traderlexusdf <- data.frame(traderprice, tradertitle, tradermileage)



#removing whitespace from the strings
traderlexusdf[,1] <- str_trim(traderlexusdf[,1])
traderlexusdf[,2] <- str_trim(traderlexusdf[,2])
traderlexusdf[,3] <- str_trim(traderlexusdf[,3])



#Removing extranenous characters
traderlexusdf[,1] <- gsub("," , "" , traderlexusdf[,1])

traderlexusdf[,3] <- gsub("miles" , "" , traderlexusdf[,3])
traderlexusdf[,3] <- gsub("," , "" , traderlexusdf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model
traderlexusdf[,c(2,4,5)] <- str_split_fixed(traderlexusdf[,2], " ", 3)
#Removing the useless Used column
traderlexusdf[,2] <- NULL
#cleaning up names to be merged with the other datasets
names(traderlexusdf) <- c("price", "mileage", "year", "model")

#Separating cars with listed prices and cars with unlisted prices
tradernoprice <- subset(traderlexusdf, traderprice == "Contact Dealer For Price")
traderpricelexus <- subset(traderlexusdf, traderprice != "Contact Dealer For Price")

#After separating the character string, the number columns can be converted to numeric.
traderpricelexus[,1] <- as.numeric(traderpricelexus[,1])  
traderpricelexus[,2] <- as.numeric(traderpricelexus[,2])


traderpricelexus[,"d1997"] <- ifelse(traderpricelexus$year == 1997, 1, 0)
traderpricelexus[,"d1998"] <- ifelse(traderpricelexus$year == 1998, 1, 0)
traderpricelexus[,"d1999"] <- ifelse(traderpricelexus$year == 1999, 1, 0)
traderpricelexus[,"d2000"] <- ifelse(traderpricelexus$year == 2000, 1, 0)
traderpricelexus[,"d2001"] <- ifelse(traderpricelexus$year == 2001, 1, 0)
traderpricelexus[,"d2002"] <- ifelse(traderpricelexus$year == 2002, 1, 0)
traderpricelexus[,"d2003"] <- ifelse(traderpricelexus$year == 2003, 1, 0)

traderpricelexus$date <- Sys.Date()
traderpricelexus$origin <- "autotrader"


traderpricelexusrda = paste0("traderpricelexus_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(traderpricelexus, file=traderpricelexusrda)


tradernopricelexusrda = paste0("tradernopricelexus_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(traderpricelexus, file=traderpricelexusrda)

