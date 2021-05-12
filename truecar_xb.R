#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
tc_xb1 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/xb/location-garden-grove-ca/?searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_xb2 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/xb/location-garden-grove-ca/?page=2&searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_xb3 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/xb/location-garden-grove-ca/?page=3&searchRadius=500&sort[]=best_match&transmission[]=Automatic")



tc_xbprice1 <- tc_xb1 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_xbprice2 <- tc_xb2 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_xbprice3 <- tc_xb3 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()



tc_xbmileage1 <- tc_xb1 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_xbmileage2 <- tc_xb2 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_xbmileage3 <- tc_xb3 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()



tc_xbyear1 <- tc_xb1 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_xbyear2 <- tc_xb2 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_xbyear3 <- tc_xb3 %>% html_nodes(".vehicle-card-year") %>% html_text()



tc_xbtitle1 <- tc_xb1 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_xbtitle2 <- tc_xb2 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_xbtitle3 <- tc_xb3 %>% html_nodes(".vehicle-header-make-model") %>% html_text()



tc_xbdf1 <- data.frame(tc_xbprice1, tc_xbmileage1, tc_xbyear1, tc_xbtitle1)
tc_xbdf2 <- data.frame(tc_xbprice2, tc_xbmileage2, tc_xbyear2, tc_xbtitle2)
tc_xbdf3 <- data.frame(tc_xbprice3, tc_xbmileage3, tc_xbyear3, tc_xbtitle3)

names(tc_xbdf1) <- c("price", "mileage", "year", "model")
names(tc_xbdf2) <- c("price", "mileage", "year", "model")
names(tc_xbdf3) <- c("price", "mileage", "year", "model")
names(tc_xbdf3) <- c("price", "mileage", "year", "model")

tc_xbdf <- rbind(tc_xbdf1, tc_xbdf2, tc_xbdf3)

#removing whitespace from the strings does this do anything?
tc_xbdf[,1] <- str_trim(tc_xbdf[,1])
tc_xbdf[,2] <- str_trim(tc_xbdf[,2])
tc_xbdf[,3] <- str_trim(tc_xbdf[,3])
tc_xbdf[,4] <- str_trim(tc_xbdf[,4])


#Removing extranenous characters
tc_xbdf[,1] <- gsub("," , "" , tc_xbdf[,1])
tc_xbdf[,1] <- gsub("\\$" , "" , tc_xbdf[,1])

tc_xbdf[,2] <- gsub("miles" , "" , tc_xbdf[,2])
tc_xbdf[,2] <- gsub("mi." , "" , tc_xbdf[,2])
tc_xbdf[,2] <- gsub("," , "" , tc_xbdf[,2])


#creating a date column and an original website column
tc_xbdf$date <- Sys.Date()
tc_xbdf$origin <- "truecar"

#dummy variables for year
tc_xbdf[,"d2005"] <- ifelse(tc_xbdf$year == 2005, 1, 0)
tc_xbdf[,"d2006"] <- ifelse(tc_xbdf$year == 2006, 1, 0)
tc_xbdf[,"d2007"] <- ifelse(tc_xbdf$year == 2007, 1, 0)
tc_xbdf[,"d2008"] <- ifelse(tc_xbdf$year == 2008, 1, 0)
tc_xbdf[,"d2009"] <- ifelse(tc_xbdf$year == 2009, 1, 0)
tc_xbdf[,"d2010"] <- ifelse(tc_xbdf$year == 2010, 1, 0)
tc_xbdf[,"d2011"] <- ifelse(tc_xbdf$year == 2011, 1, 0)
tc_xbdf[,"d2012"] <- ifelse(tc_xbdf$year == 2012, 1, 0)
tc_xbdf[,"d2013"] <- ifelse(tc_xbdf$year == 2013, 1, 0)
tc_xbdf[,"d2014"] <- ifelse(tc_xbdf$year == 2014, 1, 0)
tc_xbdf[,"d2015"] <- ifelse(tc_xbdf$year == 2015, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
tc_xbnoprice <- subset(tc_xbdf, price == "No Price")
tc_xbprice <- subset(tc_xbdf, price != "No Price")

#After separating the character string, the number columns can be converted to numeric.
tc_xbprice[,1] <- as.numeric(tc_xbprice[,1])  
tc_xbprice[,2] <- as.numeric(tc_xbprice[,2])


#dynamically naming and saving data frames
tc_xbprice_rda = paste0("tc_xbprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_xbprice, file=tc_xbprice_rda)


tc_xbnoprice_rda = paste0("tc_xbnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_xbnoprice, file=tc_xbnoprice_rda)