#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
tc_tc1 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/tc/location-garden-grove-ca/?searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_tc2 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/tc/location-garden-grove-ca/?page=2&searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_tc3 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/tc/location-garden-grove-ca/?page=3&searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_tc4 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/tc/location-garden-grove-ca/?page=4&searchRadius=500&sort[]=best_match&transmission[]=Automatic")



tc_tcprice1 <- tc_tc1 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_tcprice2 <- tc_tc2 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_tcprice3 <- tc_tc3 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_tcprice4 <- tc_tc4 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()



tc_tcmileage1 <- tc_tc1 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_tcmileage2 <- tc_tc2 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_tcmileage3 <- tc_tc3 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_tcmileage4 <- tc_tc4 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()



tc_tcyear1 <- tc_tc1 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_tcyear2 <- tc_tc2 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_tcyear3 <- tc_tc3 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_tcyear4 <- tc_tc4 %>% html_nodes(".vehicle-card-year") %>% html_text()



tc_tctitle1 <- tc_tc1 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_tctitle2 <- tc_tc2 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_tctitle3 <- tc_tc3 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_tctitle4 <- tc_tc4 %>% html_nodes(".vehicle-header-make-model") %>% html_text()



tc_tcdf1 <- data.frame(tc_tcprice1, tc_tcmileage1, tc_tcyear1, tc_tctitle1)
tc_tcdf2 <- data.frame(tc_tcprice2, tc_tcmileage2, tc_tcyear2, tc_tctitle2)
tc_tcdf3 <- data.frame(tc_tcprice3, tc_tcmileage3, tc_tcyear3, tc_tctitle3)
tc_tcdf4 <- data.frame(tc_tcprice4, tc_tcmileage4, tc_tcyear4, tc_tctitle4)


names(tc_tcdf1) <- c("price", "mileage", "year", "model")
names(tc_tcdf2) <- c("price", "mileage", "year", "model")
names(tc_tcdf3) <- c("price", "mileage", "year", "model")
names(tc_tcdf4) <- c("price", "mileage", "year", "model")

tc_tcdf <- rbind(tc_tcdf1, tc_tcdf2, tc_tcdf3, tc_tcdf4)

#removing whitespace from the strings does this do anything?
tc_tcdf[,1] <- str_trim(tc_tcdf[,1])
tc_tcdf[,2] <- str_trim(tc_tcdf[,2])
tc_tcdf[,3] <- str_trim(tc_tcdf[,3])
tc_tcdf[,4] <- str_trim(tc_tcdf[,4])


#Removing extranenous characters
tc_tcdf[,1] <- gsub("," , "" , tc_tcdf[,1])
tc_tcdf[,1] <- gsub("\\$" , "" , tc_tcdf[,1])

tc_tcdf[,2] <- gsub("miles" , "" , tc_tcdf[,2])
tc_tcdf[,2] <- gsub("mi." , "" , tc_tcdf[,2])
tc_tcdf[,2] <- gsub("," , "" , tc_tcdf[,2])


#creating a date column and an original website column
tc_tcdf$date <- Sys.Date()
tc_tcdf$origin <- "truecar"

#dummy variables for year

tc_tcdf[,"d2005"] <- ifelse(tc_tcdf$year == 2005, 1, 0)
tc_tcdf[,"d2006"] <- ifelse(tc_tcdf$year == 2006, 1, 0)
tc_tcdf[,"d2007"] <- ifelse(tc_tcdf$year == 2007, 1, 0)
tc_tcdf[,"d2008"] <- ifelse(tc_tcdf$year == 2008, 1, 0)
tc_tcdf[,"d2009"] <- ifelse(tc_tcdf$year == 2009, 1, 0)
tc_tcdf[,"d2010"] <- ifelse(tc_tcdf$year == 2010, 1, 0)
tc_tcdf[,"d2011"] <- ifelse(tc_tcdf$year == 2011, 1, 0)
tc_tcdf[,"d2012"] <- ifelse(tc_tcdf$year == 2012, 1, 0)
tc_tcdf[,"d2013"] <- ifelse(tc_tcdf$year == 2013, 1, 0)
tc_tcdf[,"d2014"] <- ifelse(tc_tcdf$year == 2014, 1, 0)
tc_tcdf[,"d2015"] <- ifelse(tc_tcdf$year == 2015, 1, 0)
tc_tcdf[,"d2016"] <- ifelse(tc_tcdf$year == 2016, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
tc_tcnoprice <- subset(tc_tcdf, price == "No Price")
tc_tcprice <- subset(tc_tcdf, price != "No Price")

#After separating the character string, the number columns can be converted to numeric.
tc_tcprice[,1] <- as.numeric(tc_tcprice[,1])  
tc_tcprice[,2] <- as.numeric(tc_tcprice[,2])


#dynamically naming and saving data frames
tc_tcprice_rda = paste0("tc_tcprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_tcprice, file=tc_tcprice_rda)


tc_tcnoprice_rda = paste0("tc_tcnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_tcnoprice, file=tc_tcnoprice_rda)