#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
tc_camry1 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/xb/location-garden-grove-ca/?searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_camry2 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/xb/location-garden-grove-ca/?page=2&searchRadius=500&sort[]=best_match&transmission[]=Automatic")
tc_camry3 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/scion/xb/location-garden-grove-ca/?page=3&searchRadius=500&sort[]=best_match&transmission[]=Automatic")



tc_camryprice1 <- tc_camry1 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_camryprice2 <- tc_camry2 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_camryprice3 <- tc_camry3 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()



tc_camrymileage1 <- tc_camry1 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_camrymileage2 <- tc_camry2 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_camrymileage3 <- tc_camry3 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()



tc_camryyear1 <- tc_camry1 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_camryyear2 <- tc_camry2 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_camryyear3 <- tc_camry3 %>% html_nodes(".vehicle-card-year") %>% html_text()



tc_camrytitle1 <- tc_camry1 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_camrytitle2 <- tc_camry2 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_camrytitle3 <- tc_camry3 %>% html_nodes(".vehicle-header-make-model") %>% html_text()



tc_camrydf1 <- data.frame(tc_camryprice1, tc_camrymileage1, tc_camryyear1, tc_camrytitle1)
tc_camrydf2 <- data.frame(tc_camryprice2, tc_camrymileage2, tc_camryyear2, tc_camrytitle2)
tc_camrydf3 <- data.frame(tc_camryprice3, tc_camrymileage3, tc_camryyear3, tc_camrytitle3)

names(tc_camrydf1) <- c("price", "mileage", "year", "model")
names(tc_camrydf2) <- c("price", "mileage", "year", "model")
names(tc_camrydf3) <- c("price", "mileage", "year", "model")
names(tc_camrydf3) <- c("price", "mileage", "year", "model")

tc_camrydf <- rbind(tc_camrydf1, tc_camrydf2, tc_camrydf3)

#removing whitespace from the strings does this do anything?
tc_camrydf[,1] <- str_trim(tc_camrydf[,1])
tc_camrydf[,2] <- str_trim(tc_camrydf[,2])
tc_camrydf[,3] <- str_trim(tc_camrydf[,3])
tc_camrydf[,4] <- str_trim(tc_camrydf[,4])


#Removing extranenous characters
tc_camrydf[,1] <- gsub("," , "" , tc_camrydf[,1])
tc_camrydf[,1] <- gsub("\\$" , "" , tc_camrydf[,1])

tc_camrydf[,2] <- gsub("miles" , "" , tc_camrydf[,2])
tc_camrydf[,2] <- gsub("mi." , "" , tc_camrydf[,2])
tc_camrydf[,2] <- gsub("," , "" , tc_camrydf[,2])


#creating a date column and an original website column
tc_camrydf$date <- Sys.Date()
tc_camrydf$origin <- "truecar"

#dummy variables for year

tc_camrydf[,"d2007"] <- ifelse(tc_camrydf$year == 2007, 1, 0)
tc_camrydf[,"d2008"] <- ifelse(tc_camrydf$year == 2008, 1, 0)
tc_camrydf[,"d2009"] <- ifelse(tc_camrydf$year == 2009, 1, 0)
tc_camrydf[,"d2010"] <- ifelse(tc_camrydf$year == 2010, 1, 0)
tc_camrydf[,"d2011"] <- ifelse(tc_camrydf$year == 2011, 1, 0)
tc_camrydf[,"d2012"] <- ifelse(tc_camrydf$year == 2012, 1, 0)
tc_camrydf[,"d2013"] <- ifelse(tc_camrydf$year == 2013, 1, 0)
tc_camrydf[,"d2014"] <- ifelse(tc_camrydf$year == 2014, 1, 0)
tc_camrydf[,"d2015"] <- ifelse(tc_camrydf$year == 2015, 1, 0)
tc_camrydf[,"d2016"] <- ifelse(tc_camrydf$year == 2016, 1, 0)
tc_camrydf[,"d2017"] <- ifelse(tc_camrydf$year == 2017, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
tc_camrynoprice <- subset(tc_camrydf, price == "No Price")
tc_camryprice <- subset(tc_camrydf, price != "No Price")

#After separating the character string, the number columns can be converted to numeric.
tc_camryprice[,1] <- as.numeric(tc_camryprice[,1])  
tc_camryprice[,2] <- as.numeric(tc_camryprice[,2])


#dynamically naming and saving data frames
tc_camryprice_rda = paste0("tc_camryprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_camryprice, file=tc_camryprice_rda)


tc_camrynoprice_rda = paste0("tc_camrynoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_camrynoprice, file=tc_camrynoprice_rda)