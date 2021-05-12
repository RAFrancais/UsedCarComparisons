#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
tc_avalon1 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/toyota/avalon/year-2005-2012/location-garden-grove-ca/?searchRadius=500&sort[]=best_match")
tc_avalon2 <- read_html("https://www.truecar.com/used-cars-for-sale/listings/toyota/avalon/year-2005-2012/location-garden-grove-ca/?page=2&searchRadius=500&sort[]=best_match")
tc_avalon3 <- read_html("")



tc_avalonprice1 <- tc_avalon1 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_avalonprice2 <- tc_avalon2 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()
tc_avalonprice3 <- tc_avalon3 %>% html_nodes(".vehicle-card-bottom-max-50 .font-weight-bold") %>% html_text()



tc_avalonmileage1 <- tc_avalon1 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_avalonmileage2 <- tc_avalon2 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()
tc_avalonmileage3 <- tc_avalon3 %>% html_nodes(".border-top .text-truncate:nth-child(1)") %>% html_text()



tc_avalonyear1 <- tc_avalon1 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_avalonyear2 <- tc_avalon2 %>% html_nodes(".vehicle-card-year") %>% html_text()
tc_avalonyear3 <- tc_avalon3 %>% html_nodes(".vehicle-card-year") %>% html_text()



tc_avalontitle1 <- tc_avalon1 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_avalontitle2 <- tc_avalon2 %>% html_nodes(".vehicle-header-make-model") %>% html_text()
tc_avalontitle3 <- tc_avalon3 %>% html_nodes(".vehicle-header-make-model") %>% html_text()



tc_avalondf1 <- data.frame(tc_avalonprice1, tc_avalonmileage1, tc_avalonyear1, tc_avalontitle1)
tc_avalondf2 <- data.frame(tc_avalonprice2, tc_avalonmileage2, tc_avalonyear2, tc_avalontitle2)
tc_avalondf3 <- data.frame(tc_avalonprice3, tc_avalonmileage3, tc_avalonyear3, tc_avalontitle3)

names(tc_avalondf1) <- c("price", "mileage", "year", "model")
names(tc_avalondf2) <- c("price", "mileage", "year", "model")
names(tc_avalondf3) <- c("price", "mileage", "year", "model")
names(tc_avalondf3) <- c("price", "mileage", "year", "model")

#the rowbind needs to be adjusted based on the number of pages.
tc_avalondf <- rbind(tc_avalondf1, tc_avalondf2)

#removing potential whitespace from the strings
tc_avalondf[,1] <- str_trim(tc_avalondf[,1])
tc_avalondf[,2] <- str_trim(tc_avalondf[,2])
tc_avalondf[,3] <- str_trim(tc_avalondf[,3])
tc_avalondf[,4] <- str_trim(tc_avalondf[,4])


#Removing extranenous characters
tc_avalondf[,1] <- gsub("," , "" , tc_avalondf[,1])
tc_avalondf[,1] <- gsub("\\$" , "" , tc_avalondf[,1])

tc_avalondf[,2] <- gsub("miles" , "" , tc_avalondf[,2])
tc_avalondf[,2] <- gsub("mi." , "" , tc_avalondf[,2])
tc_avalondf[,2] <- gsub("," , "" , tc_avalondf[,2])


#creating a date column and an original website column
tc_avalondf$date <- Sys.Date()
tc_avalondf$origin <- "truecar"

#dummy variables for year

tc_avalondf[,"d2005"] <- ifelse(tc_avalondf$year == 2005, 1, 0)
tc_avalondf[,"d2006"] <- ifelse(tc_avalondf$year == 2006, 1, 0)
tc_avalondf[,"d2007"] <- ifelse(tc_avalondf$year == 2007, 1, 0)
tc_avalondf[,"d2008"] <- ifelse(tc_avalondf$year == 2008, 1, 0)
tc_avalondf[,"d2009"] <- ifelse(tc_avalondf$year == 2009, 1, 0)
tc_avalondf[,"d2010"] <- ifelse(tc_avalondf$year == 2010, 1, 0)
tc_avalondf[,"d2011"] <- ifelse(tc_avalondf$year == 2011, 1, 0)
tc_avalondf[,"d2012"] <- ifelse(tc_avalondf$year == 2012, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
tc_avalonnoprice <- subset(tc_avalondf, price == "No Price")
tc_avalonprice <- subset(tc_avalondf, price != "No Price")

#After separating the character string, the number columns can be converted to numeric.
tc_avalonprice[,1] <- as.numeric(tc_avalonprice[,1])  
tc_avalonprice[,2] <- as.numeric(tc_avalonprice[,2])


#dynamically naming and saving data frames
tc_avalonprice_rda = paste0("tc_avalonprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_avalonprice, file=tc_avalonprice_rda)


tc_avalonnoprice_rda = paste0("tc_avalonnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(tc_avalonnoprice, file=tc_avalonnoprice_rda)