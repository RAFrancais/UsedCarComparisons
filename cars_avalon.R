#data scraping
library(rvest)
library(xml2)

library(dplyr)

#removing whitespace
library(stringr)



#filtered for automatic
cars_avalon <- read_html("https://www.cars.com/for-sale/searchresults.action/?mdId=20658&mkId=20088&page=1&perPage=100&rd=500&searchSource=GN_REFINEMENT&sort=relevance&stkTypId=28881&transTypeId=28113&yrId=20199%2C20144%2C20200%2C20145%2C20201%2C34923%2C39723&zc=92845")
cars_avalonprice <- cars_avalon %>% html_nodes(".listing-row__price") %>% html_text()
cars_avalontitle <- cars_avalon %>% html_nodes(".listing-row__title") %>% html_text()
cars_avalonmileage <- cars_avalon %>% html_nodes(".listing-row__mileage") %>% html_text()
cars_avalondf <- data.frame(cars_avalonprice, cars_avalontitle, cars_avalonmileage)



#removing whitespace from the strings does this do anything?
cars_avalondf[,1] <- str_trim(cars_avalondf[,1])
cars_avalondf[,2] <- str_trim(cars_avalondf[,2])
cars_avalondf[,3] <- str_trim(cars_avalondf[,3])



#Removing extranenous characters
cars_avalondf[,1] <- gsub("," , "" , cars_avalondf[,1])
cars_avalondf[,1] <- gsub("\\$" , "" , cars_avalondf[,1])

cars_avalondf[,3] <- gsub("miles" , "" , cars_avalondf[,3])
cars_avalondf[,3] <- gsub("mi." , "" , cars_avalondf[,3])
cars_avalondf[,3] <- gsub("," , "" , cars_avalondf[,3])

#Splitting the title column into 3 columns: "Used", "Year", and Model, then arranging the columns to follow autotrader's pattern
cars_avalondf[,c(2,4)] <- str_split_fixed(cars_avalondf[,2], " ", 2)
cars_avalondf <- cars_avalondf[, c(1,3,2,4)]

#cleaning up names to be merged with the other datasets
names(cars_avalondf) <- c("price", "mileage", "year", "model")

#creating a date column and an original website column
cars_avalondf$date <- Sys.Date()
cars_avalondf$origin <- "cars"

#dummy variables for year

cars_avalondf[,"d2005"] <- ifelse(cars_avalondf$year == 2005, 1, 0)
cars_avalondf[,"d2006"] <- ifelse(cars_avalondf$year == 2006, 1, 0)
cars_avalondf[,"d2007"] <- ifelse(cars_avalondf$year == 2007, 1, 0)
cars_avalondf[,"d2008"] <- ifelse(cars_avalondf$year == 2008, 1, 0)
cars_avalondf[,"d2009"] <- ifelse(cars_avalondf$year == 2009, 1, 0)
cars_avalondf[,"d2010"] <- ifelse(cars_avalondf$year == 2010, 1, 0)
cars_avalondf[,"d2011"] <- ifelse(cars_avalondf$year == 2011, 1, 0)
cars_avalondf[,"d2012"] <- ifelse(cars_avalondf$year == 2012, 1, 0)

#Separating cars with listed prices and cars with unlisted prices
cars_avalonnoprice <- subset(cars_avalondf, price == "Not Priced")
cars_avalonprice <- subset(cars_avalondf, price != "Not Priced")

#After separating the character string, the number columns can be converted to numeric.
cars_avalonprice[,1] <- as.numeric(cars_avalonprice[,1])  
cars_avalonprice[,2] <- as.numeric(cars_avalonprice[,2])


#dynamically naming and saving data frames
cars_avalonprice_rda = paste0("cars_avalonprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_avalonprice, file=cars_avalonprice_rda)


cars_avalonnoprice_rda = paste0("cars_avalonnoprice_", gsub(Sys.Date(), pattern = "-", replacement = ""), ".Rda")
save(cars_avalonnoprice, file=cars_avalonnoprice_rda)