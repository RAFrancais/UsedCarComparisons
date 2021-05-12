#used car comps 

load("D:/RLearning/usedcarcomp/at_tcprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/at_avalonprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/cars_avalonprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/tc_avalonprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/cars_tcprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/tc_tcprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/at_xbprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/cars_xbprice_20201217.Rda")
load("D:/RLearning/usedcarcomp/tc_xbprice_20201217.Rda")



xb1217 <- rbind(at_xbprice, cars_xbprice, tc_xbprice)
avalon1217_noAT <- rbind(cars_avalonprice, tc_avalonprice)
tc1217 <- rbind(at_tcprice, cars_tcprice, tc_tcprice)

save(xb1217, file = "xb121720.Rda")
save(tc1217, file = "tc121720.Rda")
save(avalon1217_noAT, file = "avalon_noAT121720.Rda")