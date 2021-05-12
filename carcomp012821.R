load("D:/RLearning/usedcarcomp/at_xbprice_20210128.Rda")
load("D:/RLearning/usedcarcomp/cars_xbprice_20210128.Rda")
load("D:/RLearning/usedcarcomp/tc_xbprice_20210128.Rda")
load("D:/RLearning/usedcarcomp/at_avalonprice_20210128.Rda")
load("D:/RLearning/usedcarcomp/cars_avalonprice_20210128.Rda")
load("D:/RLearning/usedcarcomp/tc_avalonprice_20210128.Rda")
load("D:/RLearning/usedcarcomp/at_tcprice20210128.Rda")
load("D:/RLearning/usedcarcomp/tc_tcprice20210128.Rda")
load("D:/RLearning/usedcarcomp/cars_tcprice20210128.Rda")


at_tcprice <- at_tcprice20210128[,-1]
cars_tcprice <- cars_tcprice20210128[,-1]
tc_tcprice <- tc_tcprice20210128[,-1]


xb0128 <- rbind(at_xbprice, cars_xbprice, tc_xbprice)
avalon0128 <- rbind(at_avalonprice, cars_avalonprice, tc_avalonprice)
tc0128 <- rbind(at_tcprice, cars_tcprice, tc_tcprice)

save(xb0128, file = "xb012821.Rda")
save(tc0128, file = "tc012821.Rda")
save(avalon0128, file = "avalon012821.Rda")