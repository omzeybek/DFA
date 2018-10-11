#DFA 
require("MARSS")
require(readxl)
setwd("/Users/omerzeybek/Library/Mobile Documents/com~apple~CloudDocs/03_Google_Trends/TSFA")
dat.spp.1980<-read_xlsx("2018_09_25.xlsx")
data.full<-(dat.spp.1980)[,-c(1:2,20,4,12,13,25,29)]
day_first<-as.Date("2013-09-15")
#Veriyi long formattan short formata çeviriyorum 
data.short <- t(data.full)
View(data.short)

#Elimdeki zaman serisi sayısı 
N_ts <- dim(data.short)[1]
#Elimdeki hafta sayısı 
TT <- dim(data.short)[2]

#Ortalamaları kullanıp verimi de-trende ettim 
Row_Bar <- apply(data.short, 1, mean, na.rm = TRUE)
dat <- data.short - Row_Bar
rownames(dat) <- rownames(data.short)
spp <- rownames(dat)

## number of processes
mm <- 3
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)
## 'QQ' is identity
QQ <- "identity"  # diag(mm)


Z_vals=list(
        "z11", 0 , 0,
        "z21", "z22", 0,
        "z31","z32","z33",
        "z41", "z42","z43",
        "z51","z52","z53",
        "z61","z62","z63",
        "z71","z72","z73",
        "z81","z82","z83",
        "z91","z92","z93",
        "z101","z102","z103",
        "z111","z112","z113",
        "z121","z122","z123",
        "z131","z132","z133",
        "z141","z142","z143",
        "z151","z152","z153",
        "z161","z162","z163",
        "z171","z172","z173",
        "z181","z182","z183",
        "z191","z192","z193",
        "z201","z202","z203",
        "z211","z212","z213",
        "z221","z222","z223",
        "z231","z232","z233",
        "z241","z242","z243",
        "z251","z252","z253",
        "z261","z262","z263",
        "z271","z272","z273",
        "z281","z282","z283",
        "z291","z292","z293"
)

ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 3, byrow = TRUE)
ZZ


## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  #matrix("zero",29,3)
dd <- "zero"  #matrix(0,1,261)
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal"


## list with specifications for model vectors/matrices
mod_list <- list(B = BB, U = uu, C = CC, c = cc, Q = QQ, Z = ZZ, 
                 A = aa,D= DD, d = dd, R = RR)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 3000, allow.degen = TRUE)

dfa_1 <- MARSS(y =dat , model = mod_list, inits = init_list,control = con_list)

