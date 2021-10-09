#===============================#
#Persiapan
#===============================#
setwd("E:/titipan Dimas/Publikasi/Github/FTS")
install.packages("quantmod") #scraping data
install.packages("zoo") #cleaning data

#===============================#
#scraping data
#===============================#
library(quantmod)
getSymbols("BBCA.JK", from = "2010-01-01", to = "2021-09-30", src =  "yahoo", adjust =  TRUE,)
data_asli = BBCA.JK$BBCA.JK.Close
n=length(data_asli)
n
#===============================#
#Cleaning data
#===============================#
library(zoo)
sum(is.na(data_asli))
which(is.na(data_asli))
data_asli = na.locf(data_asli)
n=length(data_asli)
n
write.csv(data_asli, file = "data uji.csv",row.names = TRUE)

#===============================#
#input data
#===============================#
datauji <- read.csv("data uji.csv", header=T, sep =",") 
data = datauji$BBCA.JK.Close
n=length(data)

#===============================#
#plot data aktual
plot(data,xlab="periode",ylab="harga",type = "l")
#===============================#
#mencari data maksimum dan minimum
minimal = min(data)/100
maksimal = max(data)/100
minimal
maksimal
#===============================#
#mencari data minnimum baru dan data maksimum baru untuk dijadikan sebagai batas atas dan batas bawah interval semesta pembicaraan U 
min.baru = floor(minimal)*100
max.baru = ceiling(maksimal)*100
min.baru
max.baru
#===============================#
#panjang Interval
n = round(1 +(3.3 *logb(length(data), base = 10)))
n
L = round((max.baru - min.baru)/n)
L
#===============================#
#Batas-batas interval
intrv.1 = seq(min.baru,max.baru,len = n+1)
intrv.1
#===============================#
#pembagian interval dan membentuk himpunan fuzzy
box1 = data.frame(NA,nrow=length(intrv.1)-1,ncol=3)
names(box1) = c("bawah","atas","kel")

for (i in 1:length(intrv.1)-1) {
  box1[i,1]=intrv.1[i]
  box1[i,2]=intrv.1[i+1]
  box1[i,3]=i
}
box1
#===============================#
#nilai tengah interval
n.tengah = data.frame(tengah=(box1[,1]+box1[,2])/2,kel=box1[,3])
n.tengah
#===============================#
#fuzzyfikasi ke data aktual
fuzifikasi=c() 
for (i in 1:length(data)){
  for (j in 1:nrow(box1)){
    if (i!=which.max(data)){
      if (data[i]>=(box1[j,1])&data[i]<(box1[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
    else {
      if (data[i]>=(box1[j,1])&data[i]<=(box1[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
  }
}
fuzifikasi
#===============================#
#fuzzyfikasi ke data asal
fuzzyfy = cbind(data,fuzifikasi)
fuzzyfy
#===============================#
#FLR
FLR = data.frame(fuzzifikasi=0,left=NA,right =NA)
for (i in 1:length(fuzifikasi)) {
  FLR[i,1] = fuzifikasi[i]
  FLR[i+1,2] = fuzifikasi[i]
  FLR[i,3] = fuzifikasi[i]
}
FLR = FLR[-nrow(FLR),]
FLR = FLR[-1,]
FLR
#===============================#
#membuat FLRG
FLRG = table(FLR[,2:3])
FLRG
#===============================#
#matriks transisi
#pembulatan peluang matriks transisi 3 angka belakang koma
bobot = round(prop.table(table(FLR[,2:3]),1),5)
bobot
#========================================================#
#PERAMALAN CHENG
cheng=NULL
for (i in 1:nrow(FLR)){
  for (j in 1:(nrow(bobot)))
    
    if (fuzifikasi[i]==j)
    {cheng[i]=sum(bobot[j,]*n.tengah[,1])}else
      if (fuzifikasi[i]==0)
      {cheng[i]=0}
}
Prediksi= round(cheng,0) 
Prediksi
#========================================================#
#tabel pembanding
datapakai = data[c(2:length(data))]
galat = abs(datapakai-Prediksi)
tabel = cbind(datapakai,Prediksi,galat)
tabel
#========================================================#
#Uji ketepatan
MSE = mean(galat^2, na.rm = TRUE)
MAE = mean(abs(galat))
MAPE = mean(abs(galat/datapakai*100), na.rm=TRUE)
ketepatan = cbind(MSE,MAE,MAPE)
ketepatan
