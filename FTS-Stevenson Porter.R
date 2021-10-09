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
data_asli = datauji$BBCA.JK.Close
n=length(data_asli)


plot(data_asli,xlab="periode",ylab="harga",type = "l")

#===============================#
#Perubahan data
#===============================#
dt= NULL
for (i in 1:n-1){
  dt[i]=((data_asli[i+1]-data_asli[i])/data_asli[i])*100
}
head(dt)
data = dt

#===============================#
#Himpunan Semesta U
#===============================#
Umin = floor(min(data))
Umax = ceiling(max(data))
Umin
Umax
#===============================#
#Pembentukan Interval
n = round(1 +3.322 *logb(length(data), base  = 10)) #banyaknya kelas interval
n
l = (Umax - Umin)/n #panjang interval
l
intervals = data.frame(mins = 0, maxs = 0)
intervals[1,1] = Umin
intervals[1,2] = Umin + l
for (i in 2:n){
  intervals[i,1] = intervals[i-1,2]
  intervals[i,2] = intervals[i,1] + l
}
interval = intervals
interval

#===============================#
#Fuzzifikasi
#===============================#
m = as.vector(rowMeans(interval))
fuzifikasi=c() 
for (i in 1:length(data)){
  for (j in 1:nrow(interval)){
    if (i!=which.max(data)){
      if (data[i]>=(interval[j,1])&data[i]<(interval[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
    else {
      if (data[i]>=(interval[j,1])&data[i]<=(interval[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
  }
}
fuzifikasi

#===============================#
#FLR dan FLRG
#===============================#
flr <- data.frame(current_state=0, next_state=0)
for(i in 1: length(fuzifikasi)){
  if(i < length(fuzifikasi)){
    flr[i,]=c(fuzifikasi[i],fuzifikasi[i+1])
  }
  else{
    flr[i,]=c(fuzifikasi[i],0)
  }
}
flr
flrg = list()
for (i in 1:nrow(interval)){
  flrgi=c()
  for (j in 1:(length(data)-1)){
    if (flr[j,1]==i){
      flrgi = c(flrgi,flr[j,2])
    }
  }
  flrg[[i]] = flrgi
}
flrg
anggota = list()
for (i in 1:nrow(interval)){
  y = flrg[[i]]
  r = unique(y)
  anggota[[i]] = r
}
anggota
frek <- data.frame(current_state=0)
for (i in 1:length(flrg)){
  frek[i,]=length(flrg[[i]])
}
frek
peringkat=rank(frek,ties.method = "first")
fuzz = c(1:length(peringkat))
sub_l = l/peringkat
peringkat1=data.frame(fuzz,peringkat,sub_l)
peringkat1

#===============================#
#Sub interval/kelas cacah
#===============================#
#Membuat Sub interval
s_interval = data.frame(min=0, max=0)
sub_int= function(q){
  n=peringkat[q]
  s_interval[1,1] = interval[q,1]
  s_interval[1,2] = interval[q,1]+sub_l[q]
  if (n>1){
    for (i in 2:n){
      s_interval[i,1] = s_interval[i-1,2]
      s_interval[i,2] = s_interval[i,1]+sub_l[q]
    }
  }
  return(s_interval)
}

urutan = order(peringkat1['peringkat']) #mengurutkan panjang interval dari yang terbesar
#menyusun sub interval berdasarkan panjang interval
#sekaligus akan menjadi dasar fuzzifikasi yang baru
s = data.frame(min=0, max=0)
new=rbind(s) 
i = 0
repeat{
  i = i+1
  n = urutan[i]
  new=rbind(new,sub_int(n))
  if (i >= length(urutan)){
    break()
  }
}
new=new[-1,]
new
#membuat fuzzifikasi baru berdasar subinterval
m2 = as.vector(rowMeans(new))
fuzifikasi2=c() 
for (i in 1:length(data)){
  for (j in 1:nrow(new)){
    if (i!=which.max(data)){
      if (data[i]>=(new[j,1])&data[i]<(new[j,2])){
        fuzifikasi2[i]=j
        break
      }
    }
    else {
      if (data[i]>=(new[j,1])&data[i]<=(new[j,2])){
        fuzifikasi2[i]=j
        break
      }
    }
  }
}
fuzifikasi2

#===============================#
#Ramalan dan Prediksi
#===============================#
#menentukan hasil ramalan
hasil = data.frame(0)
for (i in 1:nrow(new)){
  if (i == 1)
    hasil[i]=(1.5)/((1/m2[i])+(0.5/m2[i+1]))else
  if (i == nrow(new))
    hasil[i]=(1.5)/((0.5/m2[i-1])+(1/m2[i]))
  else
    hasil[i]=(2)/((0.5/m2[i-1])+(1/m2[i])+(0.5/m2[i+1]))
}
as.data.frame(hasil)

#menentukan hasil prediksi
prediksi=NULL
for (i in 1:length(fuzifikasi2)){
  for (j in 1:length(hasil)){
    if (fuzifikasi2[i]==j)
      {prediksi[i]=hasil[[j]]} else
        if(fuzifikasi2[i]==j)
          {prediksi[i]=hasil[[j]]}
  }
}
prediksi

#mengembalikan pada skala aslinya
t = NULL
for (i in 2:length(data_asli)){
  t[i]=data_asli[i-1]+(data_asli[i-1]*prediksi[i-1]/100)
}
t

#error(galat)
#tabel pembanding
datapakai = data_asli[c(2:length(data_asli))]
t = t[c(2:length(t))]
galat = abs(t-datapakai)
tabel = cbind(datapakai,t,galat)
tabel
#========================================================#
#Uji ketepatan
MSE = mean(galat^2, na.rm = TRUE)
MAE = mean(abs(galat), na.rm = TRUE)
MAPE = mean(abs(galat/datapakai*100), na.rm=TRUE)
ketepatan = cbind(MSE,MAE,MAPE)
ketepatan
