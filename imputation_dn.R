#Wczytanie pakietów
library(readxl)
library(VIM)
library(laeken)
#install.packages("writexl")
library("writexl")
library('ggplot2')
library(psych)

#Wczytanie danych
weather <- read_excel("C:/Users/domin/OneDrive/Pulpit/weather1.xlsx",sheet = "weather")
MCAR <- read_excel("C:/Users/domin/OneDrive/Pulpit/weather1.xlsx",sheet = "MCAR")
MAR <- read_excel("C:/Users/domin/OneDrive/Pulpit/weather1.xlsx",sheet = "MAR")
NMAR <- read_excel("C:/Users/domin/OneDrive/Pulpit/weather1.xlsx",sheet = "NMAR")

#Usuniêcie tabelki, która znajduje siê obok w arkuszu
MCAR<-MCAR[,-c(11:23)]
MAR<-MAR[,-c(11:23)]
NMAR<-NMAR[,-c(11:23)]

#Zamiana okresleñ "braki" na faktyczne braki
MCAR<-replace(MCAR, MCAR == "braki", NA)
MAR<-replace(MAR, MAR == "braki", NA)
NMAR<-replace(NMAR, NMAR == "braki", NA)

#Zmiana wartoœci zmiennych na numeryczne
MCAR$cloud_cover <-as.numeric(MCAR$cloud_cover)
MCAR$sunshine<-as.numeric(MCAR$sunshine)
MCAR$global_radiation <-as.numeric(MCAR$global_radiation)
MCAR$min_temp<-as.numeric(MCAR$min_temp)
MCAR$mean_temp<-as.numeric(MCAR$mean_temp)
MCAR$max_temp<-as.numeric(MCAR$max_temp)
MCAR$precipitation<-as.numeric(MCAR$precipitation)
MCAR$pressure <-as.numeric(MCAR$pressure)
MCAR$snow_depth <-as.numeric(MCAR$snow_depth)

MAR$cloud_cover <-as.numeric(MAR$cloud_cover)
MAR$sunshine<-as.numeric(MAR$sunshine)
MAR$global_radiation <-as.numeric(MAR$global_radiation)
MAR$min_temp<-as.numeric(MAR$min_temp)
MAR$mean_temp<-as.numeric(MAR$mean_temp)
MAR$max_temp<-as.numeric(MAR$max_temp)
MAR$precipitation<-as.numeric(MAR$precipitation)
MAR$pressure <-as.numeric(MAR$pressure)
MAR$snow_depth <-as.numeric(MAR$snow_depth)

NMAR$cloud_cover <-as.numeric(NMAR$cloud_cover)
NMAR$sunshine<-as.numeric(NMAR$sunshine)
NMAR$global_radiation <-as.numeric(NMAR$global_radiation)
NMAR$min_temp<-as.numeric(NMAR$min_temp)
NMAR$mean_temp<-as.numeric(NMAR$mean_temp)
NMAR$max_temp<-as.numeric(NMAR$max_temp)
NMAR$precipitation<-as.numeric(NMAR$precipitation)
NMAR$pressure <-as.numeric(NMAR$pressure)
NMAR$snow_depth <-as.numeric(NMAR$snow_depth)

#Nadanie zmiennym zaokr¹gleñ do 2 miejsc po przecinku
MCAR$sunshine<-round(MCAR$sunshine,0)
MCAR$min_temp<-round(MCAR$min_temp,2)
MCAR$mean_temp<-round(MCAR$mean_temp,2)
MCAR$max_temp<-round(MCAR$max_temp,2)
MCAR$precipitation<-round(MCAR$precipitation,2)

MAR$sunshine<-round(MAR$sunshine,0)
MAR$min_temp<-round(MAR$min_temp,2)
MAR$mean_temp<-round(MAR$mean_temp,2)
MAR$max_temp<-round(MAR$max_temp,2)
MAR$precipitation<-round(MAR$precipitation,2)

NMAR$sunshine<-round(NMAR$sunshine,0)
NMAR$min_temp<-round(NMAR$min_temp,2)
NMAR$mean_temp<-round(NMAR$mean_temp,2)
NMAR$max_temp<-round(NMAR$max_temp,2)
NMAR$precipitation<-round(NMAR$precipitation,2)

#Funkcja skoœnoœci
sk=function(x, na.rm = F) {
  if (na.rm) x=x[!is.na(x)]
  x=x-mean(x)
  n=length(x)
  g1=sum(x^3)/sd(x)^3*n/(n-1)/(n-2)
  return(g1)
}

#statystyki opisowe
n<-nrow(weather)
x_sr<-apply(weather,2,mean,na.rm=T)
Me<-apply(weather,2,median,na.rm=T)
s<-apply(weather,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(weather,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
weather_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)
weather_wnk <- data.frame(weather_wnk)
write_xlsx(weather_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\weather_wnk.xlsx")

##############################################################################################################################################
#Wizualizacja wybranych zmiennych przed imputacj¹ braków wywo³anych mechanizmem MCAR
#We wszystkich mechanizmach generowania braków i imputacji bêd¹ porównywane te same zmienne w celu zobaczenia ró¿nic

ggplot(weather , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(MCAR , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(weather, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(MCAR, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

#################################################################################

####Wizualizacja braków MCAR####

#1) Braki i wzory braków
plot_missing1<-aggr(MCAR, col=c('grey','pink'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(MCAR), cex.axis=0.6,
                   cex.lab=2,
                   gap=1, ylab=c('Braki',"Wzór braków"))

NA_count1 <- colSums(is.na(MCAR))
NA_count1

#2) Wykresy pude³kowe dla braków
pbox(log(MCAR[, c(1:10)]), cex.axis=0.7)

#3) Rozk³ad braków i wartoœci
x <- MCAR[, -(10)]
x[,c(1,2,4,6,7,8,9)] <- log10(x[,c(1,2,4,6,7,8,9)])
matrixplot(x, sortby = "cloud_cover", cex.axis=0.6)

###########################################################################

####Imputacja MCAR####
#1) k najbli¿szych s¹siadów (z odleg³oœci¹ Gowera)
MCAR1<-kNN(MCAR, numFun = weightedMean, weightDist=TRUE)

#Usuniêcie tabelki TRUE/FALSE
MCARkNN<-MCAR1[,-c(11:20)]
MCARkNN$cloud_cover<-round(MCARkNN$cloud_cover,0)
MCARkNN$sunshine<-round(MCARkNN$sunshine,0)

#statystyki opisowe
n<-nrow(MCARkNN)
x_sr<-apply(MCARkNN,2,mean,na.rm=T)
Me<-apply(MCARkNN,2,median,na.rm=T)
s<-apply(MCARkNN,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(MCARkNN,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
MCARkNN_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
MCARkNN_wnk <- data.frame(MCARkNN_wnk)
write_xlsx(MCARkNN_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\MCARkNN_wnk.xlsx")

k1=cor(MCARkNN)
corPlot(k1, cex = 1.1, cex.axis=0.8,cex.lab=2)

#2) Regresyjna
MCAR_REG <- regressionImp(cloud_cover+sunshine+global_radiation+min_temp+mean_temp+
                          max_temp+precipitation+pressure+snow_depth~date,data=MCAR)

#Usuniêcie tabelki TRUE/FALSE
MCAR_REG<-MCAR_REG[,-c(11:19)]
MCAR_REG$cloud_cover<-round(MCAR_REG$cloud_cover,0)
MCAR_REG$sunshine<-round(MCAR_REG$sunshine,0)

#statystyki opisowe
n<-nrow(MCAR_REG)
x_sr<-apply(MCAR_REG,2,mean,na.rm=T)
Me<-apply(MCAR_REG,2,median,na.rm=T)
s<-apply(MCAR_REG,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(MCAR_REG,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
MCAR_REG_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
MCAR_REG_wnk<- data.frame(MCAR_REG_wnk)
write_xlsx(MCAR_REG_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\MCAR_REG_wnk.xlsx")

k2=cor(MCAR_REG)
corPlot(k2, cex = 1.1, cex.axis=0.8,cex.lab=2)

#3) Random Forest
MCAR_RandF<-rangerImpute(cloud_cover+sunshine+global_radiation+min_temp+mean_temp+
               max_temp+precipitation+pressure+snow_depth~date,data=MCAR)

#Usuniêcie tabelki TRUE/FALSE
MCAR_RandF<-MCAR_RandF[,-c(11:19)]
MCAR_RandF$cloud_cover<-round(MCAR_RandF$cloud_cover,0)
MCAR_RandF$sunshine<-round(MCAR_RandF$sunshine,0)

#statystyki opisowe
n<-nrow(MCAR_RandF)
x_sr<-apply(MCAR_RandF,2,mean,na.rm=T)
Me<-apply(MCAR_RandF,2,median,na.rm=T)
s<-apply(MCAR_RandF,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(MCAR_RandF,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
MCAR_RandF_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
MCAR_RandF_wnk <- data.frame(MCAR_RandF_wnk)
write_xlsx(MCAR_RandF_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\MCAR_RandF_wnk.xlsx")

k3=cor(MCAR_RandF)
corPlot(k3, cex = 1.1, cex.axis=0.8,cex.lab=2)

###########################################################################

#Wizualizacja po imputacji kNN - MCAR
## dla zimputowanych wartoœci
par(mfrow = c(1, 3))
pbox(log(MCARkNN[, -c(3,8)]), cex.axis=0.8)
pbox(log(MCARkNN[, c(3,8)]), cex.axis=0.7)
pbox(log(MCARkNN[, c(8,3)]), cex.axis=0.7)
par(mfrow = c(1, 3))
pbox(log(MCARkNN[, c(2,3)]), cex.axis=0.7)
pbox(log(MCARkNN[, c(7,3)]), cex.axis=0.7)
pbox(log(MCARkNN[, c(9,3)]), cex.axis=0.7)

#Wizualizacja po imputacji kNN - MCAR
#dla zimputowanych wartoœci
x_imp <- kNN(MCAR[, -(10)])
x_imp[,c(1,2,3,4,6,7,8,9)] <- log10(x_imp[,c(1,2,3,4,6,7,8,9)])
matrixplot(x_imp, delimiter = "_imp", sortby = "cloud_cover", cex.axis=0.6)

###########################################################################
##Wizualizacja danych po imputacji
ggplot(MCARkNN , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(MCAR_REG , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(MCAR_RandF , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")


ggplot(MCARkNN, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(MCAR_REG, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(MCAR_RandF, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

##############################################################################################################################################

#Wizualizacja wybranych zmiennych przed imputacj¹ braków wywo³anych mechanizmem MAR

ggplot(MAR , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(MAR, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

####Wizualizacja braków MAR####

#1) Braki i wzory braków
plot_missing2<-aggr(MAR, col=c('grey','pink'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(MAR), cex.axis=0.6,
                    cex.lab=2,
                    gap=1, ylab=c('Braki',"Wzór braków"))

NA_count2 <- colSums(is.na(MAR))
NA_count2

#2) Wykresy pude³kowe dla braków
pbox(log(MAR[, c(1:10)]), cex.axis=0.7)

#3) Rozk³ad braków i wartoœci
x <- MAR[, -(10)]
x[,c(1,2,4,6,7,8,9)] <- log10(x[,c(1,2,4,6,7,8,9)])
matrixplot(x, sortby = "cloud_cover", cex.axis=0.6)

###########################################################################

####Imputacja MAR####
#1) k najbli¿szych s¹siadów (z odleg³oœci¹ Gowera)
MARkNN<-kNN(MAR, numFun = weightedMean, weightDist=TRUE)

#Usuniêcie tabelki TRUE/FALSE
MARkNN<-MARkNN[,-c(11:20)]
MARkNN$cloud_cover<-round(MARkNN$cloud_cover,0)
MARkNN$sunshine<-round(MARkNN$sunshine,0)

#statystyki opisowe
n<-nrow(MARkNN)
x_sr<-apply(MARkNN,2,mean,na.rm=T)
Me<-apply(MARkNN,2,median,na.rm=T)
s<-apply(MARkNN,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(MARkNN,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
MARkNN_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
MARkNN_wnk <- data.frame(MARkNN_wnk)
write_xlsx(MARkNN_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\MARkNN_wnk.xlsx")

k4=cor(MARkNN)
corPlot(k4, cex = 1.1, cex.axis=0.8,cex.lab=2)

#2) Hot-deck
MAR_HD <- hotdeck(MAR,ord_var="date")

#Usuniêcie tabelki TRUE/FALSE
MAR_HD<-MAR_HD [,-c(11:19)]
MAR_HD$cloud_cover<-round(MAR_HD$cloud_cover,0)
MAR_HD$sunshine<-round(MAR_HD$sunshine,0)

#statystyki opisowe
n<-nrow(MAR_HD)
x_sr<-apply(MAR_HD,2,mean,na.rm=T)
Me<-apply(MAR_HD,2,median,na.rm=T)
s<-apply(MAR_HD,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(MAR_HD,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
MAR_HD_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
MAR_HD_wnk <- data.frame(MAR_HD_wnk)
write_xlsx(MAR_HD_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\MAR_HD_wnk.xlsx")

k4=cor(MAR_HD)
corPlot(k4, cex = 1.1, cex.axis=0.8,cex.lab=2)

#3) Random Forest
MAR_RandF<-rangerImpute(cloud_cover+sunshine+global_radiation+min_temp+mean_temp+
                           max_temp+precipitation+pressure+snow_depth~date,data=MAR)

#Usuniêcie tabelki TRUE/FALSE
MAR_RandF<-MAR_RandF[,-c(11:19)]
MAR_RandF$cloud_cover<-round(MAR_RandF$cloud_cover,0)
MAR_RandF$sunshine<-round(MAR_RandF$sunshine,0)

#statystyki opisowe
n<-nrow(MAR_RandF)
x_sr<-apply(MAR_RandF,2,mean,na.rm=T)
Me<-apply(MAR_RandF,2,median,na.rm=T)
s<-apply(MAR_RandF,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(MAR_RandF,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
MAR_RandF_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
MAR_RandF_wnk <- data.frame(MAR_RandF_wnk)
write_xlsx(MAR_RandF_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\MAR_RandF_wnk.xlsx")

k6=cor(MAR_RandF)
corPlot(k6, cex = 1.1, cex.axis=0.8,cex.lab=2)

###########################################################################

#Wizualizacja po imputacji kNN - MAR
## dla zimputowanych wartoœci
par(mfrow = c(1, 3))
pbox(log(MARkNN[, -c(3,8)]), cex.axis=0.8)
pbox(log(MARkNN[, c(3,8)]), cex.axis=0.7)
pbox(log(MARkNN[, c(8,3)]), cex.axis=0.7)
par(mfrow = c(1, 3))
pbox(log(MARkNN[, c(2,3)]), cex.axis=0.7)
pbox(log(MARkNN[, c(7,3)]), cex.axis=0.7)
pbox(log(MARkNN[, c(9,3)]), cex.axis=0.7)

#Rozk³ad braków i wartoœci
#dla zimputowanych wartoœci
y_imp <- kNN(MAR[, -(10)])
y_imp[,c(1,2,3,4,6,7,8,9)] <- log10(y_imp[,c(1,2,3,4,6,7,8,9)])
matrixplot(y_imp, delimiter = "_imp", sortby = "cloud_cover", cex.axis=0.6)

###########################################################################
##Wizualizacja danych po imputacji
ggplot(MARkNN , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(MAR_HD , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(MAR_RandF , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")


ggplot(MARkNN, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(MAR_HD, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(MAR_RandF, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

##############################################################################################################################################

#Wizualizacja wybranych zmiennych przed imputacj¹ braków wywo³anych mechanizmem NMAR

ggplot(NMAR , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(NMAR, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")


####Wizualizacja braków NMAR####

#1) Braki i wzory braków
plot_missing3<-aggr(NMAR, col=c('grey','pink'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(NMAR), cex.axis=0.6,
                    cex.lab=2,
                    gap=1, ylab=c('Braki',"Wzór braków"))

NA_count3 <- colSums(is.na(NMAR))
NA_count3

#2) Wykresy pude³kowe dla braków
pbox(log(NMAR[, c(1:10)]), cex.axis=0.7)

#3) Rozk³ad braków i wartoœci
x <- NMAR[, -(10)]
x[,c(1,2,4,6,7,8,9)] <- log10(x[,c(1,2,4,6,7,8,9)])
matrixplot(x, sortby = "cloud_cover", cex.axis=0.6)

###########################################################################

####Imputacja NMAR####
#1) k najbli¿szych s¹siadów (z odleg³oœci¹ Gowera)
NMARkNN<-kNN(NMAR, numFun = weightedMean, weightDist=TRUE)

#Usuniêcie tabelki TRUE/FALSE
NMARkNN<-NMARkNN[,-c(11:20)]
NMARkNN$cloud_cover<-round(NMARkNN$cloud_cover,0)
NMARkNN$sunshine<-round(NMARkNN$sunshine,0)

#statystyki opisowe
n<-nrow(NMARkNN)
x_sr<-apply(NMARkNN,2,mean,na.rm=T)
Me<-apply(NMARkNN,2,median,na.rm=T)
s<-apply(NMARkNN,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(NMARkNN,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
NMARkNN_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
NMARkNN_wnk <- data.frame(NMARkNN_wnk)
write_xlsx(NMARkNN_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\NMARkNN_wnk.xlsx")

k7=cor(NMARkNN)
corPlot(k7, cex = 1.1, cex.axis=0.8,cex.lab=2)


#2) Hot-deck
NMAR_HD <- hotdeck(NMAR,ord_var="date")

#Usuniêcie tabelki TRUE/FALSE
NMAR_HD<-NMAR_HD [,-c(11:19)]
NMAR_HD$cloud_cover<-round(NMAR_HD$cloud_cover,0)
NMAR_HD$sunshine<-round(NMAR_HD$sunshine,0)

#statystyki opisowe
n<-nrow(NMAR_HD)
x_sr<-apply(NMAR_HD,2,mean,na.rm=T)
Me<-apply(NMAR_HD,2,median,na.rm=T)
s<-apply(NMAR_HD,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(NMAR_HD,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
NMAR_HD_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
NMAR_HD_wnk <- data.frame(NMAR_HD_wnk)
write_xlsx(NMAR_HD_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\NMAR_HD_wnk.xlsx")

k8=cor(NMAR_HD)
corPlot(k8, cex = 1.1, cex.axis=0.8,cex.lab=2)

#3) Random Forest
NMAR_RandF<-rangerImpute(cloud_cover+sunshine+global_radiation+min_temp+mean_temp+
                           max_temp+precipitation+pressure+snow_depth~date,data=NMAR)

#Usuniêcie tabelki TRUE/FALSE
NMAR_RandF<-NMAR_RandF[,-c(11:19)]
NMAR_RandF$cloud_cover<-round(NMAR_RandF$cloud_cover,0)
NMAR_RandF$sunshine<-round(NMAR_RandF$sunshine,0)

#statystyki opisowe
n<-nrow(NMAR_RandF)
x_sr<-apply(NMAR_RandF,2,mean,na.rm=T)
Me<-apply(NMAR_RandF,2,median,na.rm=T)
s<-apply(NMAR_RandF,2,sd,na.rm=T)
Vs<-s/x_sr
gl<-apply(NMAR_RandF,2,sk,na.rm=T)
s_x_œr<- s/x_sr^0.5
NMAR_RandF_wnk<-round(rbind(n,x_sr,Me,s,Vs,gl,s_x_œr),2)

#Eksport danych do excela w celu porównania wyników imputacji z baz¹ pierwotn¹ przed wygenerowaniem braków
NMAR_RandF_wnk <- data.frame(NMAR_RandF_wnk)
write_xlsx(NMAR_RandF_wnk, "C:\\Users\\domin\\OneDrive\\Pulpit\\NMAR_RandF_wnk.xlsx")

k9=cor(NMAR_RandF)
corPlot(k9, cex = 1.1, cex.axis=0.8,cex.lab=2)
###########################################################################

#Wizualizacja po imputacji kNN - NMAR
## dla zimputowanych wartoœci
par(mfrow = c(1, 3))
pbox(log(NMARkNN[, -c(3,8)]), cex.axis=0.8)
pbox(log(NMARkNN[, c(3,8)]), cex.axis=0.7)
pbox(log(NMARkNN[, c(8,3)]), cex.axis=0.7)
par(mfrow = c(1, 3))
pbox(log(NMARkNN[, c(2,3)]), cex.axis=0.7)
pbox(log(NMARkNN[, c(7,3)]), cex.axis=0.7)
pbox(log(NMARkNN[, c(9,3)]), cex.axis=0.7)


#Rozk³ad braków i wartoœci
#dla zimputowanych wartoœci
z_imp <- kNN(NMAR[, -(10)])
z_imp[,c(1,2,3,4,6,7,8,9)] <- log10(z_imp[,c(1,2,3,4,6,7,8,9)])
matrixplot(z_imp, delimiter = "_imp", sortby = "cloud_cover", cex.axis=0.6)

###########################################################################
##Wizualizacja danych po imputacji
ggplot(NMARkNN , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(NMAR_HD , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")

ggplot(NMAR_RandF , aes(x=factor(cloud_cover), fill=factor(cloud_cover))) +
  geom_bar() +
  theme(legend.position="none")


ggplot(NMARkNN, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(NMAR_HD, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")

ggplot(NMAR_RandF, aes(x=max_temp)) + 
  geom_histogram(color="pink", fill="grey")


##############################################################################################################################################

