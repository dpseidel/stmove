library(forecast)
library(adehabitatLT)
library(dplR)
library(biwavelet)

setwd("/Users/ericdougherty/Box Sync/Dissertation/MovementData/Zebra_Data")
name.list <- c("AG059", "AG061", "AG062", "AG063", "AG068", "AG252", "AG253", "AG254", "AG255", "AG256")

# Load in all zebra and compile a single dataset for ltraj object
data <- read.csv(paste(as.character(name.list[1]),".csv",sep=""))
colnames(data) <- c("X", "Name", "Date", "Longitude", "Latitude", "Speed", "Direction", "Temp", "Altitude", "PDOP")
data <- data[!duplicated(data$Date), ]
data$ID <- paste(name.list[1])

for (p in 2:length(name.list)) {
    data2 <- read.csv(paste(as.character(name.list[p]),".csv",sep=""))
    colnames(data2) <- c("X", "Name", "Date", "Longitude", "Latitude", "Speed", "Direction", "Temp",   "Altitude", "PDOP")
    data2 <- data2[!duplicated(data2$Date), ]
    data2$ID <- paste(name.list[p])
    data <- rbind(data, data2)
}

data <- data[!is.na(data$Longitude), ]
data <- data[data$Longitude < 17.2, ]
data <- data[data$Longitude > 14.4, ]
data <- data[data$Latitude > -19.5, ]
data <- data[data$Latitude < -18.5, ]
data$DatePOS <- as.POSIXct(strptime(as.character(data$Date), tz='GMT', format='%m/%d/%y %H:%M'))
data$diff[2:nrow(data)] <- diff(data$DatePOS, lag=1)
data <- data[data$diff > 2, ]

da <- as.POSIXct(strptime(as.character(data$Date), tz='GMT', format='%m/%d/%y %H:%M'))
which(duplicated(paste(da, data$ID)))

traj <- as.ltraj(xy = data[,c("Longitude","Latitude")], date = da, id = data$ID)

foo <- function(dt) {
    return(dt> (14*3600*24)) 
}

traj.cut <- cutltraj(traj, "foo(dt)", nextr = TRUE)
traj.cut <- traj.cut[summary.ltraj(traj.cut)$nb.reloc > 8000]
traj.cut

refda <- strptime("00:00", "%H:%M")
traj.NA <- setNA(traj.cut, refda, 20, units = "min")
traj.round <- sett0(traj.NA, refda, 20, units = "min")
traj.round <- traj.round[summary.ltraj(traj.round)$nb.reloc > 8000]
traj.round[[7]] <- traj.round[[7]][1:17016,]

##############################################
######         Wavelet Analysis        #######
##############################################

i=1
traj.test <- as.ltraj(xy = traj.round[[i]][,c("x","y")], date = traj.round[[i]]$date, id=as.character(summary.ltraj(traj.round)[i,2]))
lon <- traj.test[[1]][,1]
y <- lon
fit <- auto.arima(lon)
#kr <- KalmanRun(lon, fit$model)
kr <- KalmanSmooth(lon, fit$model)
id.na <- which(is.na(lon))
num <- ncol(kr$smooth)
for (j in id.na) {
    # y[j] <- fit$model$Z %*% kr$states[j,]
    y[j] <- kr$smooth[j,num]
}
lon <- y

lat <- traj.test[[1]][,2]
y <- lat
fit <- auto.arima(lat)
#kr <- KalmanRun(lon, fit$model)
kr <- KalmanSmooth(lat, fit$model)
id.na <- which(is.na(lat))
num <- ncol(kr$smooth)
for (j in id.na) {
    # y[j] <- fit$model$Z %*% kr$states[j,]
    y[j] <- kr$smooth[j,num]
}
lat <- y

xy <- cbind(lon, lat)
traj.fill <- as.ltraj(xy = xy, date = traj.test[[1]][,3], id=as.character(summary.ltraj(traj.round)[i,2]))
traj.fill.NA <- setNA(traj.fill, refda, 1, units = "hour")
traj.fill.round <- sett0(traj.fill.NA, refda, 1, units = "hour")

########################################
data <- ld(traj.fill.round)

for (j in 1:nrow(data)) {
    temp <- abs(data[j,10])
    data[j,14] <- 1 - (temp/3.14)
}

data <- data[is.na(data[,14]) != TRUE,]
k <- nrow(data)

dist.wave <- morlet(data[2:k-1,6])
persist.wave <- morlet(data[2:k-1,14])
ang.wave <- morlet(data[2:k-1,10])
ang2.wave <- morlet(data[2:k-1,9])

print(paste(as.character(summary.ltraj(traj.round)[i,1])," Step Size Wavelet",sep=''))
wavelet.plot(dist.wave, na.rm=TRUE, crn.lab = gettext("Step Size"))

print(paste(as.character(summary.ltraj(traj.round)[i,1])," Persistence Wavelet",sep=''))
wavelet.plot(persist.wave, na.rm=TRUE, crn.lab = gettext("Persistence"))

print(paste(as.character(summary.ltraj(traj.round)[i,1])," Relative Angle Wavelet",sep=''))
wavelet.plot(ang.wave, na.rm=TRUE, crn.lab = gettext("Relative Angle"))

print(paste(as.character(summary.ltraj(traj.round)[i,1])," Absolute Angle Wavelet",sep=''))
wavelet.plot(ang2.wave, na.rm=TRUE, crn.lab = gettext("Absolute Angle"))

##### Wavelet Coherence #######

dist <- data.frame(date = seq(1,length(dist.wave$y),1), y=data[2:k-1,6])
dist <- as.matrix(dist, format='%m/%d/%y %H:%M')

persist <- data.frame(date = seq(1,length(persist.wave$y),1), x1=data[2:k-1,14])
persist <- as.matrix(persist, format='%m/%d/%y %H:%M')

dist.persist <- wtc(dist, persist, quiet=TRUE)

par(mfrow = c(1,1), oma = c(4, 0, 0, 1),
    mar = c(1, 4, 4, 5), mgp = c(1.5, 0.5, 0))

plot(dist.persist, xlab = "", plot.cb = TRUE,
     main = paste(as.character(summary.ltraj(traj.round)[i,2])," Wavelet coherence of distance and persistence",sep=''))