library(forecast)

# Kalman smoothing function using arima model to predict NA points
kalman <- function(lon, lat) {
    x <- lon
    fit <- auto.arima(lon)
    kr <- KalmanSmooth(lon, fit$model)
    id.na <- which(is.na(lon))
    num <- ncol(kr$smooth)
    for (j in id.na) {
        x[j] <- kr$smooth[j, num]
    }
    lon <- x
    
    y <- lat
    fit <- auto.arima(lat)
    kr <- KalmanSmooth(lat, fit$model)
    id.na <- which(is.na(lat))
    num <- ncol(kr$smooth)
    for (j in id.na) {
        y[j] <- kr$smooth[j, num]
    }
    lat <- y
    
    return(lon, lat)
}