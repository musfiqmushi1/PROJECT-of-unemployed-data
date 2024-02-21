library(ggplot2)
library(forecast)
#Load the data
library(readxl)
UNRATE <- read_excel("UNRATE.xlsx")
unemployed_data <- UNRATE

#Convert data into time series data
ts_data <- ts(unemployed_data$unrate,frequency = 12)

#Decompose the time series
decomposition<- stl(ts_data,s.window = "periodic")
#Plot the original data, trend, seasonality, and residual
plot(decomposition)

#Extract seasonality component
seasonality <- decomposition$time.series[,"seasonal"]

#calculate average monthly seasonality effect
average_seasonality<-rowMeans(matrix(seasonality,nrow=length(seasonality),byrow=TRUE))
#Plot the average monthly seasonality effect
barplot(average_seasonality,xlab="Month",ylab="Average Seasonality")

#ACF plot
acf(ts_data)

#PACF plot
pacf(ts_data)

#save the plots to a pdf file
pdf("E:/Mywork/Project of unemployed data.pdf")
plot(decomposition)
barplot(average_seasonality,xlab="Month",ylab="Average Seasonality")
acf(ts_data)
pacf(ts_data)
dev.off()
