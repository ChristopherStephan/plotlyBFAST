library(bfast)
library(zoo)
library(plotly)

NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
mona <- bfastmonitor(NDVIa, start = c(2010, 13))
plot(mona)
plotlyBfm(mona)
