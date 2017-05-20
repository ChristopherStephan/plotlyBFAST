library(bfast)
library(zoo)
library(plotly)

# NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
# mona <- bfastmonitor(NDVIa, start = c(2010, 13))
# plotlyBfm(mona)

NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
plot(NDVIa)

mona <- bfastmonitor(NDVIa, start = c(2010, 13))
mona
plot(mona)

data = mona$data
data = data[!is.na(data)]
x = c(1:length(data))
x = mona$tspp$time

time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x

floor(mona$breakpoint)
round((mona$breakpoint - floor(mona$breakpoint)) * frequency(mona$data)) + 1

y = if(is.null(dim(mona$data))) mona$data else mona$data[,1L]
test_pred = predict(mona$model, newdata = mona$tspp)
test_pred = zoo(test_pred, mona$tspp$time, frequency = frequency(y))

df = data.frame(x, data, test_pred)

p = plot_ly(df, x = ~x, y = ~data, name='data', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y=test_pred, name='model', mode='lines') %>%
  add_trace(x=mona$history[1], y=c(min(data), max(data)), line = list(color = "gray"), name = 'start of stable history', mode = "lines") %>%
  add_trace(x=mona$monitor[1], y=c(min(data), max(data)), line = list(color = "green"), name = 'start monitoring period', mode = "lines") %>%
  add_trace(x=mona$breakpoint, y=c(min(data), max(data)), line = list(color = "red"), name = 'time of detected break', mode = "lines") %>%
  layout(p, shapes = list(
                list(type = "rect", layer = "below",
                     fillcolor = "gray", opacity = 0.2,
                     x0 = mona$history[1], x1 = mona$history[2], xref = "x",
                     y0 = min(data), y1 = max(data), yref = "y"),
                list(type = "rect", layer = "below",
                     fillcolor = "green", line = list(color = "green"), opacity = 0.2,
                     x0 = mona$monitor[1], x1 = mona$monitor[2], xref = "x",
                     y0 = min(data), y1 = max(data), yref = "y")
                )
  )
  

p



