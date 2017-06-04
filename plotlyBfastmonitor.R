library(bfast)
library(zoo)
library(plotly)

time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x
num2time <- function(bfm) sprintf("Break detected at: %i(%i)", floor(bfm$breakpoint), round((bfm$breakpoint - floor(bfm$breakpoint)) * frequency(bfm$data)))

plotlyBfm <- function(bfm) {
  data = bfm$data
  data = data[!is.na(data)]
  x = bfm$tspp$time
  y = if(is.null(dim(bfm$data))) bfm$data else bfm$data[,1L]
  test_pred = predict(bfm$model, newdata = bfm$tspp)
  test_pred = zoo(test_pred, bfm$tspp$time, frequency = frequency(y))
  
  df = data.frame(x, data, test_pred)
  
  plot = plot_ly(df, x=~x, y=~data, name='data', type='scatter', mode='lines+markers') %>%
    add_trace(y=test_pred, name='model', line=list(color = "orange"), mode='lines') %>%
    add_trace(x=bfm$history[1], y=c(min(data), max(data)), line=list(color = "gray"), name='start of stable history', mode="lines") %>%
    add_trace(x=bfm$monitor[1], y=c(min(data), max(data)), line=list(color = "green"), name='start monitoring period', mode="lines") %>%
    add_trace(x=bfm$breakpoint, y=c(min(data), max(data)), line=list(color = "red"), name='time of detected break', mode="lines") %>%
    layout(title = num2time(bfm),
           xaxis = list(title = "Index"),
           yaxis = list(title = "Data"),
           shapes = list(
             list(type = "rect", layer = "below",
                  fillcolor = "gray", opacity = 0.2,
                  x0 = bfm$history[1], x1 = bfm$history[2], xref = "x",
                  y0 = min(data), y1 = max(data), yref = "y"),
             list(type = "rect", layer = "below",
                  fillcolor = "green", line = list(color = "green"), opacity = 0.2,
                  x0 = bfm$monitor[1], x1 = bfm$monitor[2], xref = "x",
                  y0 = min(data), y1 = max(data), yref = "y")
           )
    )
  plot
}