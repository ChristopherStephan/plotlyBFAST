library(bfast)
library(zoo)
library(plotly)

time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x
num2time <- function(bfm) sprintf("Break detected at: %i(%i)", floor(bfm$breakpoint), round((bfm$breakpoint - floor(bfm$breakpoint)) * frequency(bfm$data)))

plotlyBfm <- function(bfm) {
  data = bfm$data
  time = bfm$tspp$time
  test_pred = predict(bfm$model, newdata = bfm$tspp)
  test_pred = zoo(test_pred, bfm$tspp$time, frequency = frequency(data))

  data = data[!is.na(data)]  
  df = data.frame(time, data, test_pred)
  df = cbind(rownames(df), df)
  rownames(df) <- NULL
  colnames(df) <- c("yearCycle", "numTime", "ddata", "test_pred")
  
  plot = plot_ly(df, x=~numTime, y=~ddata, name='data', type='scatter', mode='lines+markers', text=df$yearCycle, hoverinfo="x+y+text+name") %>% 
    add_trace(x=df$numTime, y=df$test_pred, name='model', line=list(color = "orange"), mode='lines') %>%
    add_trace(x=bfm$history[1], y=c(min(df$ddata), rep(min(df$ddata), (length(df$ddata)-2)), max(df$ddata)), line=list(color = "gray"), name='start of stable history', mode="lines") %>%
    add_trace(x=bfm$monitor[1], y=c(min(df$ddata), rep(min(df$ddata), (length(df$ddata)-2)), max(df$ddata)), line=list(color = "green"), name='start monitoring period', mode="lines") %>%
    add_trace(x=bfm$breakpoint, y=c(min(df$ddata), rep(min(df$ddata), (length(df$ddata)-2)), max(df$ddata)), line=list(color = "red"), name='time of detected break', mode="lines") %>%
    layout(title = num2time(bfm),
           xaxis = list(title = "Index", autotick=FALSE),
           yaxis = list(title = "Data"),
           shapes = list(
             list(type = "rect", layer = "below",
                  fillcolor = "gray", opacity = 0.2,
                  x0 = bfm$history[1], x1 = bfm$history[2], xref = "x",
                  y0 = min(df$ddata), y1 = max(df$ddata), yref = "y"),
             list(type = "rect", layer = "below",
                  fillcolor = "green", line = list(color = "green"), opacity = 0.2,
                  x0 = bfm$monitor[1], x1 = bfm$monitor[2], xref = "x",
                  y0 = min(df$ddata), y1 = max(df$ddata), yref = "y")
           )
    )
  plot
}

