# Project: RValuationAnalysis
# Version: 1.2.0
# License: GPL-3
# Copyright (c) 2021 netvapor
# URL: github.com/netvapor/RValuationAnalysis
# Formatting: UTF-8

library(quantmod)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(MASS)
library(grid)
library(gridExtra)
library(scales)
library(yaml)

config = yaml.load_file(file.path(getwd(), "config.yml"))
symbol <- config$symbol
sybol_name <- config$sybol_name
if(!is.null(sybol_name) & !sybol_name == ""){
  sybol_name = paste0(" (",sybol_name,")")
}

start_date <- config$start_date
end_date <- config$end_date
if(is.null(end_date)){
  end_date = Sys.Date()
}
focus_period <- config$focus_period

output_file_path <- config$output_file_path
if(is.null(output_file_path)){
  output_file_path = file.path(getwd(), symbol)
}

chart_size = config$chart_size
if(is.null(chart_size)){
  chart_size = c(1920, 1080)
}

text_size = config$text_size
if(is.null(text_size)){
  text_size = 15
}

without_inflation = config$without_inflation
if(is.null(without_inflation)){
  without_inflation = F
}

main_time_axis_breaks = config$main_time_axis_breaks
focus_time_axis_breaks = config$focus_time_axis_breaks

get_log_breaks <- function(data, breaks){
  # Calculate rounded axis breaks on log scale
  vec_ax_log = seq(from = log(min(data)), to = log(max(data)), length.out = breaks)
  log_axis = signif(exp(vec_ax_log), 2)
  return(log_axis)
}

get_log_axis <- function(data, breaks=13){
  # Wrapper for getting rounded axis breaks on a log scale
  range_up = log(max(data)) - log(100)
  range_down = log(100) - log(min(data))
  breaks_up = ceiling(range_up / (range_up + range_down) * breaks)
  breaks_down = ceiling(range_down / (range_up + range_down) * breaks)
  log_axis_up <- data[data>100] %>% get_log_breaks(breaks_up)
  log_axis_down <- data[data<100] %>% get_log_breaks(breaks_down)
  log_axis <- unique(c(log_axis_down, 100, log_axis_up))
  return(log_axis)
}

prices <- getSymbols(Symbols = c(symbol), from = start_date, to = end_date, auto.assign = F)
prices <- na.omit(prices)

if(without_inflation == T){
  # Adjust historic prices to current dollars
  inflation <- getSymbols(Symbols = c("CPIAUCSL"), from = start_date, to = end_date, auto.assign = F, src='FRED')
  inflation_adjustments <- inflation/as.numeric(last(inflation))
  inflation_adjustments_with_dates <- merge(prices, inflation_adjustments, join="left")
  
  current_inflation_adjustment = 0
  for(i in 1:nrow(prices)) 
  {
    if(!is.na(inflation_adjustments_with_dates$CPIAUCSL[i])){
      current_inflation_adjustment = inflation_adjustments_with_dates$CPIAUCSL[i]
    }
    prices$NDX.Close[i] <- as.numeric(inflation_adjustments_with_dates$NDX.Close[i]) / as.numeric(current_inflation_adjustment)
  }
}

start_date <- min(index(prices))
end_date <- max(index(prices))

total_increase = as.numeric(tail(Cl(prices), 1)) / as.numeric(head(Cl(prices), 1))
days_passed = as.numeric(max(index(prices)) - min(index(prices)))
avg_yearly_return = (((total_increase^(1 / days_passed))^365.25) - 1) * 100

data <- data.frame(index(prices), Cl(prices))
colnames(data) <- c("date", "close")
data$date <- as.Date(data$date)
last_value = tail(data$close, 1)

reg_rlm <-  rlm(log(data$close) ~ data$date, psi = psi.bisquare)
last_modelled_value = exp(tail(predict(reg_rlm), 1))
current_rel_val <- as.numeric((last_value/last_modelled_value)) * 100

increase <- last_modelled_value / exp(head(predict(reg_rlm), 1))
modelled_yearly_return <- (increase^(1 / (as.numeric(end_date - ymd(start_date)) / 365.25)) - 1) * 100

if(without_inflation==T){
  price_curve_ylab = "Inflation adjusted price"
} else {
  price_curve_ylab = "Price"
}

price_curve <- ggplot(data=data, aes(x = date, y = close)) +
  geom_line() +
  scale_x_date(date_breaks = main_time_axis_breaks, limits = ymd(start_date, end_date), labels = date_format("%b %Y")) +
  labs(title = paste0("Modelling ", symbol, sybol_name, " from ", min(data$date), " to ", max(data$date)),
       subtitle = paste0("Average return per annum: ", round(avg_yearly_return, digits = 1),
                         "% (black), Average modelled return per annum: ",
                         round(modelled_yearly_return, digits = 1), "% (blue)")) +
  scale_y_log10(breaks = get_log_breaks(data$close, 12), minor_breaks = NULL) +
  xlab("Time") +
  ylab(price_curve_ylab) +
  geom_line(aes(x = data$date, y = exp(predict(reg_rlm))), color = "cornflowerblue", size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

data_focus <- data %>% filter(date >= end_date - focus_period & date <= end_date)
price_focus_log_axis = get_log_breaks(c(data_focus$close,
                                        min(tail(exp(predict(reg_rlm)), nrow(data_focus))),
                                        max(tail(exp(predict(reg_rlm)), nrow(data_focus)))), 12)

price_focus <- ggplot() +
  geom_line(data = data_focus, aes(x = date, y = close)) +
  geom_segment(aes(x = end_date, y = last_modelled_value, xend = end_date, yend = last_value),
               alpha = 0.5,
               colour = "darkgreen",
               linewidth = 1.5,
               arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  geom_label(aes(label = sprintf("%+3.1f %%", current_rel_val-100),
                 x = end_date - focus_period/10,
                 y = (last_value + last_modelled_value)/2),
             size = round(text_size/2), colour = "darkgreen") +
  scale_x_date(date_breaks = focus_time_axis_breaks) +
  labs(title = paste0("Zoom in on the last ", focus_period, " days"),
       subtitle = paste0("Current valuation relative to model: ",
                         round(current_rel_val, 1), "%")) +
  scale_y_log10(breaks = price_focus_log_axis, minor_breaks = NULL) +
  coord_cartesian(xlim = ymd(c(end_date - focus_period, end_date)),
                  ylim = c(min(price_focus_log_axis), max(price_focus_log_axis))) +
  xlab("Time") +
  ylab("Price") +
  geom_line(data = data_focus, aes(x = date, y = tail(exp(predict(reg_rlm)), nrow(data_focus))), color = "cornflowerblue", size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

val_fact <- as.numeric(data$close)
val_rlm <- exp(predict(reg_rlm))
relation_fact_rlm <- val_fact / val_rlm * 100
rel_dat <- data.frame(date = data$date, relation = relation_fact_rlm)
relative_value_log_axis = get_log_axis(rel_dat$relation)
pct_over_val = round(table(relation_fact_rlm > current_rel_val)["TRUE"] / length(relation_fact_rlm) * 100, 1)

relative_value <- ggplot(data = rel_dat, aes(x = date, y = relation)) +
  geom_ribbon(aes(ymin=current_rel_val, ymax=pmax(relation, current_rel_val)),
              fill="red", col="red", alpha=0.5) +
  geom_line() +
  geom_hline(yintercept = 100, color = "blue") +
  geom_hline(yintercept = current_rel_val, color = "red") +
  scale_x_date(date_breaks = main_time_axis_breaks, limits = ymd(start_date, end_date), labels = date_format("%b %Y")) +
  scale_y_log10(breaks = relative_value_log_axis, minor_breaks = NULL) +
  coord_cartesian(ylim = c(min(relative_value_log_axis), max(relative_value_log_axis))) +
  xlab("Time") +
  ylab("Price relative to model") +
  labs(title = "Valuation relative to model",
       subtitle = paste0("A higher relative valuation than on ", max(rel_dat$date),
                         " is observed in ", pct_over_val,
                         "% of all days from ", min(rel_dat$date),
                         " to ", max(rel_dat$date), " (red area).")) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

relative_value_freq <- ggplot(data = rel_dat, aes(x = relation)) +
  geom_histogram(aes(y = ..count..), stat = "bin", bins=50, fill = "cornflowerblue") +
  geom_vline(xintercept = current_rel_val, color = "red") +
  geom_vline(xintercept = exp(mean(log(rel_dat$relation))), color = "darkgreen") +
  geom_vline(xintercept = 100, color = "blue") +
  scale_x_log10(breaks = relative_value_log_axis, minor_breaks = NULL) +
  ylab("Count of days") +
  xlab("Price relative to model") +
  coord_flip(xlim = c(min(relative_value_log_axis), max(relative_value_log_axis))) +
  labs(title = "Relative valuation histogram",
      subtitle = paste0("Current: ", round(current_rel_val, 1),
                  "% (red), Geometric mean: ", round(exp(mean(log(rel_dat$relation))), 1),
                  "% (green)")) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

filename <- paste0(output_file_path, '.png')
png(filename, width = chart_size[1] / 1.0, height = chart_size[2] / 1.0)
grid.arrange(price_curve, price_focus, relative_value, relative_value_freq,
             ncol = 2, widths = c(2.5, 1),
             bottom=textGrob("Find the source code @ github.com/netvapor/RValuationAnalysis",
                             gp=gpar(fontsize=text_size)))
dev.off()
