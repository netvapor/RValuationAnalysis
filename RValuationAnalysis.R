# Project: RValuationAnalysis
# Version: 1.0.0
# License: GPL-3
# Copyright (c) 2021 netvapor
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

symbol <- "^NDX"
sybol_name <- "NASDAQ 100"
start_date <- "1985-10-01"
end_date <- Sys.Date()
focus_period <- 365

output_directory <- getwd()
chart_size = c(1920, 1080)
text_size = 14

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

start_date <- min(index(prices))
end_date <- max(index(prices))

total_increase = as.numeric(tail(prices[, 1], 1)) / as.numeric(head(prices[, 1], 1))
days_passed = as.numeric(max(index(prices)) - min(index(prices)))
avg_yearly_return = (((total_increase^(1 / days_passed))^365.25) - 1) * 100

data <- data.frame(index(prices), Cl(prices))
colnames(data) <- c("date", "close")

reg_rlm <-  rlm(log(data$close) ~ data$date, psi = psi.bisquare)
current_rel_val <- as.numeric((tail(data$close, 1)/exp(tail(predict(reg_rlm), 1)))) * 100

increase <- exp(tail(predict(reg_rlm), 1)) / exp(head(predict(reg_rlm), 1))
modelled_yearly_return <- (increase^(1 / (as.numeric(end_date - ymd(start_date)) / 365.25)) - 1) * 100

price_curve <- ggplot(data=data, aes(x = date, y = close)) +
  geom_line() +
  scale_x_date(date_breaks = "2 years", limits = ymd(start_date, end_date), labels = date_format("%b %Y")) +
  labs(title = paste0(symbol, " (", sybol_name, ") - from ", min(data$date), " to ", max(data$date)),
       subtitle = paste0("Average return per annum: ", round(avg_yearly_return, digits = 1),
                         "% (black), Average modelled return per annum: ",
                         round(modelled_yearly_return, digits = 1), "% (blue)")) +
  scale_y_log10(breaks = get_log_breaks(data$close, 12), minor_breaks = NULL) +
  geom_hline(yintercept = tail(data$close, 1), color = "red") +
  xlab("Time") +
  ylab("Price (close)") +
  geom_line(aes(x = data$date, y = exp(predict(reg_rlm))), color = "cornflowerblue", size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

data_focus <- data %>% filter(date >= end_date - focus_period & date <= end_date)
price_focus_log_axis = get_log_breaks(c(data_focus$close,
                                        min(tail(exp(predict(reg_rlm)), nrow(data_focus))),
                                        max(tail(exp(predict(reg_rlm)), nrow(data_focus)))), 12)

price_focus <- ggplot(data = data_focus, aes(x = date, y = close)) +
  geom_line() +
  scale_x_date(date_breaks = "2 months") +
  labs(title = "Last 12 months",
       subtitle = paste0("Current valuation relative to model: ",
                         round(current_rel_val, 1), "% (red)")) +
                         # "%, equal to ",
                         # round(log(1+(above_rel_val/100))/log(((1+increase_rlm)^(1/12))),1),
                         # " months of return")) +
  scale_y_log10(breaks = price_focus_log_axis, minor_breaks = NULL) +
  geom_hline(yintercept = tail(data$close, 1), color = "red") +
  coord_cartesian(xlim = ymd(c(end_date - focus_period, end_date)),
                  ylim = c(min(price_focus_log_axis), max(price_focus_log_axis))) +
  xlab("Time") +
  ylab("Price (close)") +
  geom_line(aes(x = date, y = tail(exp(predict(reg_rlm)), nrow(data_focus))), color = "cornflowerblue", size = 1.5) +
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
  scale_x_date(date_breaks = "2 years", limits = ymd(start_date, end_date), labels = date_format("%b %Y")) +
  scale_y_log10(breaks = relative_value_log_axis, minor_breaks = NULL) +
  coord_cartesian(ylim = c(min(relative_value_log_axis), max(relative_value_log_axis))) +
  xlab("Time") +
  ylab("Price in % of model") +
  labs(title = "Valuation relative to model",
       subtitle = paste0("A higher relative valuation than on ", max(rel_dat$date),
                         " (", round(current_rel_val, 1),
                         "%)\nis observed in ", pct_over_val,
                         "% of all days from ", min(rel_dat$date),
                         " to ", max(rel_dat$date), ".")) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

relative_value_freq <- ggplot(data = rel_dat, aes(x = relation)) +
  geom_histogram(aes(y = ..count..), stat = "bin", bins=50, fill = "cornflowerblue") +
  geom_vline(xintercept = current_rel_val, color = "red") +
  geom_vline(xintercept = exp(mean(log(rel_dat$relation))), color = "orange", linetype = "longdash") +
  geom_vline(xintercept = 100, color = "blue") +
  geom_vline(xintercept = median(rel_dat$relation), color = "green", linetype = "longdash") +
  scale_x_log10(breaks = relative_value_log_axis, minor_breaks = NULL) +
  ylab("Count of days") +
  xlab("Price in % of model") +
  coord_flip(xlim = c(min(relative_value_log_axis), max(relative_value_log_axis))) +
  labs(title = "Relative valuation histogram",
      subtitle = paste0("Current: ", round(current_rel_val, 1),
                  "% (red)\nMedian: ", round(median(rel_dat$relation), 1),
                  "% (green), Geometric mean: ", round(exp(mean(log(rel_dat$relation))), 1),
                  "% (orange)")) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

filename <- paste0(file.path(output_directory, symbol), '.png')
png(filename, width = chart_size[1] / 1.0, height = chart_size[2] / 1.0)
grid.arrange(price_curve, price_focus, relative_value, relative_value_freq,
             ncol = 2, widths = c(2.5, 1),
             bottom=textGrob("Find the source code @ github.com/netvapor/RValuationAnalysis",
                             gp=gpar(fontsize=text_size)))
dev.off()
