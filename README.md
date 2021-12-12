# ValuationAnalysis
Simple R script to model valuation and analyze relative deviations over time.

The model is calculated using the iterated re-weighted least squares algorithm:  
https://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares

You can configure a symbol and time frame to analyze at the top of the script as well as the chart output settings:

```
symbol <- "^NDX"
sybol_name <- "NASDAQ 100"
start_date <- "1985-10-01"
end_date <- Sys.Date()
focus_period <- 365

output_directory <- getwd()
chart_size = c(1600, 900)
text_size = 15
main_time_axis_breaks = "3 years"
```

Example chart output:
![This is an image](example-chart.png)
