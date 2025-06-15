ARIMA - Time Series Forecasting with ARIMA: Predicting Market Prices with Precision

```{r}
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("forecast")
install.packages("xts")
```
```{r}
# Memuat pustaka yang diperlukan
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(xts)
```
```{r}
# Membaca file Excel
raw_sugar_price <- read_excel("raw_sugar_price.xlsx")
```
```{r}
print("Nama kolom:")
print(colnames(raw_sugar_price))
```
```{r}
print("Beberapa baris pertama data:")
print(head(raw_sugar_price))
```
```{r}
raw_sugar_price$Date <- as.Date(raw_sugar_price$Date, format = "%Y-%m-%d") 
```
```{r}
# Memeriksa tipe data dari kolom 'Date' dan 'Price'
print("Tipe data kolom 'Date' setelah konversi:")
print(class(raw_sugar_price$Date))
print("Tipe data kolom 'Price':")
print(class(raw_sugar_price$Price))
```
```{r}
# Memeriksa apakah ada nilai 'NA', 'NaN', atau 'Inf' di kolom 'Date' dan 'Price'
print("Jumlah nilai NA di kolom 'Date':")
print(sum(is.na(raw_sugar_price$Date)))
print("Jumlah nilai NaN di kolom 'Date':")
print(sum(is.nan(raw_sugar_price$Date)))
print("Jumlah nilai Inf di kolom 'Date':")
print(sum(is.infinite(raw_sugar_price$Date)))
print("Jumlah nilai NA di kolom 'Price':")
print(sum(is.na(raw_sugar_price$Price)))
print("Jumlah nilai NaN di kolom 'Price':")
print(sum(is.nan(raw_sugar_price$Price)))
print("Jumlah nilai Inf di kolom 'Price':")
print(sum(is.infinite(raw_sugar_price$Price)))
```
```{r}
# Menghapus baris dengan nilai 'NA', 'NaN', atau 'Inf' di kolom 'Date' dan 'Price'
raw_sugar_price <- raw_sugar_price[!is.na(raw_sugar_price$Date) & !is.nan(raw_sugar_price$Date) & !is.infinite(raw_sugar_price$Date) &
                       !is.na(raw_sugar_price$Price) & !is.nan(raw_sugar_price$Price) & !is.infinite(raw_sugar_price$Price), ]
```
```{r}
# Memeriksa ulang beberapa baris pertama setelah pembersihan
print("Beberapa baris pertama setelah pembersihan:")
print(head(raw_sugar_price))
# Memeriksa jumlah baris setelah pembersihan
print("Jumlah baris setelah pembersihan:")
print(nrow(raw_sugar_price))
```
```{r}
if (all(!is.na(raw_sugar_price$Date)) && nrow(raw_sugar_price) > 0) {
  sugar_ts <- xts(raw_sugar_price$Price, order.by = raw_sugar_price$Date) }
```
```{r}
print("Beberapa baris pertama data time series:")
  print(head(sugar_ts))
```
```{r}
# Memastikan bahwa kolom 'Date' berisi objek tanggal yang valid
if (all(!is.na(raw_sugar_price$Date)) && nrow(raw_sugar_price) > 0)
  sugar_ts <- xts(raw_sugar_price$Price, order.by = raw_sugar_price$Date)
  # Menentukan frekuensi data time series
  frequency <- 365
  ts_data <- ts(raw_sugar_price$Price, frequency = frequency, start = c(year(min(raw_sugar_price$Date)), yday(min(raw_sugar_price$Date))))
```
```{r}
  # Memeriksa data time series
  print("Beberapa baris pertama data time series:")
  print(head(ts_data))
```
```{r}
fit <- auto.arima(ts_data)
```
```{r}
 # Melakukan forecast untuk 3 bulan ke depan (90 hari)
  forecast_period <- 90
  sugar_forecast <- forecast(fit, h = forecast_period)
```
```{r}
 # Menampilkan hasil forecast
  print(sugar_forecast)
```
```{r}
plot(sugar_forecast)
```
```{r}
forecast_arima <- forecast(fit, h=90)
```
```{r}
if (!any(is.na(forecast_arima$mean)) && !any(is.infinite(forecast_arima$mean))) {
    # Plot hasil peramalan
    plot(forecast_arima, main="ARIMA Forecast of Market Close Value", ylab="Close Value", xlab="Time")
} else {
    print("Hasil peramalan memiliki nilai NA atau Inf.")
}
```
```{r}
forecast_df <- data.frame(
    Date = seq(as.Date("2024-07-20"), by = "day", length.out = 90),
    Forecast = as.numeric(forecast_arima$mean),
    Lower80 = as.numeric(forecast_arima$lower[,1]),
    Upper80 = as.numeric(forecast_arima$upper[,1]),
    Lower95 = as.numeric(forecast_arima$lower[,2]),
    Upper95 = as.numeric(forecast_arima$upper[,2])
)
```
```{r}
print(forecast_df)
```
    

MONTE CARLO - Risk Modeling with Monte Carlo Simulation: Estimating Probabilities in Uncertain Markets

```{r}
# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(xts)
library(ggplot2)
library(fGarch)  # For GARCH modeling
```


```{r}
# Read the Excel file
raw_sugar_price <- read_excel("raw_sugar_price")
```


```{r}
# Convert Date to proper date format
raw_sugar_price$Date <- as.Date(raw_sugar_price$Date, format = "%Y-%m-%d")
```


```{r}
# Remove any rows with NA, NaN, or Inf values
raw_sugar_price <- raw_sugar_price[!is.na(raw_sugar_price$Date) & !is.nan(raw_sugar_price$Date) & !is.infinite(raw_sugar_price$Date) &
                       !is.na(raw_sugar_price$Price) & !is.nan(raw_sugar_price$Price) & !is.infinite(raw_sugar_price$Price), ]
```


```{r}
# Create time series object
ts_data <- ts(raw_sugar_price$Price, frequency = 365, start = c(year(min(raw_sugar_price$Date)), yday(min(raw_sugar_price$Date))))
```


```{r}
# Fit ARIMA model
fit <- auto.arima(ts_data)
```


```{r}
# Monte Carlo simulation
n_simulations <- 1000
forecast_horizon <- 90
simulations <- matrix(NA, nrow = forecast_horizon, ncol = n_simulations)

set.seed(123)  # for reproducibility

for (i in 1:n_simulations) {
  # Generate forecast
  fc <- forecast(fit, h = forecast_horizon)
  
  # Add random noise to the forecast
  sim <- fc$mean + rnorm(forecast_horizon, 0, fc$model$sigma2 ^ 0.5)
  
  simulations[, i] <- sim
}
```


```{r}
# Analyze simulation results
last_date <- max(raw_sugar_price$Date)
future_dates <- seq(last_date + 1, by = "day", length.out = forecast_horizon)

predicted_prices <- data.frame(
  Date = future_dates,
  Mean = rowMeans(simulations),
  Median = apply(simulations, 1, median),
  Lower_CI = apply(simulations, 1, quantile, probs = 0.025),
  Upper_CI = apply(simulations, 1, quantile, probs = 0.975)
)
```


```{r}
# Visualize Results
ggplot() +
  geom_line(data = raw_sugar_price, aes(x = Date, y = Price), color = "blue") +
  geom_line(data = predicted_prices, aes(x = Date, y = Mean), color = "red") +
  geom_ribbon(data = predicted_prices, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI), 
              fill = "pink", alpha = 0.3) +
  labs(title = "Sugar Price Forecast with Monte Carlo Simulation",
       x = "Date", y = "Price") +
  theme_minimal()
```


```{r}
# Predicted Market Prices
print("Predicted Market Prices:")
print(predicted_prices)
```



```{r}
# Calculate daily returns
raw_sugar_price <- raw_sugar_price %>%
  arrange(Date) %>%
  mutate(Returns = c(NA, diff(log(Price))))
```


```{r}
# Fit ARIMA model
fit_arima <- auto.arima(ts_data)
```


```{r}
# Fit GARCH model for volatility
returns_ts <- ts(na.omit(raw_sugar_price$Returns))
fit_garch <- garchFit(~garch(1,1), data = returns_ts, trace = FALSE)
```


```{r}
# Generate ARIMA forecast
fc_arima <- forecast(fit_arima, h = forecast_horizon)
```


```{r}
# Generate GARCH forecast
  fc_garch <- predict(fit_garch, n.ahead = forecast_horizon)
```


```{r}
# Combine ARIMA and GARCH forecasts
sim_returns <- rnorm(forecast_horizon, mean = fc_arima$mean, sd = sqrt(fc_garch$standardDeviation))
```

```{r}
last_date <- max(raw_sugar_price$Date)
future_dates <- seq(last_date + 1, by = "day", length.out = forecast_horizon)
last_price <- tail(raw_sugar_price$Price, 1)
```

```{r}
# Convert returns to prices
  sim_prices <- last_price * exp(cumsum(sim_returns))
```

```{r}
# Calculate additional metrics
predicted_prices <- predicted_prices %>%
  mutate(
    Daily_Return = c(NA, diff(log(Mean))),
    Volatility = rollapply(Daily_Return, width = 20, FUN = sd, fill = NA, align = "right") * sqrt(252),
    VaR_95 = apply(simulations, 1, quantile, probs = 0.05),
    ES_95 = apply(simulations, 1, function(x) mean(x[x <= quantile(x, probs = 0.05)]))
  )
```

```{r}
# Visualize results
p <- ggplot() +
  geom_line(data = raw_sugar_price, aes(x = Date, y = Price), color = "blue") +
  geom_line(data = predicted_prices, aes(x = Date, y = Mean), color = "red") +
  geom_ribbon(data = predicted_prices, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI), 
              fill = "pink", alpha = 0.3) +
  geom_line(data = predicted_prices, aes(x = Date, y = VaR_95), color = "green", linetype = "dashed") +
  geom_line(data = predicted_prices, aes(x = Date, y = ES_95), color = "purple", linetype = "dotdash") +
  labs(title = "Sugar Price Forecast with Monte Carlo Simulation",
       subtitle = "Including VaR and Expected Shortfall",
       x = "Date", y = "Price") +
  theme_minimal()
```

```{r}
print(p)
```

QUANTMOD - Quantitative Market Analysis with quantmod: Visualizing Financial Trends and Indicators

```{r}
install.packages("TTR")
install.packages("tidyverse")
```

```{r}
# Load required libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(TTR)
```
```{r}
# Suppress tidyverse startup messages
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
```


```{r}
# Read the Excel file
raw_data <- read_excel("raw_sugar_price")

```


```{r}
# Ensure the data is properly formatted
data <- raw_data %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)
```


```{r}
# Extract the last year of data
end_date <- max(data$Date)
start_date <- end_date - 365
data <- data %>% filter(Date >= start_date)
```


```{r}
# Calculate technical indicators
data <- data %>%
  mutate(
    SMA20 = SMA(Price, n = 20),
    SMA50 = SMA(Price, n = 50),
    EMA20 = EMA(Price, n = 20)
  )

bollinger <- BBands(data$Price)
data$BB_Upper <- bollinger[, "up"]
data$BB_Lower <- bollinger[, "dn"]
data$RSI <- RSI(data$Price)

macd <- MACD(data$Price)
data$MACD <- macd[, "macd"]
data$Signal <- macd[, "signal"]
```


```{r}
# Remove rows with NA values
data <- na.omit(data)
```


```{r}
# Create price chart with indicators
price_chart <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Price, color = "Price")) +
  geom_line(aes(y = SMA20, color = "SMA 20")) +
  geom_line(aes(y = SMA50, color = "SMA 50")) +
  geom_line(aes(y = EMA20, color = "EMA 20")) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "grey", alpha = 0.2) +
  scale_color_manual(values = c("Price" = "black", "SMA 20" = "blue", "SMA 50" = "red", "EMA 20" = "green")) +
  labs(title = "Sugar Price with Technical Indicators", y = "Price", color = "Indicator") +
  theme_minimal()
```


```{r}
# Create RSI chart
rsi_chart <- ggplot(data, aes(x = Date, y = RSI)) +
  geom_line() +
  geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "red") +
  labs(title = "Relative Strength Index (RSI)", y = "RSI") +
  theme_minimal()
```


```{r}
# Create MACD chart
macd_chart <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = MACD, color = "MACD")) +
  geom_line(aes(y = Signal, color = "Signal")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("MACD" = "blue", "Signal" = "red")) +
  labs(title = "MACD", y = "Value", color = "Line") +
  theme_minimal()
```


```{r}
# Display charts
print(price_chart)
print(rsi_chart)
print(macd_chart)
```


```{r}
# Perform basic analysis
last_price <- tail(data$Price, 1)
sma20_trend <- ifelse(tail(data$Price, 1) > tail(data$SMA20, 1), "Bullish", "Bearish")
sma50_trend <- ifelse(tail(data$Price, 1) > tail(data$SMA50, 1), "Bullish", "Bearish")
rsi_value <- tail(data$RSI, 1)
rsi_condition <- case_when(
  rsi_value > 70 ~ "Overbought",
  rsi_value < 30 ~ "Oversold",
  TRUE ~ "Neutral"
)
macd_trend <- ifelse(tail(data$MACD, 1) > tail(data$Signal, 1), "Bullish", "Bearish")
```


```{r}
# Print analysis results
cat("Sugar Price Analysis Results:\n")
cat("Current Price:", last_price, "\n")
cat("20-day SMA Trend:", sma20_trend, "\n")
cat("50-day SMA Trend:", sma50_trend, "\n")
cat("RSI Value:", rsi_value, "- Condition:", rsi_condition, "\n")
cat("MACD Trend:", macd_trend, "\n")
```


```{r}
# Calculate additional statistics
annual_return <- (last_price / data$Price[1] - 1) * 100
volatility <- sd(diff(log(data$Price))) * sqrt(252) * 100

cat("Annual Return:", round(annual_return, 2), "%\n")
cat("Annualized Volatility:", round(volatility, 2), "%\n")
```


```{r}
# Identify potential support and resistance levels
price_breaks <- seq(min(data$Price), max(data$Price), length.out = 10)
hist_data <- hist(data$Price, breaks = price_breaks, plot = FALSE)
support_resistance <- data.frame(
  level = hist_data$mids,
  count = hist_data$counts
) %>%
  arrange(desc(count)) %>%
  slice_head(n = 3)

cat("\nPotential Support/Resistance Levels:\n")
print(support_resistance)
```
```{r}
# Trend Analysis
short_term_trend <- ifelse(tail(data$SMA20, 1) > tail(data$SMA20, 20), "Uptrend", "Downtrend")
long_term_trend <- ifelse(tail(data$SMA50, 1) > tail(data$SMA50, 50), "Uptrend", "Downtrend")

cat("\nTrend Analysis:\n")
cat("Short-term Trend (based on 20-day SMA):", short_term_trend, "\n")
cat("Long-term Trend (based on 50-day SMA):", long_term_trend, "\n")
```

```{r}
print(short_term_trend)
```


```{r}
# Volatility Analysis
recent_volatility <- sd(tail(data$Price, 20)) / mean(tail(data$Price, 20)) * 100
overall_volatility <- sd(data$Price) / mean(data$Price) * 100

cat("\nVolatility Analysis:\n")
cat("Recent Volatility (last 20 days):", round(recent_volatility, 2), "%\n")
cat("Overall Volatility:", round(overall_volatility, 2), "%\n")
```


```{r}
# Momentum Analysis
momentum <- (tail(data$Price, 1) - data$Price[1]) / data$Price[1] * 100

cat("\nMomentum Analysis:\n")
cat("Price Momentum (over the analyzed period):", round(momentum, 2), "%\n")
```



NEWS ANALYSIS, Support&Resistance, Fibonacci Retracement - Market Mood Decoder: Analyzing Financial News & Social Media Sentiment
[REVISED VERSION] 

```{r}
# Read the Excel file
raw_sugar_price <- read_excel("raw_sugar_price.xlsx")
```

```{r}
# Ensure the data is properly formatted
data <- raw_sugar_price %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)
```

```{r}
# Extract the last year of data
end_date <- max(data$Date)
start_date <- end_date - 365
data <- data %>% filter(Date >= start_date)
```


```{r}
# Calculate technical indicators
data <- data %>%
  mutate(
    SMA20 = SMA(Price, n = 20),
    SMA50 = SMA(Price, n = 50),
    EMA20 = EMA(Price, n = 20)
  )

bollinger <- BBands(data$Price)
data$BB_Upper <- bollinger[, "up"]
data$BB_Lower <- bollinger[, "dn"]
data$RSI <- RSI(data$Price)

macd <- MACD(data$Price)
data$MACD <- macd[, "macd"]
data$Signal <- macd[, "signal"]
```


```{r}
# Remove rows with NA values
data <- na.omit(data)
```


```{r}
# Fibonacci Retracement Function
calculate_fibonacci_levels <- function(high, low) {
  diff <- high - low
  levels <- c(0, 0.236, 0.382, 0.5, 0.618, 0.786, 1)
  fib_levels <- high - diff * levels
  names(fib_levels) <- c("0%", "23.6%", "38.2%", "50%", "61.8%", "78.6%", "100%")
  return(fib_levels)
}
```


```{r}
# Calculate Fibonacci levels
price_high <- max(data$Price)
price_low <- min(data$Price)
fib_levels <- calculate_fibonacci_levels(price_high, price_low)
```


```{r}
# Support and Resistance Analysis
calculate_support_resistance <- function(prices, n_levels = 3) {
  # Calculate price changes
  price_changes <- diff(prices)
  
  # Identify potential support and resistance levels
  support_levels <- prices[c(TRUE, price_changes >= 0)]
  resistance_levels <- prices[c(TRUE, price_changes <= 0)]
  
  # Function to find clusters
  find_clusters <- function(levels, n) {
    clusters <- kmeans(levels, centers = n)
    sort(clusters$centers)
  }
  
  # Get top support and resistance levels
  top_support <- find_clusters(support_levels, n_levels)
  top_resistance <- find_clusters(resistance_levels, n_levels)
  
  list(support = top_support, resistance = top_resistance)
}

sr_levels <- calculate_support_resistance(data$Price)
```


```{r}
# Print support and resistance levels
cat("\nSupport Levels:\n")
print(sr_levels$support)
cat("\nResistance Levels:\n")
print(sr_levels$resistance)
cat("\nFibonacci Retracement Levels:\n")
print(fib_levels)
```

```{r}
# Function for news input
input_news <- function(news_text) {
  # Process the provided news text directly
  return(list(list(
    title = "Market Update",
    summary = "news",
    sentiment = NULL  # Sentiment will be determined by analysis
  )))
}
```


```{r}
# Function to analyze news sentiment
# Function to analyze news sentiment
analyze_news_sentiment <- function(news_list) {
  if (length(news_list) == 1 && is.null(news_list[[1]]$sentiment)) {
    # If it's a single news item without sentiment, perform basic sentiment analysis
    text <- tolower(news_list[[1]]$summary)
    positive_words <- c("support", "strong", "bullish", "up", "increase", "higher", "bounced", "confident", "positive", "satisfied", "growth", "possible")
    negative_words <- c("falling", "bearish", "oversupplied", "down", "decrease", "downward", "decrease", "lower","failed", "delayed", "disappointing", "nervous", "difficult", "concern", "restrictive", "damage")
    
    positive_count <- sum(sapply(positive_words, function(word) grepl(word, text)))
    negative_count <- sum(sapply(negative_words, function(word) grepl(word, text)))
    
    if (positive_count > negative_count) {
      sentiment <- "positive"
    } else if (negative_count > positive_count) {
      sentiment <- "negative"
    } else {
      sentiment <- "neutral"
    }
    
    return(list(counts = table(sentiment), overall = sentiment))
  } else {
    stop("Invalid input for sentiment analysis.")
  }
}

# News text
news_input <- "summary news"
```

```{r}
# Process the news text
sugar_news <- input_news(news_input)
news_sentiment <- analyze_news_sentiment(sugar_news)
```

```{r}
# Print news analysis
cat("\nNews Analysis:\n")
print(news_sentiment$counts)
cat("Overall sentiment:", news_sentiment$overall, "\n")
```

```{r}
# Modify the strategy development function to consider news sentiment
develop_strategy <- function(data, sr_levels, fib_levels, short_term_trend, long_term_trend, recent_volatility, news_sentiment) {
  last_price <- tail(data$Price, 1)
  rsi <- tail(data$RSI, 1)
  
  # Find nearest Fibonacci level
  nearest_fib <- fib_levels[which.min(abs(fib_levels - last_price))]
  nearest_fib_name <- names(nearest_fib)
  
  # Basic strategy rules (as before)
 if (short_term_trend == "Uptrend" && long_term_trend == "Uptrend") {
    if (last_price < min(sr_levels$resistance) && rsi < 70) {
      strategy <- paste("Consider Long Position - Confident outlook near", nearest_fib_name, "Fibonacci support. Possible growth opportunity, but stay vigilant.")
    } else if (last_price > max(sr_levels$resistance) && rsi > 70) {
      strategy <- "Hold / Take Profit - Market showing strength. Satisfactory performance, but be cautious of overbought conditions."
    } else {
      strategy <- paste("Hold - Positive trend intact. Watch", nearest_fib_name, "Fibonacci level for potential bounce. Stay invested but remain alert.")
    }
  } else if (short_term_trend == "Downtrend" && long_term_trend == "Downtrend") {
    if (last_price > max(sr_levels$support) && rsi > 30) {
      strategy <- paste("Consider Short Position - Concerning downtrend near", nearest_fib_name, "Fibonacci resistance. Potential for further decline, but monitor for trend reversal.")
    } else if (last_price < min(sr_levels$support) && rsi < 30) {
      strategy <- "Hold / Cover Short - Market may be oversold. Possible bounce, but overall trend remains difficult. Exercise caution."
    } else {
      strategy <- paste("Hold - Downtrend persists. Watch", nearest_fib_name, "Fibonacci level. Nervous market conditions, but stay prepared for potential trend change.")
    }
  } else {
    strategy <- paste("Neutral - Restrictive conditions. Wait for clear trend near", nearest_fib_name, "Fibonacci level. Market indecision could lead to delayed movements.")
  } 
  
  # Adjust strategy based on volatility
  if (recent_volatility > 0.02) {  # High volatility threshold
    strategy <- paste(strategy, "- Use Caution (High Volatility)")
  }
  
  # Adjust strategy based on news sentiment
  if (news_sentiment == "positive") {
    strategy <- paste(strategy, "- News sentiment is positive, consider more bullish stance")
  } else if (news_sentiment == "negative") {
    strategy <- paste(strategy, "- News sentiment is negative, consider more bearish stance")
  } else {
    strategy <- paste(strategy, "- News sentiment is neutral")
  }
  
  return(strategy)
}
```


```{r}
# Function to calculate trend
calculate_trend <- function(prices, short_period = 20, long_period = 50) {
  # Calculate short-term and long-term moving averages
  short_ma <- tail(SMA(prices, n = short_period), 1)
  long_ma <- tail(SMA(prices, n = long_period), 1)
  
  # Calculate the percentage change over the last few days
  recent_change <- (tail(prices, 1) - tail(prices, 6)[1]) / tail(prices, 6)[1] * 100
  
  # Determine trend based on moving averages and recent price action
  if (short_ma > long_ma && recent_change > 1) {
    return("Uptrend")
  } else if (short_ma < long_ma && recent_change < -1) {
    return("Downtrend")
  } else {
    return("Sideways")
  }
}

# Calculate short-term and long-term trends
short_term_trend <- calculate_trend(data$Price, short_period = 20, long_period = 50)
long_term_trend <- calculate_trend(data$Price, short_period = 50, long_period = 200)
```


```{r}
# Determine short-term and long-term trends (you'll need to implement this based on your data)
short_term_trend <- "Uptrend"  # Example value, replace with actual calculation
long_term_trend <- "Uptrend"   # Example value, replace with actual calculation

# Calculate recent volatility (you've already done this)
recent_volatility <- sd(tail(data$Price, 20)) / mean(tail(data$Price, 20)) * 100
```


```{r}
# Generate strategy
current_strategy <- develop_strategy(data, sr_levels, fib_levels, short_term_trend, long_term_trend, recent_volatility, news_sentiment$overall)
```

```{r}
# Assuming news_sentiment is a list with an 'overall' element for the sentiment
cat("\nEnhanced Sugar Price Analysis Results:\n")
cat("Current Price:", tail(data$Price, 1), "\n")
cat("Short-term Trend:", short_term_trend, "\n")
cat("Long-term Trend:", long_term_trend, "\n")
cat("RSI:", round(tail(data$RSI, 1), 2), "\n")
cat("Recent Volatility:", round(recent_volatility, 2), "%\n")
# Access the 'overall' sentiment from the news_sentiment list
cat("News Sentiment:", news_sentiment$overall, "\n")
cat("Suggested Strategy:", current_strategy, "\n")
```

```{r}
# Enhanced Trend Analysis
short_term_trend <- ifelse(tail(data$SMA20, 1) > tail(data$SMA20, 20), "Uptrend", "Downtrend")
long_term_trend <- ifelse(tail(data$SMA50, 1) > tail(data$SMA50, 50), "Uptrend", "Downtrend")
```

```{r}
# Volatility Analysis using GARCH model
returns <- diff(log(data$Price))
garch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(1, 1)))
garch_fit <- ugarchfit(garch_model, returns)
volatility_forecast <- sigma(garch_fit)
expected_volatility <- tail(volatility_forecast, 1)
```



```{r}
# Strategy Development
develop_strategy <- function(data, sr_levels, fib_levels, short_term_trend, long_term_trend, volatility) {
  last_price <- tail(data$Price, 1)
  rsi <- tail(data$RSI, 1)
  
  # Find nearest Fibonacci level
  nearest_fib <- fib_levels[which.min(abs(fib_levels - last_price))]
  nearest_fib_name <- names(nearest_fib)
  
  # Basic strategy rules
  if (short_term_trend == "Uptrend" && long_term_trend == "Uptrend") {
    if (last_price < min(sr_levels$resistance) && rsi < 70) {
      strategy <- paste("Consider Long Position - Near", nearest_fib_name, "Fibonacci level")
    } else if (last_price > max(sr_levels$resistance) && rsi > 70) {
      strategy <- "Hold / Take Profit"
    } else {
      strategy <- paste("Hold - Watch", nearest_fib_name, "Fibonacci level")
    }
  } else if (short_term_trend == "Downtrend" && long_term_trend == "Downtrend") {
    if (last_price > max(sr_levels$support) && rsi > 30) {
      strategy <- paste("Consider Short Position - Near", nearest_fib_name, "Fibonacci level")
    } else if (last_price < min(sr_levels$support) && rsi < 30) {
      strategy <- "Hold / Cover Short"
    } else {
      strategy <- paste("Hold - Watch", nearest_fib_name, "Fibonacci level")
    }
  } else {
    strategy <- paste("Neutral - Wait for Clear Trend, currently near", nearest_fib_name, "Fibonacci level")
  }
  
  # Adjust strategy based on volatility
  if (volatility > 0.02) {  # High volatility threshold
    strategy <- paste(strategy, "- Use Caution (High Volatility)")
  }
  
  return(strategy)
}
```
[2ND VERSION]
# Enhanced strategy rules
```{r}
generate_strategy <- function(short_term_trend, long_term_trend, last_price, sr_levels, rsi, volume, macd, nearest_fib_name, atr, market_sentiment, data, sr_levels, fib_levels, short_term_trend, long_term_trend, volatility) {
  base_strategy <- if (short_term_trend == long_term_trend) {
    if (short_term_trend == "Uptrend") "Bullish" else "Bearish"
  } else {
    "Neutral"
  }
  
  explanation <- paste(
    base_strategy,
    "- ",
    case_when(
      short_term_trend == "Uptrend" && long_term_trend == "Uptrend" -> {
        if (last_price < min(sr_levels$resistance) && rsi < 70) {
          sprintf("Potential buying opportunity near the %s Fibonacci level. The asset is in a strong uptrend with both short-term and long-term indicators aligned. Current price (%s) is below the nearest resistance (%s) and RSI (%s) indicates room for further upside. Consider a long position, but be cautious of potential pullbacks.", 
                  nearest_fib_name, round(last_price, 2), round(min(sr_levels$resistance), 2), round(rsi, 2))
        } else if (last_price > max(sr_levels$resistance) && rsi > 70) {
          sprintf("Asset has broken through key resistance levels and may be overbought. Current price (%s) is above the highest known resistance (%s) and RSI (%s) suggests potential exhaustion. Consider taking profits on existing long positions or implementing trailing stop-losses.", 
                  round(last_price, 2), round(max(sr_levels$resistance), 2), round(rsi, 2))
        } else {
          sprintf("The asset is consolidating near the %s Fibonacci level. While the overall trend is bullish, the current price action suggests a period of consolidation. Monitor for a potential breakout above %s or a pullback to %s.", 
                  nearest_fib_name, round(max(sr_levels$resistance), 2), round(min(sr_levels$support), 2))
        }
      },
      short_term_trend == "Downtrend" && long_term_trend == "Downtrend" -> {
        if (last_price > max(sr_levels$support) && rsi > 30) {
          sprintf("Potential shorting opportunity near the %s Fibonacci level. The asset is in a confirmed downtrend with both short-term and long-term indicators aligned. Current price (%s) is above the nearest support (%s) and RSI (%s) suggests room for further downside. Consider a short position, but be aware of potential short squeezes.", 
                  nearest_fib_name, round(last_price, 2), round(max(sr_levels$support), 2), round(rsi, 2))
        } else if (last_price < min(sr_levels$support) && rsi < 30) {
          sprintf("Asset has broken through key support levels and may be oversold. Current price (%s) is below the lowest known support (%s) and RSI (%s) indicates potential for a bounce. Consider covering short positions or looking for short-term contrarian long opportunities.", 
                  round(last_price, 2), round(min(sr_levels$support), 2), round(rsi, 2))
        } else {
          sprintf("The asset is consolidating near the %s Fibonacci level. While the overall trend is bearish, the current price action suggests a period of consolidation. Watch for a potential breakdown below %s or a relief rally to %s.", 
                  nearest_fib_name, round(min(sr_levels$support), 2), round(max(sr_levels$resistance), 2))
        }
      },
      TRUE -> {
        sprintf("Mixed signals present near the %s Fibonacci level. Short-term trend (%s) and long-term trend (%s) are in conflict. Current price (%s) is between support (%s) and resistance (%s). Consider waiting for a clearer trend to emerge or focus on shorter-term mean reversion strategies.", 
                nearest_fib_name, short_term_trend, long_term_trend, round(last_price, 2), round(max(sr_levels$support), 2), round(min(sr_levels$resistance), 2))
      }
    )
  )
  
  # Additional analysis based on volume, MACD, ATR, and market sentiment
  additional_analysis <- paste(
    "\n\nAdditional Analysis:",
    sprintf("\n- Volume: %s", if (volume > mean(volume) * 1.5) "Significantly above average, suggesting strong conviction in the current move." else "Within normal range, monitor for potential breakout volumes."),
    sprintf("\n- MACD: %s", if (macd$histogram > 0 && macd$signal > 0) "Bullish momentum increasing" else if (macd$histogram < 0 && macd$signal < 0) "Bearish momentum increasing" else "Momentum is mixed or weakening"),
    sprintf("\n- ATR: %s", if (atr > mean(atr) * 1.2) "Volatility is elevated, consider adjusting position sizes and stop-losses accordingly." else "Volatility is within normal range."),
    sprintf("\n- Market Sentiment: %s", market_sentiment)
  )
  
  strategy <- paste(explanation, additional_analysis)
  
  return(strategy)
}
```


```{r}
# Generate strategy
current_strategy
```


```{r}
# Print analysis results
cat("\nEnhanced Sugar Price Analysis Results:\n")
cat("Current Price:", tail(data$Price, 1), "\n")
cat("Short-term Trend:", short_term_trend, "\n")
cat("Long-term Trend:", long_term_trend, "\n")
cat("RSI:", round(tail(data$RSI, 1), 2), "\n")
cat("Overall Volatility:", round(expected_volatility * 100, 2), "%\n")
cat("Suggested Strategy:", current_strategy, "\n")
```


```{r}
# Visualize results
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Price, color = "Price")) +
  geom_line(aes(y = SMA20, color = "SMA 20")) +
  geom_line(aes(y = SMA50, color = "SMA 50")) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "grey", alpha = 0.2) +
  geom_hline(yintercept = sr_levels$support, linetype = "dashed", color = "green") +
  geom_hline(yintercept = sr_levels$resistance, linetype = "dashed", color = "red") +
  geom_hline(yintercept = fib_levels, linetype = "dotted", color = "purple") +
  scale_color_manual(values = c("Price" = "black", "SMA 20" = "blue", "SMA 50" = "red")) +
  labs(title = "Sugar Price with Technical Indicators, S/R Levels, and Fibonacci Retracements", 
       y = "Price", color = "Indicator") +
  theme_minimal() +
  annotate("text", x = max(data$Date), y = fib_levels, 
           label = paste0(names(fib_levels), " (", round(fib_levels, 2), ")"), 
           hjust = 1.1, vjust = 1, size = 3, color = "purple")

```
=========

```{r}
# Assuming 'data' is a data frame with a 'Price' column containing historical price data

# Calculate the high and low points for the Fibonacci retracement
high_point <- max(data$Price)
low_point <- min(data$Price)

# Define Fibonacci ratios
fib_ratios <- c(0.236, 0.382, 0.5, 0.618, 0.786)

# Calculate Fibonacci retracement levels
fib_levels <- low_point + (high_point - low_point) * fib_ratios

# Plot the price data
plot(data$Price, type = "l", xlab = "Time", ylab = "Price", main = "Fibonacci Retracement Chart")

# Add horizontal lines for Fibonacci levels
abline(h = fib_levels, col = "blue", lty = 2)

# Add high and low points
abline(h = high_point, col = "red", lty = 2)
abline(h = low_point, col = "red", lty = 2)
```
```{r}
# Generate Fibonacci sequence
library(ggplot2)
fib_sequence <- numeric(20)
fib_sequence[1:2] <- c(0, 1)
for (i in 3:20) {
  fib_sequence[i] <- fib_sequence[i - 1] + fib_sequence[i - 2]
}

# Calculate Fibonacci ratios
fib_ratios <- fib_sequence[2:length(fib_sequence)] / fib_sequence[1:(length(fib_sequence) - 1)]

# Create a data frame for Fibonacci ratios
fib_data <- data.frame(
  Ratio = fib_ratios,
  Index = 2:length(fib_sequence)
)

# Plot the Fibonacci ratios
ggplot(fib_data, aes(x = Index, y = Ratio)) +
  geom_line() +
  geom_point() +
  labs(title = "Fibonacci Ratios Chart", x = "Index", y = "Ratio") +
  theme_minimal()
```


[SHORT VERSION/ONLY SENTIMENT SUMMARY]

```{r}
# Load required libraries
library(readxl)
library(dplyr)
library(TTR)
library(ggplot2)
library(forecast)
library(rugarch)

# 1. Data Loading and Preprocessing
load_and_preprocess_data <- function(file_path) {
  # Read the Excel file
  raw_data <- read_excel("raw_sugar_price (1).xlsx")
  
  # Ensure the data is properly formatted
  data <- raw_data %>%
    mutate(Date = as.Date(Date)) %>%
    arrange(Date)
  
  # Extract the last year of data
  end_date <- max(data$Date)
  start_date <- end_date - 365
  data <- data %>% filter(Date >= start_date)
  
  return(data)
}

# 2. Technical Indicators Calculation
calculate_technical_indicators <- function(data) {
  data <- data %>%
    mutate(
      SMA20 = SMA(Price, n = 20),
      SMA50 = SMA(Price, n = 50),
      EMA20 = EMA(Price, n = 20)
    )
  
  bollinger <- BBands(data$Price)
  data$BB_Upper <- bollinger[, "up"]
  data$BB_Lower <- bollinger[, "dn"]
  data$RSI <- RSI(data$Price)
  
  macd <- MACD(data$Price)
  data$MACD <- macd[, "macd"]
  data$Signal <- macd[, "signal"]
  
  return(na.omit(data))
}

# 3. Fibonacci Retracement Levels
calculate_fibonacci_levels <- function(high, low) {
  diff <- high - low
  levels <- c(0, 0.236, 0.382, 0.5, 0.618, 0.786, 1)
  fib_levels <- high - diff * levels
  names(fib_levels) <- c("0%", "23.6%", "38.2%", "50%", "61.8%", "78.6%", "100%")
  return(fib_levels)
}

# 4. Support and Resistance Analysis
calculate_support_resistance <- function(prices, n_levels = 3) {
  price_changes <- diff(prices)
  support_levels <- prices[c(TRUE, price_changes >= 0)]
  resistance_levels <- prices[c(TRUE, price_changes <= 0)]
  
  find_clusters <- function(levels, n) {
    clusters <- kmeans(levels, centers = n)
    sort(clusters$centers)
  }
  
  top_support <- find_clusters(support_levels, n_levels)
  top_resistance <- find_clusters(resistance_levels, n_levels)
  
  list(support = top_support, resistance = top_resistance)
}

# 5. News Sentiment Analysis
input_news <- function(news_text) {
  list(list(
    title = "Market Update",
    summary = news_text,
    sentiment = NULL
  ))
}

analyze_news_sentiment <- function(news_list) {
  if (length(news_list) == 1 && is.null(news_list[[1]]$sentiment)) {
    text <- tolower(news_list[[1]]$summary)
    positive_words <- c("support", "strong", "bullish", "up", "increase", "higher", "bounced", "confident", "positive", "satisfied", "growth", "possible")
    negative_words <- c("falling", "bearish", "oversupplied", "down", "decrease", "downward", "decrease", "lower", "failed", "delayed", "disappointing", "nervous", "difficult", "concern", "restrictive", "damage")
    
    positive_count <- sum(sapply(positive_words, function(word) grepl(word, text)))
    negative_count <- sum(sapply(negative_words, function(word) grepl(word, text)))
    
    if (positive_count > negative_count) {
      sentiment <- "positive"
    } else if (negative_count > positive_count) {
      sentiment <- "negative"
    } else {
      sentiment <- "neutral"
    }
    
    return(list(counts = table(sentiment), overall = sentiment))
  } else {
    stop("Invalid input for sentiment analysis.")
  }
}

# 6. Trend Analysis
calculate_trend <- function(prices, short_period = 20, long_period = 50, sideways_threshold = 0.02) {
  short_ma <- tail(SMA(prices, n = short_period), 1)
  long_ma <- tail(SMA(prices, n = long_period), 1)
  recent_change <- (tail(prices, 1) - tail(prices, short_period)[1]) / tail(prices, short_period)[1]
  long_ma_slope <- (tail(long_ma, 1) - tail(long_ma, 10)[1]) / 10
  
  if (short_ma > long_ma * (1 + sideways_threshold) && recent_change > sideways_threshold && long_ma_slope > 0) {
    return("Uptrend")
  } else if (short_ma < long_ma * (1 - sideways_threshold) && recent_change < -sideways_threshold && long_ma_slope < 0) {
    return("Downtrend")
  } else {
    return("Sideways")
  }
}

# 7. Volatility Analysis
calculate_volatility <- function(prices) {
  returns <- diff(log(prices))
  garch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(1, 1)))
  garch_fit <- ugarchfit(garch_model, returns)
  volatility_forecast <- sigma(garch_fit)
  expected_volatility <- tail(volatility_forecast, 1)
  return(expected_volatility)
}

# 8. Strategy Development
develop_strategy <- function(data, sr_levels, fib_levels, short_term_trend, long_term_trend, volatility, news_sentiment, macd, signal) {
  last_price <- tail(data$Price, 1)
  rsi <- tail(data$RSI, 1)
  nearest_fib <- fib_levels[which.min(abs(fib_levels - last_price))]
  nearest_fib_name <- names(nearest_fib)
  
  # Determine the strategy based on trends, price levels, RSI, MACD, and Bollinger Bands
  if (short_term_trend == "Uptrend" && long_term_trend == "Uptrend") {
    if (last_price < min(sr_levels$resistance) && rsi < 70 && macd > signal) {
      strategy <- paste("Consider Long Position - Strong uptrend near", nearest_fib_name, "Fibonacci level, MACD supports upward momentum")
    } else if (last_price > max(sr_levels$resistance) && rsi > 70) {
      strategy <- "Hold / Take Partial Profits - Uptrend may be overextended, watch for reversal signals"
    } else {
      strategy <- paste("Hold Long - Uptrend intact, watch", nearest_fib_name, "Fibonacci level, monitor MACD for potential reversal")
    }
  } else if (short_term_trend == "Downtrend" && long_term_trend == "Downtrend") {
    if (last_price > max(sr_levels$support) && rsi > 30 && macd < signal) {
      strategy <- paste("Consider Short Position - Strong downtrend near", nearest_fib_name, "Fibonacci level, MACD supports downward momentum")
    } else if (last_price < min(sr_levels$support) && rsi < 30) {
      strategy <- "Hold / Cover Short - Downtrend may be overextended, watch for reversal signals"
    } else {
      strategy <- paste("Hold Short - Downtrend intact, watch", nearest_fib_name, "Fibonacci level, monitor MACD for potential reversal")
    }
  } else if (short_term_trend == "Sideways" || long_term_trend == "Sideways") {
    if (last_price > max(sr_levels$resistance)) {
      strategy <- paste("Consider Short - Price at upper range near", nearest_fib_name, "Fibonacci level, watch for breakout confirmation")
    } else if (last_price < min(sr_levels$support)) {
      strategy <- paste("Consider Long - Price at lower range near", nearest_fib_name, "Fibonacci level, watch for breakout confirmation")
    } else {
      strategy <- paste("Neutral - Range-bound market near", nearest_fib_name, "Fibonacci level, look for breakout signals")
    }
  } else {
    strategy <- paste("Mixed Signals - Short-term", short_term_trend, "vs Long-term", long_term_trend, "- Use caution and consider reducing position size")
  }
  
  # Adjust strategy based on volatility
  if (volatility > 0.02) {
    strategy <- paste(strategy, "- Use Caution (High Volatility)")
  }
  
  # Adjust strategy based on news sentiment
  if (news_sentiment == "positive") {
    strategy <- paste(strategy, "- Bullish news sentiment supports upward moves")
  } else if (news_sentiment == "negative") {
    strategy <- paste(strategy, "- Bearish news sentiment supports downward moves")
  } else {
    strategy <- paste(strategy, "- Neutral news sentiment")
  }
  
  return(strategy)
}

# 9. Visualization
create_price_chart <- function(data, sr_levels, fib_levels) {
  ggplot(data, aes(x = Date)) +
    geom_line(aes(y = Price, color = "Price")) +
    geom_line(aes(y = SMA20, color = "SMA 20")) +
    geom_line(aes(y = SMA50, color = "SMA 50")) +
    geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "grey", alpha = 0.2) +
    geom_hline(yintercept = sr_levels$support, linetype = "dashed", color = "green") +
    geom_hline(yintercept = sr_levels$resistance, linetype = "dashed", color = "red") +
    geom_hline(yintercept = fib_levels, linetype = "dotted", color = "purple") +
    scale_color_manual(values = c("Price" = "black", "SMA 20" = "blue", "SMA 50" = "red")) +
    labs(title = "Sugar Price with Technical Indicators, S/R Levels, and Fibonacci Retracements", 
         y = "Price", color = "Indicator") +
    theme_minimal() +
    annotate("text", x = max(data$Date), y = fib_levels, 
             label = paste0(names(fib_levels), " (", round(fib_levels, 2), ")"), 
             hjust = 1.1, vjust = 1, size = 3, color = "purple")
}

# 10. Main Analysis Function
analyze_sugar_price <- function(file_path, news_text) {
  # Load and preprocess data
  data <- load_and_preprocess_data(file_path)
  
  # Calculate technical indicators
  data <- calculate_technical_indicators(data)
  
  # Calculate Fibonacci levels
  fib_levels <- calculate_fibonacci_levels(max(data$Price), min(data$Price))
  
  # Calculate support and resistance levels
  sr_levels <- calculate_support_resistance(data$Price)
  
  # Analyze news sentiment
  sugar_news <- input_news(news_text)
  news_sentiment <- analyze_news_sentiment(sugar_news)
  
  # Calculate trends
  short_term_trend <- calculate_trend(data$Price, short_period = 20, long_period = 50)
  long_term_trend <- calculate_trend(data$Price, short_period = 50, long_period = 200)
  
  # Calculate volatility
  volatility <- calculate_volatility(data$Price)
  
  # Develop strategy
  strategy <- develop_strategy(data, sr_levels, fib_levels, short_term_trend, long_term_trend, volatility, news_sentiment$overall)
  
  # Create visualization
  chart <- create_price_chart(data, sr_levels, fib_levels)
  
  # Return results
  list(
    data = data,
    fib_levels = fib_levels,
    sr_levels = sr_levels,
    news_sentiment = news_sentiment,
    short_term_trend = short_term_trend,
    long_term_trend = long_term_trend,
    volatility = volatility,
    strategy = strategy,
    chart = chart
  )
}

# 11. Run the analysis
file_path <- "raw_sugar_price.xlsx"
news_text <- "Sugar prices have been volatile due to supply concerns and changing demand patterns."
results <- analyze_sugar_price(file_path, news_text)

# 12. Print results
cat("\nSugar Price Analysis Results:\n")
cat("Current Price:", tail(results$data$Price, 1), "\n")
cat("Short-term Trend:", results$short_term_trend, "\n")
cat("Long-term Trend:", results$long_term_trend, "\n")
cat("RSI:", round(tail(results$data$RSI, 1), 2), "\n")
cat("Volatility:", round(results$volatility * 100, 2), "%\n")
cat("News Sentiment:", results$news_sentiment$overall, "\n")
cat("Suggested Strategy:", results$strategy, "\n")

# 13. Display the chart
print(results$chart)
``` 
