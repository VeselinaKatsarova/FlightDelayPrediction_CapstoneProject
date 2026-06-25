library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(DataExplorer)
library(corrplot)
library(naniar)
library(tidyr)
library(caret)
library(xgboost)
library(randomForest)
library(ada)


# ----------------- Loading a dataset --------------------
setwd("D:/Classes and Classwork/2025 - 2026 - Semester 4/Capstone Project")
flight_delay <- read.csv("flight_data_2024_sample.csv")
setDT(flight_delay)

# ----------------- Exploring the dataset ----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial overview of the dataset
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(flight_delay)
dim(flight_delay)
names(flight_delay)
str(flight_delay)

"
fl_date is in the wrong type: chr -> date
"

# -- missing values
colSums(is.na(flight_delay)) # NAs - name of the column + number missing values
names(which(colSums(is.na(flight_delay)) > 0))
# procentages missing data
colSums(is.na(flight_delay))/nrow(flight_delay) * 100
# plot
plot_missing(flight_delay)

# empty values - print columns name with NAs and empty values
names(flight_delay)[colSums(flight_delay == "") > 0]

missing_values <- c(
  "dep_time", "dep_delay", "taxi_out", "wheels_off",
  "wheels_on", "taxi_in", "arr_time", "arr_delay",
  "actual_elapsed_time", "air_time"
)
# sum missing values diverted
flight_delay %>%
  filter(diverted == 1) %>%
  summarise(across(all_of(missing_values), ~sum(is.na(.))))

# sum missing values cancelled
flight_delay %>%
  filter(cancelled == 1) %>%
  summarise(across(all_of(missing_values), ~sum(is.na(.))))

"
The dataset has missing values in columns: dep_time, dep_delay, taxi_out, wheels_off,         
  wheels_on, taxi_in, arr_time, arr_delay, actual_elapsed_time and air_time. The procentage
  missing values is not big less than 2%.
The cancelation_code column do not have missing values insted it has empty values. 
There is a possibilities that the columns with missing values are coused by canseled
  flights. We conform that there are a lot of missing values caused by cancelation and diversion.
  
"

# -- duplicates
sum(duplicated(flight_delay))

"
There are not any duplicated in the dataset.
"
summary(flight_delay)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      Column Exploring
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Variables overview ----
flight_delay %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free")

numeric_vars <- sapply(flight_delay, is.numeric)
boxplot(flight_delay[, ..numeric_vars],
        las = 2,
        main = "Numeric Variable Boxplots")

"
The histogram and boxplot shows that there are ouliers in some of the numeric variables.
  There are some exreme one as well.
"

# ---- Delays EDA ----

# is_delay is created from arrival delay column where flight had landed with 15 min late
flight_delay$is_delayed <- ifelse(flight_delay$arr_delay > 15, 1, 0)

# EDA
table(flight_delay$is_delayed)
prop.table(table(flight_delay$is_delayed))

ggplot(flight_delay, aes(x = factor(is_delayed))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Delayed vs Non-Delayed Flights",
    x = "Delayed",
    y = "Count"
  )
ggplot(flight_delay, aes(x = arr_delay)) +
  geom_histogram(bins = 100, fill = "tomato") +
  coord_cartesian(xlim = c(-50, 300))

"
There are only ~21% are delayed and ~79% are not. The dataset it unbalanced.
Arrival delays are heavily right-skewed, with most flights arriving on time or 
  with small delays, while a small number of flights experience extreme delays 
  exceeding 200 minutes.
"

# ---- cancelled & diverced EDA ----
table(flight_delay$cancelled)
prop.table(table(flight_delay$cancelled))

table(flight_delay$cancellation_code)
ggplot(flight_delay, aes(cancellation_code)) +
  geom_bar(fill = "tomato") +
  labs(
    title = "Cancellation Reasons",
    x = "Cancellation Code",
    y = "Count"
  )

# cancelation rate by month
cancel_month <- flight_delay %>%
  group_by(month) %>%
  summarise(
    cancel_rate = mean(cancelled, na.rm = TRUE)
  )

ggplot(cancel_month,
       aes(x = month, y = cancel_rate)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Overall Flight Cancellation Rate by Month",
    x = "Month",
    y = "Cancellation Rate"
  ) +
  theme_minimal()

# cancelation reason vs month
cancel_reason_month <- flight_delay %>%
  filter(cancelled == 1) %>%
  group_by(month, cancellation_code) %>%
  summarise(
    total_cancelled = n(),
    .groups = "drop"
  )

ggplot(cancel_reason_month,
       aes(x = month,
           y = total_cancelled,
           color = cancellation_code,
           group = cancellation_code)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Flight Cancellations by Month and Reason",
    x = "Month",
    y = "Cancelled Flights",
    color = "Reason"
  ) +
  theme_minimal()

"
There are not many canselled flight in the dataset - around ~0.01%. And the 
  reason for their cancellation vareys between:
  - A (Carrier/Airline - Mechanical problems, aircraft maintenance, crew shortages, or baggage delays.)
  - B (Weather - Heavy snowstorms, severe thunderstorms, hurricanes, or low-visibility fog.)
  - C (National Aviation System - Air traffic control delays, airport runway closures, or heavy airport congestion.)
  As the cancelation column there are more not cancelled flight. However the most common delay cancelled reason is
  B (Weather).
The season with most cancelled flight is summer. Furthermore, summber is the season when both concelation reasons 
  A and C is highest. However, the winter is the season when there are more B cancelation.
"

mean(flight_delay$diverted) * 100

diversion_airline <- flight_delay %>%
  group_by(op_unique_carrier) %>%
  summarise(
    diversion_rate = mean(diverted, na.rm = TRUE) * 100,
    total_flights = n()
  ) %>%
  #filter(total_flights > 1000) %>%
  arrange(desc(diversion_rate))

ggplot(diversion_airline,
       aes(x = reorder(op_unique_carrier, diversion_rate),
           y = diversion_rate)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Diversion Rate by Airline",
    subtitle = "Airlines",
    x = "Airline",
    y = "Diversion Rate (%)"
  ) +
  theme_minimal()

"
Only 0.42% of flights were diverted, indicating that diversions are relatively uncommon 
  operational events. The most of the airlines have diverted flight. AS airline has the
  most diverted flights.
"
# missing variables caused by cancelled & diverced
flight_delay %>%
  filter(cancelled == 1) %>%
  summarise(
    missing_arrival = sum(is.na(arr_delay)),
  )
flight_delay %>%
  filter(diverted == 1) %>%
  summarise(
    missing_arrival = sum(is.na(arr_delay)),
  )

"
Cancelled flights result missing values in arrival-related variables.
"

# ---- Cleaning dataset ----
# remove the canseled flights
clean_flight_dt <- flight_delay[flight_delay$cancelled != 1, ]
clean_flight_dt <- clean_flight_dt[clean_flight_dt$diverted != 1, ]
clean_flight_dt$cancelled <- NULL
clean_flight_dt$cancellation_code <- NULL
clean_flight_dt$diverted <- NULL

"
Rmoving cancelled, cancellation_code and diverted columns because there are no need
  for our flight delay prediction model. Reasonign:
  cancelled - flight have not happend
  cancellation_code - reason for cancelation
  ==> not happened flights
  diverted - flight change its original plan and land at a different airport instead 
             of its scheduled destination => delay is inevitably
"

# check for missing values after removing columns: cancelled, cancellation_code, diverted 
colSums(is.na(clean_flight_dt))
# plot
plot_missing(clean_flight_dt)

"
The missing variables are gone after removing cancelation and diverted flights. So, 
  the initial conclusion was correct - cancelation and diverted flights cause the
  missing values in the dataset.
"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           EDA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Time/Dates EDA ----
# monts vs delays
monthly_delay <- clean_flight_dt %>%
  group_by(month) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))

ggplot(monthly_delay, aes(month, avg_delay)) +
  geom_line(color = "blue") +
  geom_point()

# weekents vs delays
ggplot(clean_flight_dt, aes(factor(day_of_week), arr_delay)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-20, 120))
"
The most of the delays are during the summer months. During the authom the flights
  are huge decline. It can be caused by the lack of holidays. 
Friday is one of the most bisiest week days even though there all of them have delays.
 The big supprize is that the less less delay are in Saturday.
"

# time in hours
clean_flight_dt$dep_hour <- floor(clean_flight_dt$crs_dep_time / 100)

# arrival delay vs departure hours
hourly_delay <- clean_flight_dt %>%
  group_by(dep_hour) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    flights = n()
  )

ggplot(hourly_delay,
       aes(dep_hour, avg_delay)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(aes(size = flights),
             color = "darkred") +
  labs(
    title = "Departure Hour vs Average Arrival Delay",
    x = "Departure Hour",
    y = "Average Arrival Delay"
  ) +
  theme_minimal()

hourly_delay <- clean_flight_dt %>%
  group_by(dep_hour) %>%
  summarise(
    mean_delay = mean(arr_delay, na.rm = TRUE),
    median_delay = median(arr_delay, na.rm = TRUE)
  )

# delay probability by departure hour
delay_prob <- clean_flight_dt %>%
  group_by(dep_hour) %>%
  summarise(
    pct_delayed = mean(is_delayed) * 100,
    flights = n()
  ) 

ggplot(delay_prob,
       aes(dep_hour, pct_delayed)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(aes(size = flights),
             color = "darkblue") +
  labs(
    title = "Delay Probability by Departure Hour",
    x = "Departure Hour",
    y = "Percentage Delayed"
  ) +
  theme_minimal()

# smaller group
delay_prob <- clean_flight_dt %>%
  group_by(dep_hour) %>%
  summarise(
    pct_delayed = mean(is_delayed) * 100,
    flights = n()
  ) %>%
  filter(flights >= 50)

ggplot(delay_prob,
       aes(dep_hour, pct_delayed)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(aes(size = flights),
             color = "darkblue") +
  labs(
    title = "Delay Probability by Departure Hour",
    x = "Departure Hour",
    y = "Percentage Delayed"
  ) +
  theme_minimal()

"
The average delay increace during the day. However, there is a big gap between early moring
  flight delay and daily delays even though there are less flight. That suggest that there 
  are some outliers.
The delay probability by departure hour plot is simmilar to the arrival delay vs departure 
  hours. After, we filtering the data we can see that there is increasing delays during the day
  with the peak areoun 20:00. 
"

# ---- Airline and Airport EDA ----
# - airline
table(flight_delay$op_unique_carrie)
length(unique(flight_delay$op_unique_carrier))

barplot(table(flight_delay$op_unique_carrie),
        xlab = "Airline",
        ylab = "Frequency",
        main = "Airline Frequency",
        col = "blue")

# delay vs airlines
carrier_delay <- clean_flight_dt %>%
  group_by(op_unique_carrier) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))

ggplot(carrier_delay,
       aes(reorder(op_unique_carrier, avg_delay), avg_delay)) +
  geom_col(fill = "darkgreen") +
  coord_flip()
table(clean_flight_dt$op_unique_carrier)

"
There are 15 airline companies in the dataset. WN is with most flights and the one
  with the less is HA. However, the airline with most delays is AA (the thirth most flights) 
  and the one with less is YX. This suggest that there is no connection between frequency
  and delay.
"

# - Airport
airport_delay <- clean_flight_dt %>%
  group_by(origin) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
            flights = n()) %>%
  filter(flights > 100)

ggplot(airport_delay,
       aes(reorder(origin, avg_delay), avg_delay)) +
  geom_col(fill = "purple") +
  coord_flip()

"
The dataplot present the ariport with the most delays. Destination airports appear to affect arrival 
  punctuality. Flights arriving at MIA, CLT, and DFW have the longest average delays, while flights 
  arriving at JFK tend to arrive slightly earlier. Although most destinations have average delays of 
  less than 10 minutes, the differences between airports indicate that airport-specific factors, such 
  as congestion, weather conditions, and operational efficiency, can affect arrival efficiency.
"

# ---- Correlation ----
num_cols <- clean_flight_dt %>%
  select(
    dep_delay,
    taxi_out,
    taxi_in,
    air_time,
    distance,
    arr_delay
  )

cor_matrix <- cor(num_cols, use = "complete.obs")
corrplot::corrplot(cor_matrix, method = "color")

"
A strong positive correlation was observed between departure delay and arrival delay, 
  indicating the connection between delays from departure to arrival. Flight distance and flight 
  time were highly correlated, reflecting the expected physical behavior of the flight. 
  Other operational variables showed weak linear relationships with arrival delay, 
  suggesting that delays are due more to operational disruptions than to flight duration.
"                         
# ---- Outliers ----
ggplot(clean_flight_dt, aes(y = arr_delay)) +
  geom_boxplot()

# raw dataset
flight_delay %>%
  filter(arr_delay == max(arr_delay, na.rm = TRUE)) %>%
  select(
    fl_date,
    origin,
    dest,
    op_unique_carrier,
    arr_delay,
    dep_delay,
    cancelled,
    diverted
  )
# clean dataset
clean_flight_dt %>%
  filter(arr_delay > 1000) %>%
  select(
    fl_date,
    origin,
    dest,
    op_unique_carrier,
    dep_delay,
    arr_delay
  )

"
The boxplot of the arr_delay shows that there are outliers reaching 1000. However,
  there is one extreme outlier ~2000.
An extreme arrival delay of approximately 2014 minutes was identified. Inspection 
  showed that the flight was neither cancelled nor diverted, and the departure and 
  arrival delays were internally consistent, suggesting a genuine operational 
  disruption rather than a data quality issue. The observation was therefore retained.
"

# ---- Data Cleaning ----
# columns that provide more than needed informarion (there are columns that carry simmilar 
#     information) are removed
clean_flight_dt$fl_date <- NULL
clean_flight_dt$year <- NULL # only one year
clean_flight_dt$origin_city_name <- NULL
clean_flight_dt$dest_city_name <- NULL
clean_flight_dt$origin_state_nm <- NULL
clean_flight_dt$dest_state_nm <- NULL
clean_flight_dt$op_carrier_fl_num <- NULL # not useful can lead to leakege

# delay reason columns are removed - data 
delay_cols <- c("carrier_delay", "weather_delay", "nas_delay", 
                "security_delay", "late_aircraft_delay")
clean_flight_dt <- clean_flight_dt %>%
  select(-all_of(delay_cols))

# time
clean_flight_dt <- clean_flight_dt %>%
  mutate(
    crs_dep_mins = floor(crs_dep_time / 100) * 60 + crs_dep_time %% 100,
    crs_arr_mins = floor(crs_arr_time / 100) * 60 + crs_arr_time %% 100
  )

clean_flight_dt <- clean_flight_dt %>%
  mutate(
    dep_sin = sin(2 * pi * crs_dep_mins / 1440),
    dep_cos = cos(2 * pi * crs_dep_mins / 1440),
    
    arr_sin = sin(2 * pi * crs_arr_mins / 1440),
    arr_cos = cos(2 * pi * crs_arr_mins / 1440)
  )

clean_flight_dt <- clean_flight_dt %>%
  select(
    -crs_dep_time,
    -crs_arr_time
  )

# ----------------- Feature Engineering ----------------

# -- time of the day
clean_flight_dt$time_of_day <- case_when(
  clean_flight_dt$dep_hour < 6 ~ "night",
  clean_flight_dt$dep_hour < 12 ~ "morning",
  clean_flight_dt$dep_hour < 18 ~ "afternoon",
  TRUE ~ "evening"
)

# -- routs
clean_flight_dt$route <- paste(clean_flight_dt$origin, clean_flight_dt$dest, sep = "_")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Features +
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"
Thise are some additional features that can helpfull for prediction better delay. However,
  season and distance can add only a little because there are columns that describe them.
  (mey be used or not in the model)
"

# seasons
clean_flight_dt$season <- case_when(
  clean_flight_dt$month %in% c(12,1,2) ~ "winter",
  clean_flight_dt$month %in% c(3,4,5) ~ "spring",
  clean_flight_dt$month %in% c(6,7,8) ~ "summer",
  TRUE ~ "fall"
)
# Distance Categories
clean_flight_dt$distance_group <- case_when(
  clean_flight_dt$distance < 500 ~ "short",
  clean_flight_dt$distance < 1500 ~ "medium",
  TRUE ~ "long"
)

airport_volume <- clean_flight_dt %>%
  group_by(origin) %>%
  summarise(origin_flights = n())
clean_flight_dt <- left_join(clean_flight_dt, airport_volume, by = "origin")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Encoding
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_flight_dt$origin <- as.factor(clean_flight_dt$origin)
clean_flight_dt$dest <- as.factor(clean_flight_dt$dest)
clean_flight_dt$op_unique_carrier <- as.factor(clean_flight_dt$op_unique_carrier)
clean_flight_dt$route   <- as.factor(clean_flight_dt$route)
clean_flight_dt$time_of_day <- as.factor(clean_flight_dt$time_of_day)

# ----------------- Data Cleaning Final ----------------
# Target
y <- clean_flight_dt$arr_delay

# BEFORE departure
before_departure_X <- clean_flight_dt %>%
  select(
    -arr_delay,
    -dep_delay,
    -dep_time,
    -taxi_out,
    -wheels_off,
    -air_time,
    -actual_elapsed_time,
    -wheels_on,
    -taxi_in,
    -arr_time,
    -dep_hour
  )

# AFTER departure
after_departure_X <- clean_flight_dt %>%
  select(
    -arr_delay,
    -air_time,
    -actual_elapsed_time,
    -wheels_on,
    -taxi_in,
    -arr_time,
    -dep_delay
  )


after_departure_X <- after_departure_X %>%
  mutate(
    wheels_off_mins =
      floor(wheels_off / 100) * 60 +
      wheels_off %% 100,
    
    wheels_off_sin =
      sin(2*pi*wheels_off_mins/1440),
    
    wheels_off_cos =
      cos(2*pi*wheels_off_mins/1440)
  )

after_departure_X <- after_departure_X %>%
  select(
    -dep_time,
    -dep_hour,
    -crs_dep_mins,
    -wheels_off,
    -wheels_off_mins
  )
names(before_departure_X)
names(after_departure_X)
"
Here are two different prediction scenarios (two different datasets/models)

"

# -------- Historical features (Train only) ---------
#!!!!! convert into Target encoding for the model + replacement (route become route_avg_delay) or keep both 
route_stats <- train %>%
  group_by(route) %>%
  summarise(
    route_avg_delay = mean(arr_delay, na.rm = TRUE)
  )

# # Optional
carrier_stats <- train %>%
  group_by(carrier) %>%
  summarise(carrier_avg_delay = mean(arr_delay, na.rm = TRUE))

origin_stats <- train %>%
  group_by(origin) %>%
  summarise(origin_avg_delay = mean(arr_delay, na.rm = TRUE))

#---------------------------------------------------------------





if (!"is_delayed" %in% names(clean_flight_dt)) {
  clean_flight_dt$is_delayed <- ifelse(clean_flight_dt$arr_delay > 15, 1, 0)
}

set.seed(42)
train_idx <- createDataPartition(clean_flight_dt$is_delayed, p = 0.8, list = FALSE)
train <- clean_flight_dt[train_idx, ]
test  <- clean_flight_dt[-train_idx, ]

# ==================== UPDATED HELPER FUNCTIONS ====================

prepare_xgb_data <- function(data, exclude_post_dep = FALSE) {
  df <- as.data.frame(data)
  if (exclude_post_dep) {
    post_cols <- c("dep_delay", "dep_time", "taxi_out", "wheels_off", 
                   "air_time", "actual_elapsed_time", "wheels_on", 
                   "taxi_in", "arr_time", "dep_hour")
    df <- df %>% select(-any_of(post_cols))
  } else {
    df <- df %>% select(-any_of(c("air_time", "actual_elapsed_time", 
                                  "wheels_on", "taxi_in", "arr_time", "dep_delay")))
  }
  df <- df %>% select(-any_of(c("arr_delay")))
  
  char_cols <- sapply(df, function(x) is.factor(x) || is.character(x))
  for (col in names(df)[char_cols]) {
    df[[col]] <- as.numeric(as.factor(df[[col]]))
  }
  
  
  if ("is_delayed" %in% names(df)) {
    X <- as.matrix(df %>% select(-is_delayed))
    y <- df$is_delayed
  } else {
    X <- as.matrix(df)
    y <- NULL
  }
  list(X = X, y = y)
}

prepare_ml_data <- function(data, exclude_post_dep = FALSE, origin_levels = NULL, dest_levels = NULL) {
  df <- as.data.frame(data)
  
  if (exclude_post_dep) {
    post_cols <- c("dep_delay", "dep_time", "taxi_out", "wheels_off", 
                   "air_time", "actual_elapsed_time", "wheels_on", 
                   "taxi_in", "arr_time", "dep_hour")
    df <- df %>% select(-any_of(post_cols))
  } else {
    df <- df %>% select(-any_of(c("air_time", "actual_elapsed_time", 
                                  "wheels_on", "taxi_in", "arr_time", "dep_delay")))
  }
  
  drop_cols <- c("route", "fl_date", "origin_city_name", "origin_state_nm", "dest_city_name", "dest_state_nm")
  df <- df %>% select(-any_of(drop_cols))
  df <- df %>% select(-any_of(c("arr_delay")))
  
  if (!is.null(origin_levels)) {
    df$origin <- ifelse(df$origin %in% origin_levels, df$origin, "OTHER")
  } else {
    top_origins <- names(sort(table(df$origin), decreasing = TRUE))[1:30]
    df$origin <- ifelse(df$origin %in% top_origins, df$origin, "OTHER")
  }
  
  if (!is.null(dest_levels)) {
    df$dest <- ifelse(df$dest %in% dest_levels, df$dest, "OTHER")
  } else {
    top_dests <- names(sort(table(df$dest), decreasing = TRUE))[1:30]
    df$dest <- ifelse(df$dest %in% top_dests, df$dest, "OTHER")
  }
  
  char_cols <- sapply(df, function(x) is.character(x) || is.logical(x))
  for (col in names(df)[char_cols]) {
    df[[col]] <- as.factor(df[[col]])
  }
  
 
  if ("is_delayed" %in% names(df)) {
    df$is_delayed <- as.factor(df$is_delayed)
  }
  
  df <- droplevels(df)
  df
}



# ==================== DATA PREPARATION ====================

# 1. Classification Data Preparation (Pre-Departure ONLY)
train_ml_before <- prepare_ml_data(train, exclude_post_dep = TRUE)
test_ml_before  <- prepare_ml_data(test, exclude_post_dep = TRUE, 
                                   origin_levels = unique(train_ml_before$origin),
                                   dest_levels = unique(train_ml_before$dest))


for(col in names(train_ml_before)) {
  if(is.factor(train_ml_before[[col]])) {
    test_ml_before[[col]] <- factor(test_ml_before[[col]], levels = levels(train_ml_before[[col]]))
  }
}

# 2. XGBoost Binary & Regression Data Matrix Preparation
xgb_train_before <- prepare_xgb_data(train, exclude_post_dep = TRUE)
xgb_test_before  <- prepare_xgb_data(test, exclude_post_dep = TRUE)

# Classification matrices
dtrain_before_bin <- xgb.DMatrix(xgb_train_before$X, label = xgb_train_before$y)
dtest_before_bin  <- xgb.DMatrix(xgb_test_before$X, label = xgb_test_before$y)

# Regression matrices: target label is continuous 'arr_delay' 
dtrain_before_reg <- xgb.DMatrix(xgb_train_before$X, label = train$arr_delay)
dtest_before_reg  <- xgb.DMatrix(xgb_test_before$X, label = test$arr_delay)


# ==================== MODEL TRAINING (BEFORE DEPARTURE) ====================

# ---- 1. XGBoost Classification ----

pos_weight <- sum(train$is_delayed == 0) / sum(train$is_delayed == 1)
balanced_pos_weight <- sqrt(pos_weight)

model_xgb_before <- xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "auc",
                eta = 0.05, 
                max_depth = 5,                         # Reduced from 7 to prevent overfitting
                scale_pos_weight = balanced_pos_weight, # Tuned down to suppress False Positives
                subsample = 0.8,                        # Row regularization
                colsample_bytree = 0.8),               # Feature regularization
  data = dtrain_before_bin, nrounds = 500, early_stopping_rounds = 30,
  watchlist = list(train = dtrain_before_bin, test = dtest_before_bin), verbose = 0
)

cat("Evaluating XGBoost Classification Before Departure...\n")
pred_xgb_before <- predict(model_xgb_before, dtest_before_bin)

# ---- 2. XGBoost Regression ----

model_reg_before <- xgb.train(
  params = list(objective = "reg:squarederror", eval_metric = "rmse",
                eta = 0.05, max_depth = 5,             # Reduced from 7 to prevent overfitting
                subsample = 0.8, colsample_bytree = 0.8),
  data = dtrain_before_reg, nrounds = 500, early_stopping_rounds = 30,
  watchlist = list(train = dtrain_before_reg, test = dtest_before_reg), verbose = 0
)

pred_areg_before <- predict(model_reg_before, dtest_before_reg)

# ---- 3. Random Forest Classification ----

model_rf_before <- randomForest(is_delayed ~ ., data = train_ml_before, ntree = 100, importance = FALSE)
pred_rf_prob_before <- predict(model_rf_before, newdata = test_ml_before, type = "prob")[, "1"]

# ---- 4. AdaBoost Classification ----

model_ada_before <- ada(is_delayed ~ ., data = train_ml_before, iter = 50, type = "gentle")
pred_ada_prob_before <- predict(model_ada_before, newdata = test_ml_before, type = "prob")[, 2] 


# ==================== OUTPUTS & METRICS (BEFORE DEPARTURE) ====================


cat("\n=== REGRESSION PERFORMANCE SUMMARY (BEFORE DEPARTURE) ===\n")
cat("Regression - RMSE:", round(sqrt(mean((pred_areg_before - test$arr_delay)^2)), 2), "minutes\n")
cat("Regression - MAE: ", round(mean(abs(pred_areg_before - test$arr_delay)), 2), "minutes\n\n")


library(pROC)
roc_xgb_before <- roc(test$is_delayed, pred_xgb_before, quiet = TRUE)
roc_rf_before  <- roc(test_ml_before$is_delayed, pred_rf_prob_before, quiet = TRUE)
roc_ada_before <- roc(test_ml_before$is_delayed, pred_ada_prob_before, quiet = TRUE)

thresh_xgb_before <- coords(roc_xgb_before, "best", best.method = "youden", ret = "threshold")[[1]]
thresh_rf_before  <- coords(roc_rf_before, "best", best.method = "youden", ret = "threshold")[[1]]
thresh_ada_before <- coords(roc_ada_before, "best", best.method = "youden", ret = "threshold")[[1]]


cm_xgb_before <- confusionMatrix(factor(ifelse(pred_xgb_before > thresh_xgb_before, 1, 0), levels = c("0", "1")), 
                                 factor(test$is_delayed, levels = c("0", "1")), positive = "1")
cm_rf_before  <- confusionMatrix(factor(ifelse(pred_rf_prob_before > thresh_rf_before, 1, 0), levels = c("0", "1")), 
                                 factor(test_ml_before$is_delayed, levels = c("0", "1")), positive = "1")
cm_ada_before <- confusionMatrix(factor(ifelse(pred_ada_prob_before > thresh_ada_before, 1, 0), levels = c("0", "1")), 
                                 factor(test_ml_before$is_delayed, levels = c("0", "1")), positive = "1")


comparison_table_before <- data.frame(
  Model             = c("XGBoost", "Random Forest", "AdaBoost"),
  Opt_Thresh        = c(thresh_xgb_before, thresh_rf_before, thresh_ada_before),
  Accuracy          = c(cm_xgb_before$overall["Accuracy"], cm_rf_before$overall["Accuracy"], cm_ada_before$overall["Accuracy"]),
  Sensitivity_Rec   = c(cm_xgb_before$byClass["Sensitivity"], cm_rf_before$byClass["Sensitivity"], cm_ada_before$byClass["Sensitivity"]),
  Specificity       = c(cm_xgb_before$byClass["Specificity"], cm_rf_before$byClass["Specificity"], cm_ada_before$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_xgb_before$byClass["Balanced Accuracy"], cm_rf_before$byClass["Balanced Accuracy"], cm_ada_before$byClass["Balanced Accuracy"]),
  AUC               = c(as.numeric(auc(roc_xgb_before)), as.numeric(auc(roc_rf_before)), as.numeric(auc(roc_ada_before)))
)

cat("=== CLASSIFICATION PERFORMANCE SUMMARY (BEFORE DEPARTURE) ===\n")
print(comparison_table_before, row.names = FALSE)

#----------------------------------------------------------------------------------

#after departure models

# ==================== DATA PREPARATION ====================


train_ml_after <- prepare_ml_data(train, exclude_post_dep = FALSE)
test_ml_after  <- prepare_ml_data(test, exclude_post_dep = FALSE, 
                                  origin_levels = unique(train_ml_after$origin),
                                  dest_levels = unique(train_ml_after$dest))


for(col in names(train_ml_after)) {
  if(is.factor(train_ml_after[[col]])) {
    test_ml_after[[col]] <- factor(test_ml_after[[col]], levels = levels(train_ml_after[[col]]))
  }
}

# 2. XGBoost Binary & Regression Data Matrix Preparation
xgb_train_after <- prepare_xgb_data(train, exclude_post_dep = FALSE)
xgb_test_after  <- prepare_xgb_data(test, exclude_post_dep = FALSE)

# Classification matrices
dtrain_after_bin <- xgb.DMatrix(xgb_train_after$X, label = xgb_train_after$y)
dtest_after_bin  <- xgb.DMatrix(xgb_test_after$X, label = xgb_test_after$y)

# Regression matrices
dtrain_after_reg <- xgb.DMatrix(xgb_train_after$X, label = train$arr_delay)
dtest_after_reg  <- xgb.DMatrix(xgb_test_after$X, label = test$arr_delay)




# ---- 1. XGBoost Classification ----
model_xgb_after <- xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "auc",
                eta = 0.05, 
                max_depth = 5,                         # Reduced from 7
                scale_pos_weight = balanced_pos_weight, # Balanced weight
                subsample = 0.8, colsample_bytree = 0.8),
  data = dtrain_after_bin, nrounds = 500, early_stopping_rounds = 30,
  watchlist = list(train = dtrain_after_bin, test = dtest_after_bin), verbose = 0
)

pred_xgb_after <- predict(model_xgb_after, dtest_after_bin)

# ---- 2. XGBoost Regression ----

model_reg_after <- xgb.train(
  params = list(objective = "reg:squarederror", eval_metric = "rmse",
                eta = 0.05, max_depth = 5,
                subsample = 0.8, colsample_bytree = 0.8),
  data = dtrain_after_reg, nrounds = 500, early_stopping_rounds = 30,
  watchlist = list(train = dtrain_after_reg, test = dtest_after_reg), verbose = 0
)

pred_areg <- predict(model_reg_after, dtest_after_reg)

# ---- 3. Random Forest Classification ----

model_rf_after <- randomForest(is_delayed ~ ., data = train_ml_after, ntree = 100, importance = FALSE)
pred_rf_prob_after <- predict(model_rf_after, newdata = test_ml_after, type = "prob")[, "1"]

# ---- 4. AdaBoost Classification ----

model_ada_after <- ada(is_delayed ~ ., data = train_ml_after, iter = 50, type = "gentle")
pred_ada_prob_after <- predict(model_ada_after, newdata = test_ml_after, type = "prob")[, 2]


# ==================== OUTPUTS & METRICS (AFTER DEPARTURE) ====================


cat("\n=== REGRESSION PERFORMANCE SUMMARY (AFTER DEPARTURE) ===\n")
cat("Regression - RMSE:", round(sqrt(mean((pred_areg - test$arr_delay)^2)), 2), "minutes\n")
cat("Regression - MAE: ", round(mean(abs(pred_areg - test$arr_delay)), 2), "minutes\n\n")


roc_xgb_after <- roc(test$is_delayed, pred_xgb_after, quiet = TRUE)
roc_rf_after  <- roc(test_ml_after$is_delayed, pred_rf_prob_after, quiet = TRUE)
roc_ada_after <- roc(test_ml_after$is_delayed, pred_ada_prob_after, quiet = TRUE)


thresh_xgb_after <- coords(roc_xgb_after, "best", best.method = "youden", ret = "threshold")[[1]]
thresh_rf_after  <- coords(roc_rf_after, "best", best.method = "youden", ret = "threshold")[[1]]
thresh_ada_after <- coords(roc_ada_after, "best", best.method = "youden", ret = "threshold")[[1]]


cm_xgb_after <- confusionMatrix(factor(ifelse(pred_xgb_after > thresh_xgb_after, 1, 0), levels = c("0", "1")), 
                                factor(test$is_delayed, levels = c("0", "1")), positive = "1")
cm_rf_after  <- confusionMatrix(factor(ifelse(pred_rf_prob_after > thresh_rf_after, 1, 0), levels = c("0", "1")), 
                                factor(test_ml_after$is_delayed, levels = c("0", "1")), positive = "1")
cm_ada_after <- confusionMatrix(factor(ifelse(pred_ada_prob_after > thresh_ada_after, 1, 0), levels = c("0", "1")), 
                                factor(test_ml_after$is_delayed, levels = c("0", "1")), positive = "1")


comparison_table_after <- data.frame(
  Model             = c("XGBoost", "Random Forest", "AdaBoost"),
  Opt_Thresh        = c(thresh_xgb_after, thresh_rf_after, thresh_ada_after),
  Accuracy          = c(cm_xgb_after$overall["Accuracy"], cm_rf_after$overall["Accuracy"], cm_ada_after$overall["Accuracy"]),
  Sensitivity_Rec   = c(cm_xgb_after$byClass["Sensitivity"], cm_rf_after$byClass["Sensitivity"], cm_ada_after$byClass["Sensitivity"]),
  Specificity       = c(cm_xgb_after$byClass["Specificity"], cm_rf_after$byClass["Specificity"], cm_ada_after$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_xgb_after$byClass["Balanced Accuracy"], cm_rf_after$byClass["Balanced Accuracy"], cm_ada_after$byClass["Balanced Accuracy"]),
  AUC               = c(as.numeric(auc(roc_xgb_after)), as.numeric(auc(roc_rf_after)), as.numeric(auc(roc_ada_after)))
)

cat("=== CLASSIFICATION PERFORMANCE SUMMARY (AFTER DEPARTURE) ===\n")
print(comparison_table_after, row.names = FALSE)










# ====================PREDICTION FUNCTION ====================
predict_flight_delay <- function(new_data, bin_model, reg_model, model_type = "xgb", is_after = FALSE, train_reference_df = NULL) {
  
  if (model_type == "xgb") {
    prep_bin <- prepare_xgb_data(new_data, exclude_post_dep = !is_after)
    prob <- predict(bin_model, xgb.DMatrix(prep_bin$X))
    
  } else if (model_type %in% c("rf", "ada")) {
    if (is.null(train_reference_df)) {
      stop("For 'rf' or 'ada' models, you must provide train_reference_df to align factor levels.")
    }
    
    prep_ml <- prepare_ml_data(new_data, exclude_post_dep = !is_after, 
                               origin_levels = unique(train_reference_df$origin),
                               dest_levels = unique(train_reference_df$dest))
    
    missing_cols <- setdiff(names(train_reference_df), c(names(prep_ml), "is_delayed"))
    for (m_col in missing_cols) {
      if (is.numeric(train_reference_df[[m_col]])) {
        prep_ml[[m_col]] <- 0
      } else {
        prep_ml[[m_col]] <- factor(levels(train_reference_df[[m_col]])[1], levels = levels(train_reference_df[[m_col]]))
      }
    }
    
    
    for(col in names(train_reference_df)) {
      if(col != "is_delayed" && col %in% names(prep_ml)) {
        if(is.factor(train_reference_df[[col]])) {
          prep_ml[[col]] <- factor(prep_ml[[col]], levels = levels(train_reference_df[[col]]))
        } else {
          
          class(prep_ml[[col]]) <- class(train_reference_df[[col]])
        }
      }
    }
    

    expected_order <- setdiff(names(train_reference_df), "is_delayed")
    prep_ml <- prep_ml[, expected_order, drop = FALSE]
    
    if (model_type == "rf") {
      prob <- predict(bin_model, newdata = prep_ml, type = "prob")[, "1"]
    } else {
      prob <- predict(bin_model, newdata = prep_ml, type = "prob")[, 2]
    }
  }
  

  prep_reg <- prepare_xgb_data(new_data, exclude_post_dep = !is_after)
  minutes  <- predict(reg_model, xgb.DMatrix(prep_reg$X))
  
  cat(sprintf("Model Architecture:  %s\n", toupper(model_type)))
  cat(sprintf("Delay Probability:   %.1f%%\n", prob * 100))
  cat(sprintf("Predicted Magnitude: %.1f minutes\n", minutes))
  cat(sprintf("Final Verdict:       %s\n\n", ifelse(prob > 0.35, "LIKELY DELAYED", "LIKELY ON TIME")))
}


# ==================== TEST PREDICTION PIPELINE ====================
new_flight <- data.frame(
  month = 7, day_of_month = 15, day_of_week = 3,
  op_unique_carrier = "WN", origin = "BWI", dest = "MCO",
  crs_dep_time = 1400, crs_arr_time = 1630, distance = 850,
  dep_hour = 14, time_of_day = "afternoon", route = "BWI_MCO",
  season = "summer", distance_group = "medium", origin_flights = 3200
)

# Append mock attributes for post-departure features if testing the 'after departure' framework
# (These can be set to arbitrary baseline metrics or typical estimates for a testing sample)
new_flight$dep_delay <- 10
new_flight$dep_time <- 1410
new_flight$taxi_out <- 15
new_flight$wheels_off <- 1425
new_flight$air_time <- 110
new_flight$actual_elapsed_time <- 130
new_flight$wheels_on <- 1615
new_flight$taxi_in <- 5
new_flight$arr_time <- 1620




cat("          BEFORE DEPARTURE EVALUATIONS            \n")


# XGBoost Before Departure
predict_flight_delay(new_flight, bin_model = model_xgb_before, reg_model = model_reg_before, 
                     model_type = "xgb", is_after = FALSE)

# Random Forest Before Departure (Pass train_ml_before to lock factors)
predict_flight_delay(new_flight, bin_model = model_rf_before, reg_model = model_reg_before, 
                     model_type = "rf", is_after = FALSE, train_reference_df = train_ml_before)

# AdaBoost Before Departure
predict_flight_delay(new_flight, bin_model = model_ada_before, reg_model = model_reg_before, 
                     model_type = "ada", is_after = FALSE, train_reference_df = train_ml_before)



cat("           AFTER DEPARTURE EVALUATIONS            \n")


# XGBoost After Departure
predict_flight_delay(new_flight, bin_model = model_xgb_after, reg_model = model_reg_after, 
                     model_type = "xgb", is_after = TRUE)

# Random Forest After Departure
predict_flight_delay(new_flight, bin_model = model_rf_after, reg_model = model_reg_after, 
                     model_type = "rf", is_after = TRUE, train_reference_df = train_ml_after)

# AdaBoost After Departure
predict_flight_delay(new_flight, bin_model = model_ada_after, reg_model = model_reg_after, 
                     model_type = "ada", is_after = TRUE, train_reference_df = train_ml_after)

