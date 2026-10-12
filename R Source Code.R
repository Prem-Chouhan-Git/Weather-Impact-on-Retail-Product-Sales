library(DBI)
library(RSQLite)
library(lubridate)
library(dplyr)
library(rsample)
library(mice)
library(Metrics)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(yardstick)

#Relative path, make sure the DB is in working directory
db_path <- "./project1_raw_data.db"
con <- dbConnect(RSQLite::SQLite(), db_path)

#SQL ########################################################################### 
top3 <- 
"SELECT item_nbr, sum(units) AS total_units_sold
FROM train
GROUP BY item_nbr
ORDER BY total_units_sold DESC
LIMIT 3;"
top3_products <- dbGetQuery(con, top3)
print(top3_products)

join <- 
"WITH store_station AS (SELECT DISTINCT store_nbr, station_nbr FROM key)
SELECT t.date, t.store_nbr,t.item_nbr,t.units, ss.station_nbr
FROM train AS t
JOIN store_station AS ss
ON t.store_nbr = ss.store_nbr;"
joined_sales_station <- dbGetQuery(con,join)
print(head(joined_sales_station))

daily_tavg_top <-
"WITH top3_products AS (SELECT item_nbr FROM train GROUP BY item_nbr
ORDER BY SUM(units) DESC LIMIT 3),
one_top_product AS (
SELECT item_nbr FROM top3_products LIMIT 1)
SELECT t.date, t.item_nbr, SUM(t.units) AS daily_units_sold, 
AVG(CAST(w.tavg AS FLOAT))AS avg_temp
FROM train AS t
JOIN key AS k
ON k.store_nbr = t.store_nbr
LEFT JOIN weather AS w
ON w.station_nbr = k.station_nbr
AND w.date = t.date
WHERE t.item_nbr = (SELECT item_nbr FROM one_top_product)
GROUP BY t.date, t.item_nbr
ORDER BY t.date;"
daily_sales_tavg_top <- dbGetQuery(con, daily_tavg_top)
print(head(daily_sales_tavg_top))

#Designing a table to be loaded into R
#This query will return every row for the top 3 products,
#with the weather for each sale date. 
#Used left join so that no sales rows are missing if weather is missing.
qry <- 
"WITH top3_products AS (SELECT item_nbr FROM train GROUP BY item_nbr
ORDER BY SUM(units) DESC LIMIT 3),
sales AS (
SELECT date, store_nbr, item_nbr,SUM(units) AS units
FROM train
WHERE item_nbr IN (SELECT item_nbr FROM top3_products)
GROUP BY date, store_nbr, item_nbr),
store_station AS (SELECT DISTINCT store_nbr, station_nbr FROM key)
SELECT s.date, s.store_nbr, ss.station_nbr, s.item_nbr, s.units, w.tmax, w.tmin, 
w.tavg, w.depart, w.dewpoint, w.wetbulb, w.heat, w.cool, w.sunrise, w.sunset, 
w.codesum, w.snowfall, w.preciptotal, w.stnpressure, w.sealevel, w.resultspeed, 
w.resultdir, w.avgspeed
FROM sales AS s
JOIN store_station AS ss
ON ss.store_nbr = s.store_nbr
LEFT JOIN weather AS w
ON w.station_nbr = ss.station_nbr
AND w.date = s.date;"
df <- dbGetQuery(con, qry)
dbDisconnect(con)

#R #############################################################################
#Cleaning data
df$date <- ymd(df$date)

sum(trimws(df$codesum) == "", na.rm = TRUE)
df$codesum[trimws(df$codesum) == ""] <- NA
sum(trimws(df$codesum) == "", na.rm = TRUE)
sum(trimws(df$codesum) == " ", na.rm = TRUE)

sum(trimws(df$sunrise) == "", na.rm = TRUE)
sum(trimws(df$sunrise) == " ", na.rm = TRUE)

sum(trimws(df$sunset) == "", na.rm = TRUE)
sum(trimws(df$sunset) == " ", na.rm = TRUE)

sum(trimws(df$preciptotal) == "T", na.rm = TRUE)
sum(trimws(df$snowfall) == "T", na.rm = TRUE)
sum(trimws(df$preciptotal) == "t", na.rm = TRUE)
sum(trimws(df$snowfall) == "t", na.rm = TRUE)

str(df)

colSums(is.na(df))

df %>%
summarise(avgspeed = mean(is.na(avgspeed)),
tmax = mean(is.na(tmax)),
tmin = mean(is.na(tmin)),
tavg = mean(is.na(tavg)),
depart = mean(is.na(depart)),
dewpoint = mean(is.na(dewpoint)),
wetbulb = mean(is.na(wetbulb)),
heat = mean(is.na(heat)),
cool = mean(is.na(cool)),
sunrise = mean(is.na(sunrise)),
sunset = mean(is.na(sunset)),
codesum = mean(is.na(codesum)),
snowfall = mean(is.na(snowfall)),
preciptotal = mean(is.na(preciptotal)),
stnpressure = mean(is.na(stnpressure)),
sealevel = mean(is.na(sealevel)),
resultspeed = mean(is.na(resultspeed)),
resultdir = mean(is.na(resultdir)))

dropped_df <- subset(df, select = -c(depart, sunrise, sunset))

dropped_df %>% count(units, sort = TRUE)
plot(dropped_df$units)

removed_outliers <- dropped_df[dropped_df$units < 1000,]

#Creating features 
added_features <- removed_outliers %>%
arrange(date) %>%
mutate(
dow = factor(weekdays(date),
levels = c("Monday","Tuesday","Wednesday","Thursday","Friday",
"Saturday","Sunday")),
month = factor(format(date, "%m")),
store_number = factor(store_nbr), 
is_weekend = if_else(weekdays(date) %in% c("Saturday", "Sunday"), 1,0))

#Splitting data frame by date
split <- rsample::initial_validation_time_split(added_features,
prop = c(0.70, 0.10))

train <- training(split)
valid <- validation(split)
test  <- testing(split)

#Overwriting previous split so that no dates are overlapping in splits
valid_first_date <- min(valid$date)
test_first_date  <- min(test$date)

train <- added_features %>% filter(date < valid_first_date)
valid <- added_features %>% filter(date >= valid_first_date,
date < test_first_date)
test  <- added_features %>% filter(date >= test_first_date)

#Variables that I want to impute
variables_for_imputation <- c("tmax", "tmin", "tavg", "dewpoint", "wetbulb",
"heat", "cool", "preciptotal", "stnpressure","sealevel", "resultspeed",
"resultdir", "avgspeed")

#Variables that I don't want to impute
no_imputation <- c("date", "store_nbr", "station_nbr", "item_nbr","units", 
"snowfall", "codesum", "dow", "month", "store_number", "is_weekend")

#Imputing train only
train_to_impute <- train[,variables_for_imputation]

set.seed(123)
train_imp <- mice(train_to_impute, m = 1, method = "pmm")

train_imp_complete <- complete(train_imp, 1)

train_imputed <- cbind(train[, no_imputation],train_imp_complete)

#Applying train imputation model to valid
valid_to_impute <- valid[, variables_for_imputation]

valid_imp <- mice.mids(train_imp, newdata = valid_to_impute)

valid_imp_complete <- complete(valid_imp, 1)

valid_imputed <- cbind(valid[, no_imputation], valid_imp_complete)

#Checking for missing values in the imputed train and valid
sapply(train_imputed, function(x) sum(is.na(x)))
sapply(valid_imputed, function(x) sum(is.na(x)))

#Creating train and valid splits for each item
train_45 <- train_imputed %>% filter(item_nbr == 45)
valid_45 <- valid_imputed %>% filter(item_nbr == 45)

train_9 <- train_imputed %>% filter(item_nbr == 9)
valid_9 <- valid_imputed %>% filter(item_nbr == 9)

train_5 <- train_imputed %>% filter(item_nbr == 5)
valid_5 <- valid_imputed %>% filter(item_nbr == 5)

#Linear model training for tavg
lm_45_1_tavg <- lm(units ~ tavg, data = train_45)
lm_45_2_tavg <- lm(units ~ tavg + dow + month, data = train_45)
lm_45_3_tavg <- lm(units ~ tavg + dow + month + store_number, 
data = train_45)
lm_45_4_tavg <- lm(units ~ tavg + is_weekend + month + store_number, 
data = train_45)

lm_9_1_tavg <- lm(units ~ tavg, data = train_9)
lm_9_2_tavg <- lm(units ~ tavg + dow + month, data = train_9)
lm_9_3_tavg <- lm(units ~ tavg + dow + month + store_number, 
data = train_9)
lm_9_4_tavg <- lm(units ~ tavg + is_weekend + month + store_number, 
data = train_9)

lm_5_1_tavg <- lm(units ~ tavg, data = train_5)
lm_5_2_tavg <- lm(units ~ tavg + dow + month, data = train_5)
lm_5_3_tavg <- lm(units ~ tavg + dow + month + store_number, 
data = train_5)
lm_5_4_tavg <- lm(units ~ tavg + is_weekend + month + store_number, 
data = train_5)

#Lm Validating item 45 tavg models
p45_1_tavg <- predict(lm_45_1_tavg, newdata = valid_45)
p45_2_tavg <- predict(lm_45_2_tavg, newdata = valid_45)
p45_3_tavg <- predict(lm_45_3_tavg, newdata = valid_45)
p45_4_tavg <- predict(lm_45_4_tavg, newdata = valid_45)

p45_1_tavg <- pmax(0, p45_1_tavg)
p45_2_tavg <- pmax(0, p45_2_tavg)
p45_3_tavg <- pmax(0, p45_3_tavg)
p45_4_tavg <- pmax(0, p45_4_tavg)

validation_45_tavg <- data.frame(
Model = c("lm_45_1_tavg", "lm_45_2_tavg", "lm_45_3_tavg", "lm_45_4_tavg"),
MAE   = c(Metrics::mae(valid_45$units, p45_1_tavg),
          Metrics::mae(valid_45$units, p45_2_tavg),
          Metrics::mae(valid_45$units, p45_3_tavg),
          Metrics::mae(valid_45$units, p45_4_tavg)),
RMSE  = c(Metrics::rmse(valid_45$units, p45_1_tavg),
          Metrics::rmse(valid_45$units, p45_2_tavg),
          Metrics::rmse(valid_45$units, p45_3_tavg),
          Metrics::rmse(valid_45$units, p45_4_tavg)),
R2    = c(yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_1_tavg),
          yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_2_tavg),
          yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_3_tavg),
          yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_4_tavg)))

print(validation_45_tavg)

#Validating item 9 tavg models
p9_1_tavg <- predict(lm_9_1_tavg, newdata = valid_9)
p9_2_tavg <- predict(lm_9_2_tavg, newdata = valid_9)
p9_3_tavg <- predict(lm_9_3_tavg, newdata = valid_9)
p9_4_tavg <- predict(lm_9_4_tavg, newdata = valid_9)

p9_1_tavg <- pmax(0, p9_1_tavg)
p9_2_tavg <- pmax(0, p9_2_tavg)
p9_3_tavg <- pmax(0, p9_3_tavg)
p9_4_tavg <- pmax(0, p9_4_tavg)

validation_9_tavg <- data.frame(
Model = c("lm_9_1_tavg", "lm_9_2_tavg", "lm_9_3_tavg", "lm_9_4_tavg"),
MAE   = c(Metrics::mae(valid_9$units, p9_1_tavg),
          Metrics::mae(valid_9$units, p9_2_tavg),
          Metrics::mae(valid_9$units, p9_3_tavg),
          Metrics::mae(valid_9$units, p9_4_tavg)),
RMSE  = c(Metrics::rmse(valid_9$units, p9_1_tavg),
          Metrics::rmse(valid_9$units, p9_2_tavg),
          Metrics::rmse(valid_9$units, p9_3_tavg),
          Metrics::rmse(valid_9$units, p9_4_tavg)),
R2    = c(yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_1_tavg),
          yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_2_tavg),
          yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_3_tavg),
          yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_4_tavg)))

print(validation_9_tavg)

#Validating item 5 tavg models
p5_1_tavg <- predict(lm_5_1_tavg, newdata = valid_5)
p5_2_tavg <- predict(lm_5_2_tavg, newdata = valid_5)
p5_3_tavg <- predict(lm_5_3_tavg, newdata = valid_5)
p5_4_tavg <- predict(lm_5_4_tavg, newdata = valid_5)

p5_1_tavg <- pmax(0, p5_1_tavg)
p5_2_tavg <- pmax(0, p5_2_tavg)
p5_3_tavg <- pmax(0, p5_3_tavg)
p5_4_tavg <- pmax(0, p5_4_tavg)

validation_5_tavg <- data.frame(
Model = c("lm_5_1_tavg", "lm_5_2_tavg", "lm_5_3_tavg", "lm_5_4_tavg"),
MAE   = c(Metrics::mae(valid_5$units, p5_1_tavg),
          Metrics::mae(valid_5$units, p5_2_tavg),
          Metrics::mae(valid_5$units, p5_3_tavg),
          Metrics::mae(valid_5$units, p5_4_tavg)),
RMSE  = c(Metrics::rmse(valid_5$units, p5_1_tavg),
          Metrics::rmse(valid_5$units, p5_2_tavg),
          Metrics::rmse(valid_5$units, p5_3_tavg),
          Metrics::rmse(valid_5$units, p5_4_tavg)),
R2    = c(yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_1_tavg),
          yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_2_tavg),
          yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_3_tavg),
          yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_4_tavg)))

print(validation_5_tavg)

#Linear model training for cool
lm_45_1_cool <- lm(units ~ cool, data = train_45)
lm_45_2_cool <- lm(units ~ cool + dow + month, data = train_45)
lm_45_3_cool <- lm(units ~ cool + dow + month + store_number, 
data = train_45)
lm_45_4_cool <- lm(units ~ cool + is_weekend + month + store_number, 
data = train_45)

lm_9_1_cool <- lm(units ~ cool, data = train_9)
lm_9_2_cool <- lm(units ~ cool + dow + month, data = train_9)
lm_9_3_cool <- lm(units ~ cool + dow + month + store_number, 
data = train_9)
lm_9_4_cool <- lm(units ~ cool + is_weekend + month + store_number, 
data = train_9)

lm_5_1_cool <- lm(units ~ cool, data = train_5)
lm_5_2_cool <- lm(units ~ cool + dow + month, data = train_5)
lm_5_3_cool <- lm(units ~ cool + dow + month + store_number, 
data = train_5)
lm_5_4_cool <- lm(units ~ cool + is_weekend + month + store_number, 
data = train_5)

#Validating item 45 cool models
p45_1_cool <- predict(lm_45_1_cool, newdata = valid_45)
p45_2_cool <- predict(lm_45_2_cool, newdata = valid_45)
p45_3_cool <- predict(lm_45_3_cool, newdata = valid_45)
p45_4_cool <- predict(lm_45_4_cool, newdata = valid_45)

p45_1_cool <- pmax(0, p45_1_cool)
p45_2_cool <- pmax(0, p45_2_cool)
p45_3_cool <- pmax(0, p45_3_cool)
p45_4_cool <- pmax(0, p45_4_cool)

validation_45_cool <- data.frame(
Model = c("lm_45_1_cool", "lm_45_2_cool", "lm_45_3_cool", "lm_45_4_cool"),
MAE   = c(Metrics::mae(valid_45$units, p45_1_cool),
          Metrics::mae(valid_45$units, p45_2_cool),
          Metrics::mae(valid_45$units, p45_3_cool),
          Metrics::mae(valid_45$units, p45_4_cool)),
RMSE  = c(Metrics::rmse(valid_45$units, p45_1_cool),
          Metrics::rmse(valid_45$units, p45_2_cool),
          Metrics::rmse(valid_45$units, p45_3_cool),
          Metrics::rmse(valid_45$units, p45_4_cool)),
R2    = c(yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_1_cool),
          yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_2_cool),
          yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_3_cool),
          yardstick::rsq_trad_vec(truth = valid_45$units, estimate = p45_4_cool)))

print(validation_45_cool)

#Validating item 9 cool models
p9_1_cool <- predict(lm_9_1_cool, newdata = valid_9)
p9_2_cool <- predict(lm_9_2_cool, newdata = valid_9)
p9_3_cool <- predict(lm_9_3_cool, newdata = valid_9)
p9_4_cool <- predict(lm_9_4_cool, newdata = valid_9)

p9_1_cool <- pmax(0, p9_1_cool)
p9_2_cool <- pmax(0, p9_2_cool)
p9_3_cool <- pmax(0, p9_3_cool)
p9_4_cool <- pmax(0, p9_4_cool)

validation_9_cool <- data.frame(
Model = c("lm_9_1_cool", "lm_9_2_cool", "lm_9_3_cool", "lm_9_4_cool"),
MAE   = c(Metrics::mae(valid_9$units, p9_1_cool),
          Metrics::mae(valid_9$units, p9_2_cool),
          Metrics::mae(valid_9$units, p9_3_cool),
          Metrics::mae(valid_9$units, p9_4_cool)),
RMSE  = c(Metrics::rmse(valid_9$units, p9_1_cool),
          Metrics::rmse(valid_9$units, p9_2_cool),
          Metrics::rmse(valid_9$units, p9_3_cool),
          Metrics::rmse(valid_9$units, p9_4_cool)),
R2    = c(yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_1_cool),
          yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_2_cool),
          yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_3_cool),
          yardstick::rsq_trad_vec(truth = valid_9$units, estimate = p9_4_cool)))

print(validation_9_cool)

#Validating item 5 cool models
p5_1_cool <- predict(lm_5_1_cool, newdata = valid_5)
p5_2_cool <- predict(lm_5_2_cool, newdata = valid_5)
p5_3_cool <- predict(lm_5_3_cool, newdata = valid_5)
p5_4_cool <- predict(lm_5_4_cool, newdata = valid_5)

p5_1_cool <- pmax(0, p5_1_cool)
p5_2_cool <- pmax(0, p5_2_cool)
p5_3_cool <- pmax(0, p5_3_cool)
p5_4_cool <- pmax(0, p5_4_cool)

validation_5_cool <- data.frame(
Model = c("lm_5_1_cool", "lm_5_2_cool", "lm_5_3_cool", "lm_5_4_cool"),
MAE   = c(Metrics::mae(valid_5$units, p5_1_cool),
          Metrics::mae(valid_5$units, p5_2_cool),
          Metrics::mae(valid_5$units, p5_3_cool),
          Metrics::mae(valid_5$units, p5_4_cool)),
RMSE  = c(Metrics::rmse(valid_5$units, p5_1_cool),
          Metrics::rmse(valid_5$units, p5_2_cool),
          Metrics::rmse(valid_5$units, p5_3_cool),
          Metrics::rmse(valid_5$units, p5_4_cool)),
R2    = c(yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_1_cool),
          yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_2_cool),
          yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_3_cool),
          yardstick::rsq_trad_vec(truth = valid_5$units, estimate = p5_4_cool)))

print(validation_5_cool)

#Tree model training for tavg
tree_45_3_tavg <- rpart(units ~ tavg + dow + month + store_number,
method = "anova", data = train_45, control=rpart.control(cp=0.001, minsplit=15, 
maxdepth=20))

tree_9_3_tavg <- rpart(units ~ tavg + dow + month + store_number,
method = "anova",data = train_9, control=rpart.control(cp=0.001, minsplit=15, 
maxdepth=20))

tree_5_3_tavg <- rpart(units ~ tavg + dow + month + store_number,
method = "anova", data = train_5, control=rpart.control(cp=0.001, minsplit=15, 
maxdepth=20))

#Tree validation for tavg for item 45
p45_3_tavg_tree <- predict(tree_45_3_tavg, newdata = valid_45)
p45_3_tavg_tree <- pmax(0, p45_3_tavg_tree)

validation_45_3_tavg_tree <- data.frame(
Model = "tree_45_3_tavg",
MAE = Metrics::mae(valid_45$units, p45_3_tavg_tree),
RMSE = Metrics::rmse(valid_45$units, p45_3_tavg_tree),
R2 = yardstick::rsq_trad_vec(
truth = valid_45$units,
estimate = p45_3_tavg_tree))

print(validation_45_3_tavg_tree)
print(sort(tree_45_3_tavg$variable.importance, decreasing = TRUE))

#Tree validation for tavg for item 9
p9_3_tavg_tree <- predict(tree_9_3_tavg, newdata = valid_9)
p9_3_tavg_tree <- pmax(0, p9_3_tavg_tree)

validation_9_3_tavg_tree <- data.frame(
Model = "tree_9_3_tavg",
MAE = Metrics::mae(valid_9$units, p9_3_tavg_tree),
RMSE = Metrics::rmse(valid_9$units, p9_3_tavg_tree),
R2 = yardstick::rsq_trad_vec(
truth = valid_9$units,
estimate = p9_3_tavg_tree))

print(validation_9_3_tavg_tree)
print(sort(tree_9_3_tavg$variable.importance, decreasing = TRUE))

#Tree validation for tavg for item 5
p5_3_tavg_tree <- predict(tree_5_3_tavg, newdata = valid_5)
p5_3_tavg_tree <- pmax(0, p5_3_tavg_tree)

validation_5_3_tavg_tree <- data.frame(
Model = "tree_5_3_tavg",
MAE = Metrics::mae(valid_5$units, p5_3_tavg_tree),
RMSE = Metrics::rmse(valid_5$units, p5_3_tavg_tree),
R2 = yardstick::rsq_trad_vec(
truth = valid_5$units,
estimate = p5_3_tavg_tree))

print(validation_5_3_tavg_tree)
print(sort(tree_5_3_tavg$variable.importance, decreasing = TRUE))

#Tree model training for cool
tree_45_3_cool <- rpart(units ~ cool + dow + month + store_number,
method = "anova",data = train_45, control=rpart.control(cp=0.001, minsplit=15, 
maxdepth=20))

tree_9_3_cool <- rpart(units ~ cool + dow + month + store_number,
method = "anova",data = train_9, control=rpart.control(cp=0.001, minsplit=15, 
maxdepth=20))

tree_5_3_cool <- rpart(units ~ cool + dow + month + store_number,
method = "anova",data = train_5, control=rpart.control(cp=0.001, minsplit=15, 
maxdepth=20))

#Tree validation for cool for item 45
p45_3_cool_tree <- predict(tree_45_3_cool, newdata = valid_45)
p45_3_cool_tree <- pmax(0, p45_3_cool_tree)

validation_45_3_cool_tree <- data.frame(
Model = "tree_45_3_cool",
MAE = Metrics::mae(valid_45$units, p45_3_cool_tree),
RMSE = Metrics::rmse(valid_45$units, p45_3_cool_tree),
R2 = yardstick::rsq_trad_vec(
truth = valid_45$units,
estimate = p45_3_cool_tree))

print(validation_45_3_cool_tree)
print(sort(tree_45_3_cool$variable.importance, decreasing = TRUE))

#Tree validation for cool for item 9
p9_3_cool_tree <- predict(tree_9_3_cool, newdata = valid_9)
p9_3_cool_tree <- pmax(0, p9_3_cool_tree)

validation_9_3_cool_tree <- data.frame(
Model = "tree_9_3_cool",
MAE = Metrics::mae(valid_9$units, p9_3_cool_tree),
RMSE = Metrics::rmse(valid_9$units, p9_3_cool_tree),
R2 = yardstick::rsq_trad_vec(
truth = valid_9$units,
estimate = p9_3_cool_tree))

print(validation_9_3_cool_tree)
print(sort(tree_9_3_cool$variable.importance, decreasing = TRUE))

#Tree validation for cool for item 5
p5_3_cool_tree <- predict(tree_5_3_cool, newdata = valid_5)
p5_3_cool_tree <- pmax(0, p5_3_cool_tree)

validation_5_3_cool_tree <- data.frame(
Model = "tree_5_3_cool",
MAE = Metrics::mae(valid_5$units, p5_3_cool_tree),
RMSE = Metrics::rmse(valid_5$units, p5_3_cool_tree),
R2 = yardstick::rsq_trad_vec(
truth = valid_5$units,
estimate = p5_3_cool_tree))

print(validation_5_3_cool_tree)
print(sort(tree_5_3_cool$variable.importance, decreasing = TRUE))

#Overall validation comparisons ################################################
overall_validation_45 <- bind_rows(
validation_45_tavg %>% filter(Model == "lm_45_3_tavg"),
validation_45_cool %>% filter(Model == "lm_45_3_cool"),
validation_45_3_tavg_tree,
validation_45_3_cool_tree) %>% arrange(RMSE, MAE, desc(R2))

overall_validation_9 <- bind_rows(
validation_9_tavg %>% filter(Model == "lm_9_3_tavg"),
validation_9_cool %>% filter(Model == "lm_9_3_cool"),
validation_9_3_tavg_tree,
validation_9_3_cool_tree) %>% arrange(RMSE, MAE, desc(R2))

overall_validation_5 <- bind_rows(
validation_5_tavg %>% filter(Model == "lm_5_3_tavg"),
validation_5_cool %>% filter(Model == "lm_5_3_cool"),
validation_5_3_tavg_tree,
validation_5_3_cool_tree) %>% arrange(RMSE, MAE, desc(R2))

print(overall_validation_45)
print(overall_validation_9)
print(overall_validation_5)

#Combing and valid and train, then refitting imputation
train_valid <- bind_rows(train, valid)

train_valid_to_impute <- train_valid[, variables_for_imputation]

set.seed(123)
train_valid_imp <- mice(train_valid_to_impute, m = 1, method = "pmm", printFlag = FALSE)

train_valid_imp_complete <- complete(train_valid_imp, 1)

train_valid_imputed <- cbind(train_valid[, no_imputation], train_valid_imp_complete)

#Imputing test from the refitted train and valid imputation
test_to_impute <- test[, variables_for_imputation]

test_imp <- mice.mids(train_valid_imp, newdata = test_to_impute)

test_imp_complete <- complete(test_imp, 1)

test_imputed <- cbind(test[, no_imputation], test_imp_complete)

#Checking for missing values
sapply(train_valid_imputed, function(x) sum(is.na(x)))
sapply(test_imputed, function(x) sum(is.na(x)))

#Creating final train+valid and test splits for each item
train_45_valid <- train_valid_imputed %>% filter(item_nbr == 45)
test_45_final  <- test_imputed %>% filter(item_nbr == 45)

train_9_valid <- train_valid_imputed %>% filter(item_nbr == 9)
test_9_final  <- test_imputed %>% filter(item_nbr == 9)

train_5_valid <- train_valid_imputed %>% filter(item_nbr == 5)
test_5_final  <- test_imputed %>% filter(item_nbr == 5)

#Final tavg models for validation
#Item 45
lm_45_3_tavg_final <- lm(units ~ tavg + dow + month + store_number,
data = train_45_valid)

p45_test_tavg_final <- predict(lm_45_3_tavg_final, newdata = test_45_final)
p45_test_tavg_final <- pmax(0, p45_test_tavg_final)

summary(lm_45_3_tavg_final)

final_metrics_45_tavg <- data.frame(
Model = "lm_45_3_tavg_final",
MAE = Metrics::mae(test_45_final$units, p45_test_tavg_final),
RMSE = Metrics::rmse(test_45_final$units, p45_test_tavg_final),
R2 = yardstick::rsq_trad_vec(
truth = test_45_final$units,
estimate = p45_test_tavg_final))

print(final_metrics_45_tavg)

# Item 9
lm_9_3_tavg_final <- lm(units ~ tavg + dow + month + store_number,
data = train_9_valid)

p9_test_tavg_final <- predict(lm_9_3_tavg_final, newdata = test_9_final)
p9_test_tavg_final <- pmax(0, p9_test_tavg_final)

summary(lm_9_3_tavg_final)

final_metrics_9_tavg <- data.frame(
Model = "lm_9_3_tavg_final",
MAE   = Metrics::mae(test_9_final$units, p9_test_tavg_final),
RMSE  = Metrics::rmse(test_9_final$units, p9_test_tavg_final),
R2    = yardstick::rsq_trad_vec(
truth = test_9_final$units,
estimate = p9_test_tavg_final))

print(final_metrics_9_tavg)

# Item 5
lm_5_3_tavg_final <- lm(units ~ tavg + dow + month + store_number,
data = train_5_valid)

p5_test_tavg_final <- predict(lm_5_3_tavg_final, newdata = test_5_final)
p5_test_tavg_final <- pmax(0, p5_test_tavg_final)

summary(lm_5_3_tavg_final)

final_metrics_5_tavg <- data.frame(
Model = "lm_5_3_tavg_final",
MAE = Metrics::mae(test_5_final$units, p5_test_tavg_final),
RMSE = Metrics::rmse(test_5_final$units, p5_test_tavg_final),
R2 = yardstick::rsq_trad_vec(
truth = test_5_final$units,
estimate = p5_test_tavg_final))

print(final_metrics_5_tavg)

#Final cool models for validation
# Item 45
lm_45_3_cool_final <- lm(units ~ cool + dow + month + store_number,
data = train_45_valid)

p45_test_cool_final <- predict(lm_45_3_cool_final, newdata = test_45_final)
p45_test_cool_final <- pmax(0, p45_test_cool_final)

summary(lm_45_3_cool_final)

final_metrics_45_cool <- data.frame(
Model = "lm_45_3_cool_final",
MAE = Metrics::mae(test_45_final$units, p45_test_cool_final),
RMSE = Metrics::rmse(test_45_final$units, p45_test_cool_final),
R2 = yardstick::rsq_trad_vec(
truth = test_45_final$units,
estimate = p45_test_cool_final))

print(final_metrics_45_cool)

# Item 9
lm_9_3_cool_final <- lm(units ~ cool + dow + month + store_number,
data = train_9_valid)

p9_test_cool_final <- predict(lm_9_3_cool_final, newdata = test_9_final)
p9_test_cool_final <- pmax(0, p9_test_cool_final)

summary(lm_9_3_cool_final)

final_metrics_9_cool <- data.frame(
Model = "lm_9_3_cool_final",
MAE = Metrics::mae(test_9_final$units, p9_test_cool_final),
RMSE = Metrics::rmse(test_9_final$units, p9_test_cool_final),
R2 = yardstick::rsq_trad_vec(
truth = test_9_final$units,
estimate = p9_test_cool_final))

print(final_metrics_9_cool)

# Item 5
lm_5_3_cool_final <- lm(units ~ cool + dow + month + store_number,
data = train_5_valid)

p5_test_cool_final <- predict(lm_5_3_cool_final, newdata = test_5_final)
p5_test_cool_final <- pmax(0, p5_test_cool_final)

summary(lm_5_3_cool_final)

final_metrics_5_cool <- data.frame(
Model = "lm_5_3_cool_final",
MAE = Metrics::mae(test_5_final$units, p5_test_cool_final),
RMSE = Metrics::rmse(test_5_final$units, p5_test_cool_final),
R2 = yardstick::rsq_trad_vec(
truth = test_5_final$units,
estimate = p5_test_cool_final))

print(final_metrics_5_cool)

#Creating a typical store_number, dow and month

#Item 45
store_summary_45 <- train_45_valid %>%
group_by(store_number) %>%
summarise(avg_units = mean(units), .groups = "drop")

target_45 <- median(store_summary_45$avg_units)

typical_store_45 <- store_summary_45 %>%
mutate(diff = abs(avg_units - target_45)) %>%
arrange(diff) %>%
slice(1) %>%
pull(store_number)

typical_dow_45 <- names(sort(table(train_45_valid$dow), decreasing = TRUE))[1]
typical_month_45 <- names(sort(table(train_45_valid$month), decreasing = TRUE))[1]

#Item 9
store_summary_9 <- train_9_valid %>%
group_by(store_number) %>%
summarise(avg_units = mean(units), .groups = "drop")

target_9 <- median(store_summary_9$avg_units)

typical_store_9 <- store_summary_9 %>%
mutate(diff = abs(avg_units - target_9)) %>%
arrange(diff) %>%
slice(1) %>%
pull(store_number)

typical_dow_9 <- names(sort(table(train_9_valid$dow), decreasing = TRUE))[1]
typical_month_9 <- names(sort(table(train_9_valid$month), decreasing = TRUE))[1]

#Item 5
store_summary_5 <- train_5_valid %>%
group_by(store_number) %>%
summarise(avg_units = mean(units), .groups = "drop")

target_5 <- median(store_summary_5$avg_units)

typical_store_5 <- store_summary_5 %>%
mutate(diff = abs(avg_units - target_5)) %>%
arrange(diff) %>%
slice(1) %>%
pull(store_number)

typical_dow_5 <- names(sort(table(train_5_valid$dow), decreasing = TRUE))[1]
typical_month_5 <- names(sort(table(train_5_valid$month), decreasing = TRUE))[1]

print(typical_store_45)
print(typical_store_9)
print(typical_store_5)

#directory for plots to be saved into
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

#linear model plots ############################################################
#Plot for predicted units vs tavg for item 45 
plot_data_45_tavg <- data.frame(
tavg = seq(min(train_45_valid$tavg, na.rm = TRUE),
max(train_45_valid$tavg, na.rm = TRUE),
length.out = 200),
dow = factor(typical_dow_45, levels = levels(train_45_valid$dow)),
month = factor(typical_month_45, levels = levels(train_45_valid$month)),
store_number = factor(as.character(typical_store_45),
levels = levels(train_45_valid$store_number)))

pred_45_tavg <- predict(lm_45_3_tavg_final, newdata = plot_data_45_tavg,
interval = "confidence")

plot_data_45_tavg$predicted <- pmax(0, pred_45_tavg[, "fit"])
plot_data_45_tavg$conf.low <- pmax(0, pred_45_tavg[, "lwr"])
plot_data_45_tavg$conf.high <- pmax(0, pred_45_tavg[, "upr"])

p_45_tavg <- ggplot(plot_data_45_tavg, aes(tavg, predicted)) +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% CI"),
alpha = 0.2) + geom_line(aes(color = "Predicted"), linewidth = 1) +
labs(title = "Item 45: Predicted Units vs Average Temperature",
subtitle = "Predictions shown for a typical store, day of week, and month",
x = "Average Temperature (°F)", y = "Predicted Units Sold",
color = NULL, fill = NULL)

print(p_45_tavg)
ggsave("plots/item45_tavg.png", plot = p_45_tavg, width = 7, height = 4, dpi = 300)

#Plot for predicted units vs tavg for item 9 
plot_data_9_tavg <- data.frame(
tavg = seq(min(train_9_valid$tavg, na.rm = TRUE),
max(train_9_valid$tavg, na.rm = TRUE),
length.out = 200),
dow = factor(typical_dow_9, levels = levels(train_9_valid$dow)),
month = factor(typical_month_9, levels = levels(train_9_valid$month)),
store_number = factor(as.character(typical_store_9),
levels = levels(train_9_valid$store_number)))

pred_9_tavg <- predict(lm_9_3_tavg_final, newdata = plot_data_9_tavg,
interval = "confidence")

plot_data_9_tavg$predicted <- pmax(0, pred_9_tavg[, "fit"])
plot_data_9_tavg$conf.low <- pmax(0, pred_9_tavg[, "lwr"])
plot_data_9_tavg$conf.high <- pmax(0, pred_9_tavg[, "upr"])

p_9_tavg <- ggplot(plot_data_9_tavg, aes(tavg, predicted)) +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% CI"),
alpha = 0.2) + geom_line(aes(color = "Predicted"), linewidth = 1) +
labs(title = "Item 9: Predicted Units vs Average Temperature",
subtitle = "Predictions shown for a typical store, day of week, and month",
x = "Average Temperature (°F)", y = "Predicted Units Sold",
color = NULL, fill = NULL)

print(p_9_tavg)
ggsave("plots/item9_tavg.png", plot = p_9_tavg, width = 7, height = 4, dpi = 300)

#Plot for predicted units vs tavg for item 5 
plot_data_5_tavg <- data.frame(
tavg = seq(min(train_5_valid$tavg, na.rm = TRUE),
max(train_5_valid$tavg, na.rm = TRUE),
length.out = 200),
dow = factor(typical_dow_5, levels = levels(train_5_valid$dow)),
month = factor(typical_month_5, levels = levels(train_5_valid$month)),
store_number = factor(as.character(typical_store_5),
levels = levels(train_5_valid$store_number)))

pred_5_tavg <- predict(lm_5_3_tavg_final, newdata = plot_data_5_tavg,
interval = "confidence")

plot_data_5_tavg$predicted <- pmax(0, pred_5_tavg[, "fit"])
plot_data_5_tavg$conf.low <- pmax(0, pred_5_tavg[, "lwr"])
plot_data_5_tavg$conf.high <- pmax(0, pred_5_tavg[, "upr"])

p_5_tavg <- ggplot(plot_data_5_tavg, aes(tavg, predicted)) +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% CI"),
alpha = 0.2) + geom_line(aes(color = "Predicted"), linewidth = 1) +
labs(title = "Item 5: Predicted Units vs Average Temperature",
subtitle = "Predictions shown for a typical store, day of week, and month",
x = "Average Temperature (°F)", y = "Predicted Units Sold",
color = NULL, fill = NULL)

print(p_5_tavg)
ggsave("plots/item5_tavg.png", plot = p_5_tavg, width = 7, height = 4, dpi = 300)


#Plot for predicted units vs cool for item 45 
plot_data_45_cool <- data.frame(
cool = seq(min(train_45_valid$cool, na.rm = TRUE),
max(train_45_valid$cool, na.rm = TRUE),
length.out = 200),
dow = factor(typical_dow_45, levels = levels(train_45_valid$dow)),
month = factor(typical_month_45, levels = levels(train_45_valid$month)),
store_number = factor(as.character(typical_store_45),
levels = levels(train_45_valid$store_number)))

pred_45_cool <- predict(lm_45_3_cool_final, newdata = plot_data_45_cool,
interval = "confidence")

plot_data_45_cool$predicted <- pmax(0, pred_45_cool[, "fit"])
plot_data_45_cool$conf.low <- pmax(0, pred_45_cool[, "lwr"])
plot_data_45_cool$conf.high <- pmax(0, pred_45_cool[, "upr"])

p_45_cool <- ggplot(plot_data_45_cool, aes(cool, predicted)) +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% CI"),
alpha = 0.2) + geom_line(aes(color = "Predicted"), linewidth = 1) +
labs(title = "Item 45: Predicted Units vs Cooling Degree Days",
subtitle = "Predictions shown for a typical store, day of week, and month",
x = "Cooling Degree Days (Base 65°F)", y = "Predicted Units Sold",
color = NULL, fill = NULL)

print(p_45_cool)
ggsave("plots/item45_cool.png", plot = p_45_cool, width = 7, height = 4, dpi = 300)

#Plot for predicted units vs cool for item 9 
plot_data_9_cool <- data.frame(
cool = seq(min(train_9_valid$cool, na.rm = TRUE),
max(train_9_valid$cool, na.rm = TRUE),
length.out = 200),
dow = factor(typical_dow_9, levels = levels(train_9_valid$dow)),
month = factor(typical_month_9, levels = levels(train_9_valid$month)),
store_number = factor(as.character(typical_store_9),
levels = levels(train_9_valid$store_number)))

pred_9_cool <- predict(lm_9_3_cool_final, newdata = plot_data_9_cool,
interval = "confidence")

plot_data_9_cool$predicted <- pmax(0, pred_9_cool[, "fit"])
plot_data_9_cool$conf.low <- pmax(0, pred_9_cool[, "lwr"])
plot_data_9_cool$conf.high <- pmax(0, pred_9_cool[, "upr"])

p_9_cool <- ggplot(plot_data_9_cool, aes(cool, predicted)) +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% CI"),
alpha = 0.2) + geom_line(aes(color = "Predicted"), linewidth = 1) +
labs(title = "Item 9: Predicted Units vs Cooling Degree Days",
subtitle = "Predictions shown for a typical store, day of week, and month",
x = "Cooling Degree Days (Base 65°F)", y = "Predicted Units Sold",
color = NULL, fill = NULL)

print(p_9_cool)
ggsave("plots/item9_cool.png", plot = p_9_cool, width = 7, height = 4, dpi = 300)

#Plot for predicted units vs cool for item 5 
plot_data_5_cool <- data.frame(
cool = seq(min(train_5_valid$cool, na.rm = TRUE),
max(train_5_valid$cool, na.rm = TRUE),
length.out = 200),
dow = factor(typical_dow_5, levels = levels(train_5_valid$dow)),
month = factor(typical_month_5, levels = levels(train_5_valid$month)),
store_number = factor(as.character(typical_store_5),
levels = levels(train_5_valid$store_number)))

pred_5_cool <- predict(lm_5_3_cool_final, newdata = plot_data_5_cool,
interval = "confidence")

plot_data_5_cool$predicted <- pmax(0, pred_5_cool[, "fit"])
plot_data_5_cool$conf.low <- pmax(0, pred_5_cool[, "lwr"])
plot_data_5_cool$conf.high <- pmax(0, pred_5_cool[, "upr"])

p_5_cool <- ggplot(plot_data_5_cool, aes(cool, predicted)) +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "95% CI"),
alpha = 0.2) + geom_line(aes(color = "Predicted"), linewidth = 1) +
labs(title = "Item 5: Predicted Units vs Cooling Degree Days",
subtitle = "Predictions shown for a typical store, day of week, and month",
x = "Cooling Degree Days (Base 65°F)", y = "Predicted Units Sold",
color = NULL, fill = NULL)

print(p_5_cool)
ggsave("plots/item5_cool.png", plot = p_5_cool, width = 7, height = 4, dpi = 300)

#Tree model plots ##############################################################
png("plots/tree_item45_tavg.png", width = 1200, height = 700, res = 300)
rpart.plot(tree_45_3_tavg, main = "Tavg Decision Tree for Item 45")
dev.off()

png("plots/tree_item9_tavg.png", width = 1200, height = 700, res = 300)
rpart.plot(tree_9_3_tavg, main = "Tavg Decision Tree for Item 9")
dev.off()

png("plots/tree_item5_tavg.png", width = 1200, height = 700, res = 300)
rpart.plot(tree_5_3_tavg, main = "Tavg Decision Tree for Item 5")
dev.off()

png("plots/tree_item45_cool.png", width = 1200, height = 700, res = 300)
rpart.plot(tree_45_3_cool, main = "Cool Decision Tree for Item 45")
dev.off()

png("plots/tree_item9_cool.png", width = 1200, height = 700, res = 300)
rpart.plot(tree_9_3_cool, main = "Cool Decision Tree for Item 9")
dev.off()

png("plots/tree_item5_cool.png", width = 1200, height = 700, res = 300)
rpart.plot(tree_5_3_cool, main = "Cool Decision Tree for Item 5")
dev.off()

