---
title: "Demand forecasting"
author: "Bastien Rochowski"
date: "`r format(Sys.time(), '%d %B, %Y')`"
---
   
# libraries -------
library(kableExtra)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tsibble)
library(pwt9)
library(corrplot)
library(factoextra)
library(urca)
library(aTSA)
library(tseries)
library(forecast)
library(wesanderson)


# Load data
network_data <- read_csv(here::here("data/physical_network.csv"))

head(network_data)
dim(network_data)
summary(network_data)
glimpse(network_data)


# unique items by column
for (i in names(network_data)) {
        print(paste(i, length(table(network_data[i])), sep = ":"))
}

table(network_data$"Request Date")

# check NAs
count_missing <- function(df){
        sapply(df, FUN = function(col) sum(is.na(col)))
}

nacount <- count_missing(network_data)
hasNA <- which(nacount > 0)
nacount[hasNA]

# do the date make sense
print_date <- table(network_data$"Request Date") # yes
plot(print_date)


#### Exploratory order_data Analysis - Outliers
# # basic summary
# # summary(network_data$Quantity)
# # 
# upper_lim <- 0.99
# lower_lim <- 0.01
# 
# wh_counts <- network_data %>% group_by(destination_simple) %>%
#   summarise(total_orders = n(),
#             sum_orders = sum(Quantity, na.rm = TRUE))
# 
# # understand what and how much is being removed
# outs <- network_data %>% group_by(destination_simple, "2nd Item Number") %>%
#   mutate(upper = quantile(Quantity, upper_lim)) %>%
#   mutate(lower = quantile(Quantity, lower_lim)) %>%
#   mutate(take_out = ifelse(Quantity > upper | Quantity < lower, 1, 0)) %>%
#   filter(take_out == 1) %>%
#   group_by(destination_simple) %>%
#   summarise(count_removed = n(),
#             sum_removed = sum(Quantity, na.rm = TRUE),
#             avg_remove = mean(Quantity, na.rm = TRUE)) %>%
#   left_join(wh_counts, by="destination_simple") %>%
#   mutate(pct_count_removed = count_removed / total_orders,
#          pct_sum_removed = sum_removed / sum_orders)
# 
# # remove outliers - doing this by destination_simple and product ID since scales are different
# # removing top and bottom 2% - be weary of this, understand what is lost
# 
# network_data_clean <- network_data %>% group_by(destination_simple, "2nd Item Number") %>%
#   mutate(upper = quantile(Quantity, upper_lim)) %>%
#   mutate(lower = quantile(Quantity, lower_lim)) %>%
#   mutate(take_out = ifelse(Quantity > upper | Quantity < lower, 1, 0)) %>%
#   filter(take_out == 0)


# Feature selection ------------------------------------------

# select only the column with descriptions and filters NAs out
network_data_sel <- network_data %>% # network_data_clean if one wants outliers in
        select(.,"Request Date", "2nd Item Number", Brands, "Sold To", "Ship To",
               "Volume (MT)", destination_simple, "Price UOM", UOM, Quantity) %>%
        rename(., Request_Date = "Request Date", SKU = "2nd Item Number",
               Sold_To = "Sold To", Ship_To = "Ship To",
               #               Extended_Amount = "Extended Amount", Unit_Price = "Unit Price",
               Price_UOM ="Price UOM",
               Volume_MT = "Volume (MT)") %>%
        # create month, year, and day-of-week variables for order demand
        mutate(DOW = lubridate::wday(Request_Date),
               Month = lubridate::month(Request_Date),
               Year = lubridate::year(Request_Date)) %>%
        filter(., destination_simple == "Customers", # select only for retail customers
               Price_UOM == "CT", UOM == "CT",
               Request_Date > as.Date("2016-12-31") &
                       Request_Date < as.Date("2019-12-01")) %>% # filter out starting and ending months
        select(., -c(destination_simple, Price_UOM, UOM)) %>%
        #  drop_na() %>%
        glimpse()


# print the first rows as kablle table
# my_kable = function(x, max.rows=6, ...) {
#         kable(x[1:max.rows, ], ...)
# }

# my_kable(network_data_sel, 5, caption = "Physical Network Data selected") %>%
#         kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# check sum of quantities by month
summ_by_month <- network_data_sel %>%
        mutate(Year = as.factor(Year)) %>% 
        group_by(Year, Month) %>%
        summarise(Quantity = sum(Quantity)) 

ggplot(summ_by_month, aes(Month, Quantity, color = Year)) +
        geom_line()

head(network_data_sel)
nacount <- count_missing(network_data_sel)
hasNA <- which(nacount > 0)
nacount[hasNA]

# Transforming Variables - Class
# network_data_sel$Request_Date <- as_date(network_data$Request_Date)
network_data_sel$SKU <- as.factor(network_data_sel$SKU)
network_data_sel$Brands <- as.factor(network_data_sel$Brands)

# create a dataframe with the sum of quantity over a month by SKU (X2nd.Item.Number)
network_data_month <- network_data_sel %>% # without outliers, use network_data_sel_out
        group_by(., SKU, #Sold.To, Ship.To, # if you want total by customer by shipping address
                 Month_Year = floor_date(Request_Date, "month")) %>%
        mutate(., Quantity_Month = sum(Quantity),
               #               Extended_Amount = sum(Extended_Amount),
               Volume_MT = sum(Volume_MT)) %>%
        arrange(., Request_Date, SKU) %>% 
        ungroup(.,) %>%
        drop_na(.,) %>% 
        glimpse(.,) # All features in the good format


# test to check sum has worked
network_data_sel <- network_data_sel %>% 
        arrange(., Request_Date, SKU)
glimpse(network_data)

test1 <- network_data_sel %>% 
        filter(SKU == 110455, Request_Date > as.Date("2017-02-28") & 
                       Request_Date < as.Date("2017-04-01")) 

sum(test1$Quantity) # to verify by looking at network_data_sel$Quantity for given SKU


# filtering out identical rows
network_data_month_unique <- network_data_month %>% 
        select(Month_Year, everything(), -c(Sold_To, Quantity)) %>% 
        distinct(Month_Year, SKU, .keep_all = TRUE) %>% 
        mutate(SKU = paste("SKU", SKU, sep = "_"))
#        group_by(Month_Year, X2nd.Item.Number) %>% 
#        filter(n() == 1,) %>% 
#        ungroup()

# SKU as factor
network_data_month_unique$SKU <- as.factor(network_data_month_unique$SKU)
head(network_data_month_unique)        

# Check NAs
nacount <- count_missing(network_data_month_unique)
hasNA <- which(nacount > 0)
nacount[hasNA]



# EDA ---------------------------- 

# Summary
summary(network_data_month_unique)

#### Plot numeric features
network_data_month_unique %>%
        select_if(is.numeric) %>%
        gather() %>% 
        #pivot_longer(names_to = c(Extended_Amount, Volume_MT, Quantity_Month), values_to = value) %>%
        ggplot(aes(x = value)) +
        facet_wrap(~ key, scales = "free", ncol = 4) +
        geom_density()


# Plot categorical features
network_data_month_unique %>%
        select_if(is.factor) %>%
        gather() %>%
        ggplot(aes(x = value)) +
        facet_wrap(~ key, scales = "free", ncol = 2) +
        geom_bar() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "top") +
        scale_color_tableau() +
        scale_fill_tableau()


# Plotting Trends 
# arrange network_data_month_unique by date
#network_data_sel <- network_data_sel %>% 
#  arrange(SKU, Request_Date) 

# Year as factor for plotting
network_data_month$Year <- as.factor(network_data_month$Year)

# trend plots
overall_day <- network_data_sel %>% 
        group_by(Request_Date) %>% 
        summarise(orders = sum(Quantity)) # note, if I can keep it on one line I will
ggplot(overall_day, aes(x = Request_Date, y = orders)) + geom_line() + 
        labs(x="Date",y="Total Orders",title="Total Orders by Day") +
        theme_bw()

# plot time-series orders by month
by_month <- network_data_month %>%
        group_by(Month_Year) %>%
        summarise(orders = sum(Quantity, na.rm = TRUE))
ggplot(by_month, aes(x=Month_Year, y = orders)) +
        geom_line() +
        labs(x = "Month", y = "Total Orders", title = "Total Orders by Month") +
        theme_bw() +
        geom_smooth(size = 0.2, se = FALSE, color = 'gray')

# inspect the dow curves DOW
dow_chart <- network_data_month %>% 
        group_by(Month_Year, DOW) %>% 
        summarise(orders=sum(Quantity)) %>% 
        mutate(pct = orders/sum(orders))

# inspect the dow curves Month
month_chart <- network_data_month %>% 
        group_by(Year, Month) %>% 
        summarise(orders=sum(Quantity_Month)) %>% 
        mutate(pct = orders/sum(orders)) %>% 
        ungroup()

# day of the week curves
ggplot(dow_chart, aes(x = DOW, y = pct, group = Month_Year)) + geom_line() +
        labs(x = "Day of Week", y = "Percent of Total Orders", 
             title = "Disribution of Orders Across Days of Week") +
        theme_bw()
ggplot(dow_chart %>% filter(!DOW %in% c(1,7)), 
       aes(x = DOW, y = pct, 
           group = Month_Year)) + geom_line() +
        labs(x = "Day of Week",y = "Percent of Total Orders", 
             title= "Disribution of Orders Across Days of Week (no weekends)") +
        theme_bw()

# day of the week boxplots
ggplot(dow_chart, aes(x = DOW, y = pct, group = Month_Year)) + 
        geom_boxplot()  + 
        labs(x = "Day of Week", y = "Percent of Total Orders", 
             title = "Disribution of Orders Across Days of Week") +
        theme_bw()

# ggplot(month_chart %>% filter(!Month %in% c(1,12)), 
#        aes(x = Month, y = pct, group = Year)) + geom_line() +
#   labs(x="Month of the Year", y = "Percent of Total Orders",
#        title = "Disribution of Orders Across Month of the Year") +
#   theme_bw()


# month of the year curves
ggplot(month_chart, aes(x = Month, y = pct, group = Year, colour = Year)) + geom_line() +
        labs(x="Month of the Year",y="Percent of Total Orders",
             title="Disribution of Orders Across Month of the Year") +
        theme_bw()+
        scale_colour_manual(values = wes_palette(n = 3, name = "Darjeeling1"))


# Sale clustering 

# Cluster plots
# The k-means implementation in R expects a wide data frame (currently my 
# data frame is in the long format) and no missing values

# formate for clustering and forecasting
head(network_data_month_unique)

network_data_wide <- network_data_month_unique %>%
        select(Month_Year, SKU, Quantity_Month) %>%
        pivot_wider(names_from = Month_Year, 
                    values_from = Quantity_Month, 
                    values_fill = list(Quantity_Month = 0))  

network_data_wide <- network_data_wide %>% 
        remove_rownames %>% 
        column_to_rownames(var = "SKU")


#network_data_wide$X2nd.Item.Number <- as.numeric(network_data_wide$X2nd.Item.Number)
head(network_data_wide)

k1 <- kmeans(network_data_wide, center = 2, nstart = 5)
k2 <- kmeans(network_data_wide, center = 3, nstart = 5)
k3 <- kmeans(network_data_wide, center = 4, nstart = 5)
k4 <- kmeans(network_data_wide, center = 5, nstart = 5)
k5 <- kmeans(network_data_wide, center = 6, nstart = 5)
k6 <- kmeans(network_data_wide, center = 7, nstart = 5)
#k7 <- kmeans(network_data_wide, center = 8, nstart = 5)
#k8 <- kmeans(network_data_wide, center = 9, nstart = 5)
#k9 <- kmeans(network_data_wide, center = 10, nstart = 5)

p1 <- fviz_cluster(k1, geom = "point", network_data_wide) + ggtitle("k = 2")
p2 <- fviz_cluster(k2, geom = "point", network_data_wide) + ggtitle("k = 3")
p3 <- fviz_cluster(k3, geom = "point", network_data_wide) + ggtitle("k = 4")
p4 <- fviz_cluster(k4, geom = "point", network_data_wide) + ggtitle("k = 5")
p5 <- fviz_cluster(k5, geom = "point", network_data_wide) + ggtitle("k = 6")
p6 <- fviz_cluster(k6, geom = "point", network_data_wide) + ggtitle("k = 7")
#p7 <- fviz_cluster(k7, geom = "point", network_data_wide) + ggtitle("k = 8")
#p8 <- fviz_cluster(k8, geom = "point", network_data_wide) + ggtitle("k = 9")
#p9 <- fviz_cluster(k9, geom = "point", network_data_wide) + ggtitle("k = 10")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)

# optimal cluster number
# Elbow method(k-means clustering)
set.seed(57)

# function to compute total-within cluster sum of square 
wss <- function(k) {kmeans(network_data_wide, k, nstart = 10)$tot.withinss}

# compute and plot wss for k = 1 to k = 15
k.values = 1:15

# extract wss for 2-15 clusters
wss_values <-  map_dbl(k.values, wss)

# silhouette method
library(cluster)
set.seed(57)
silouhette_score <- function(k) {
        km = kmeans(network_data_wide, centers = k, nstart = 5)
        ss = silhouette(km$cluster, dist(network_data_wide))
        mean(ss[, 3])
}

k = 2:10
avg_sil <- sapply(k, silouhette_score)

# Gap statistic
fviz_nbclust(network_data_wide, 
             kmeans, 
             method = "gap_stat")

# Cluster number = 4
set.seed(57)
cluster_4 <- kmeans(network_data_wide, centers = 4, nstart = 2) 
cluster_4$cluster <- as.factor(cluster_4$cluster)
cluster_4    
str(cluster_4)
cluster_4$size
cluster_4$withinss

# plot elbow and silouhette plot
par(mfrow = c(2,2))
plot(k.values, wss_values,
     type = "b", pch = 19, frame = FALSE,
     main = "Elbow Plot",
     xlab = "Number of cluster K",
     ylab = "Total within clusters sum of squares")
plot(k, type = "b", avg_sil, xlab = "number of clusters", 
     main = "Silhouette Plot",
     ylab = "average silhouette scores", 
     frame = "False")

group1 <- data.frame (t (network_data_wide[cluster_4$cluster == 2,]))
group2 <- data.frame (t(network_data_wide[cluster_4$cluster == 1,]))
group3 <- data.frame (t(network_data_wide[cluster_4$cluster == 3,]))
group4 <- data.frame (t(network_data_wide[cluster_4$cluster == 4,]))

summary(sapply(group1, mean))
hist(sapply(group1, mean), main = "Histogram of Product Group 1", 
     xlab = "Number of Transactions")



# Product segmentation

# Group pie chart
library(wesanderson)
library(formattable)
slices <- c(ncol(group1), ncol(group2), ncol(group3), ncol(group4))
lbls <- c("G1", "G2", "G3", "G4")
lbls <- paste(lbls, formattable::percent(slices/sum(slices)))
pie(slices, labels = lbls, col = wes_palette("Chevalier1", length(lbls)),
    main = "Pie Chart of Product Segmentation")


# Box-Plot of product segmentation
group1_mean <- sapply(group1, mean)
group2_mean <- sapply(group2, mean)
group3_mean <- sapply(group3, mean)
group4_mean <- sapply(group4, mean)

boxplot(group1_mean, group2_mean, group3_mean, group4_mean, 
        main = "Box-Plot of Product Segmentation", 
        names = c("Group 1", "Group 2", "Group 3", "Group 4"))

lapply(list(group1_mean, group2_mean, group3_mean, group4_mean), 
       summary)

par(mfrow=c(1,2), mai = c(0.6, 0.6, 0.6, 0.6)) # set the plotting area into a 1*2 array
pie(slices, labels = lbls, col = wes_palette("Chevalier1", length(lbls)),
    main = "Pie Chart")
boxplot(group1_mean, group2_mean, group3_mean, group4_mean, 
        main = "Box-Plot", 
        names = c("G1", "G2", "G3", "G4"))



# Forecast analysis -----------------------------

# Group 1 

par(mfrow = c(1, 2))
hist(group1_mean, main = "Frequency distribution - Group 1", xlab = "Number of Transactions")
boxplot(group1_mean, main = "Box-plot - Group 1", names = c("Group1"))

idx1 <- which.min(abs(group1_mean-summary(group1_mean)["Median"]))
#print(idx1)

# Forecast
ts1 <- ts(group1[,idx1], frequency = 12)
plot(ts1)


# Group 2 
#summary(group2_mean)
par(mfrow = c(1, 2))
hist(group2_mean, main = "Histogram - Group 2", xlab = "Number of Transactions")
boxplot(group2_mean, main = "Box-plot - Group 2", names = c("Group2"))


idx2 = which.min (abs(group2_mean-summary (group2_mean)["Median"]))
print(idx2)
summary(group2[, idx2])
boxplot (group2[,idx2], main = "Box-Plot")


# decomposition
ts2 <- ts(group2[,idx2], frequency = 12)
p2 = plot(ts2)
ggAcf(ts2)
decomp_ts2 <- stl(ts2, s.window = "periodic")
deseasonal_ts2 <- seasadj(decomp_ts2)
plot(decomp_ts2)

# stationarity
library(tseries)
adf.test(ts2, alternative = "stationary")

count_d2 = diff(deseasonal_ts2, differences = 12) # differencing of order 2 is suficient
plot(count_d2)
adf.test(count_d2, alternative = "stationary")
Acf(count_d2, main='ACF for Differenced Series')
Pacf(count_d2, main='PACF for Differenced Series') 


# ARIMA
fit_arima <- auto.arima(deseasonal_ts2, seasonal = FALSE)
summary(fit_arima)
checkresiduals(fit_arima)

# Evaluate and iterate
fit_best_ts2 <- arima(deseasonal_ts2, order = c(0,1,12))
tsdisplay(residuals(fit_best_ts2), lag.max = 15, main = '(0,1,12) Model Residuals')
# summary(fit_best_ts2)

fcast <- forecast(fit_best_ts2, h = 4)
plot(fcast)

# Model performance
hold <- window(ts(deseasonal_ts2), start = 30)
fit_no_holdout = arima(ts(deseasonal_ts2[-c(30:35)]), order = c(0,1,12))

fcast_no_holdout <- forecast(fit_no_holdout,h=6)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_ts2))

# Accuracy
accuracy(fcast_no_holdout, hold)

# Average, Naïve & Drift Methods 
# p1 = autoplot(deseasonal_ts2) + autolayer(meanf(deseasonal_ts2, h = 4)) + xlab("Month") + 
#   ylab("Number of Transactions") + ggtitle("Group 2: Average Method")
# p2 = autoplot(deseasonal_ts2) + autolayer(naive(deseasonal_ts2, h = 4)) + xlab("Month") + 
#   ylab("Number of Transactions") + ggtitle("Group 2: Naive Method")
# p3 = autoplot(deseasonal_ts2) + autolayer(rwf(deseasonal_ts2, h = 4)) + xlab("Month") + 
#   ylab("Number of Transactions") + ggtitle("Group 2: Drift Method")
# grid.arrange(p1, p2, p3, ncol = 3)


# ETS (exponential smoothing)
fit_ets <- ets(deseasonal_ts2)
#summary(fit_ets)
checkresiduals(fit_ets)
autoplot(forecast::forecast(fit_ets, h = 4)) + xlab("Month") +
        ylab("Number of Transactions") + ggtitle("Group 2: Forecast using ETS Method")
forecast::forecast(fit_ets, h = 4)

fit_no_holdout_ets <- ets(deseasonal_ts2[-c(32:35)])
fcast_no_holdout_ets <- forecast(fit_no_holdout_ets, h = 4)
plot(fcast_no_holdout_ets, main =" ")
lines(ts(deseasonal_ts2))

# Accuracy
accuracy(fcast_no_holdout_ets, hold)


# Group 3
#summary(group3_mean)
par(mfrow = c(1, 2))
hist(group3_mean, main = "Histogram - Group 3", xlab = "Number of Transactions")
boxplot(group3_mean, main = "Box-plot - Group 3", names = c("Group3"))

idx3 <- which.min(abs(group3_mean-summary (group3_mean)["Mean"]))
print(idx3)
summary(group3[, idx3])
boxplot (group3[,idx3], main = "Box-Plot Group 3")

# decomposition
ts3 <- ts(group3[,idx3], frequency = 12)
p3 <-  plot(ts3)
ggAcf(ts3)

decomp_ts3 <- stl(ts3, s.window = "periodic")
deseasonal_ts3 <- seasadj(decomp_ts3)
plot(decomp_ts3)

# stationarity
adf.test(ts3, alternative = "stationary")

count_d3 <-  diff(deseasonal_ts3, differences = 2) # differencing of order 2 is suficient
plot(count_d3)
adf.test(count_d3, alternative = "stationary")
Acf(count_d3, main='ACF for Differenced Series')
Pacf(count_d3, main='PACF for Differenced Series') 

# ARIMA
fit_arima <- auto.arima(deseasonal_ts3, seasonal = FALSE)
summary(fit_arima)
checkresiduals(fit_arima)


# Evaluate and iterate

fit_best_ts3 <- arima(deseasonal_ts3, order = c(2,0,2))
tsdisplay(residuals(fit_best_ts3), lag.max = 10, main = '(2,0,2) Model Residuals') 
fcast <- forecast(fit_best_ts3, h = 4)
plot(fcast)


# ETS (exponential smoothing)
fit_ets <- ets(deseasonal_ts3)
summary(fit_ets)
checkresiduals(fit_ets)

autoplot(forecast::forecast(fit_ets, h = 4)) + xlab("Month") +
        ylab("Number of Transactions") + ggtitle("Group 3: Forecast using ETS Method")
forecast::forecast(fit_ets, h = 4)


# Model performance
hold <- window(ts(deseasonal_ts3), start = 32)
fit_no_holdout <- arima(ts(deseasonal_ts3[-c(32:35)]), order = c(2,0,2))
fcast_no_holdout <- forecast(fit_no_holdout, h = 4)
plot(fcast_no_holdout, main =" ")
lines(ts(deseasonal_ts3))

fit_no_holdout_ets <- ets(deseasonal_ts3[-c(32:35)])
fcast_no_holdout_ets <- forecast(fit_no_holdout_ets, h = 4)
plot(fcast_no_holdout_ets, main =" ")
lines(ts(deseasonal_ts3))

# simple moving average method for smoothing
# autoplot(ts3, series = "Data") + autolayer(ma(ts3, 1), series = "1-MA" ) + 
# autolayer(ma(ma(ts3, 5), 3), series = "3x5-MA") +
# scale_colour_manual(values=c("Data" = "grey50", "5-MA" = "blue", "3x5-MA" = "red"),
# breaks=c("Data","5-MA", "3x5-MA"))

# p1 = ggAcf(ts3)
# p2 = ggPacf(ts3)
# grid.arrange(p1, p2, ncol = 2)
# 
# fit_arima1 = arima(ts3, order = c(2, 1, 0))
# fit_arima2 = arima(ts3, order = c(2, 1, 1))
# fit_arima3 = arima(ts3, order = c(2, 1, 1))
# fit_arima_auto = auto.arima(ts3, seasonal = FALSE)
# print(c(fit_arima1$aic, fit_arima2$aic, fit_arima3$aic, fit_arima_auto$aic))
# summary(fit_arima_auto)
# checkresiduals(fit_arima_auto)
# 
# autoplot(forecast::forecast(fit_arima_auto, h = 1)) + xlab("Month") + 
#   ylab("Number of Transactions") + 
#   ggtitle("Group 3: Forecast using ARIMA Method")
# 
# 
# fit_arima = auto.arima(ts3, seasonal = FALSE)
# summary(fit_arima)
# checkresiduals(fit_arima)


# Average, Naïve & Drift Methods 
# p1 = autoplot(ts3) + autolayer(meanf(ts3, h = 1)) + xlab("Month") + 
#   ylab("Number of Transactions") + ggtitle("Group 3: Average Method")
# p2 = autoplot(ts3) + autolayer(naive(ts3, h = 1)) + xlab("Month") + 
#   ylab("Number of Transactions") + ggtitle("Group 2: Naive Method")
# p3 = autoplot(ts3) + autolayer(rwf(ts3, h = 1)) + xlab("Month") + 
#   ylab("Number of Transactions") + ggtitle("Group 2: Drift Method")
# grid.arrange(p1, p2, p3, ncol = 3)


# Accuracy
accuracy(fcast_no_holdout, hold)


# Group 4
#summary(group4_mean, include = FALSE)
par(mfrow = c(1, 2))
hist(group4_mean, main = "Histogram of Group 4", xlab = "Number of Transactions")
boxplot(group4_mean, main = "Box-plot of Group 4", names = c("Group4"))

idx4 = which.min (abs (group4_mean-summary (group4_mean)["Median"]))
print(idx4)
summary(group4[, idx4])
boxplot (group4[,idx4], main = "Box-Plot Group 4")

# decomposition
ts4 <- ts(group4[,idx4], frequency = 12)
p4 = plot(ts4)
ggAcf(ts4)

decomp_ts4 <- stl(ts4, s.window = "periodic")
deseasonal_ts4 <- seasadj(decomp_ts4)
plot(decomp_ts4)

# stationarity
adf.test(ts4, alternative = "stationary")
count_d4 <-  diff(deseasonal_ts4, differences = 2) # differencing of order 2 is suficient
plot(count_d4)
adf.test(count_d4, alternative = "stationary")
Acf(count_d4, main='ACF for Differenced Series')
Pacf(count_d4, main='PACF for Differenced Series') 


# ARIMA
fit_arima <- auto.arima(deseasonal_ts4, seasonal = FALSE)
#summary(fit_arima)
checkresiduals(fit_arima)


# Evaluate and iterate
fit_best_ts4 <- arima(deseasonal_ts4, order = c(0,1,2))
tsdisplay(residuals(fit_best_ts4), lag.max = 15, main = '(0,1,2) Model Residuals') 
fcast <- forecast(fit_best_ts4, h = 4)
plot(fcast)


# Model performance
hold <- window(ts(deseasonal_ts4), start = 33)
fit_no_holdout = arima(ts(deseasonal_ts4[-c(33:35)]), order = c(0,0,4))
fcast_no_holdout <- forecast(fit_no_holdout, h = 4)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_ts4))


# Accuracy
accuracy(fcast_no_holdout, hold)

# simple moving average method for smoothing
autoplot(deseasonal_ts4, series = "Data") + autolayer(ma(deseasonal_ts4, 1), series = "1-MA" ) +
        autolayer(ma(ma(deseasonal_ts4, 5), 3), series = "3x5-MA") +
        scale_colour_manual(values=c("Data" = "grey50", "5-MA" = "blue", "3x5-MA" = "red"),
                            breaks=c("Data","5-MA", "3x5-MA"))


# ETS (exponential smoothing)
fit_ets <- ets(ts4)
summary(fit_ets)
checkresiduals(fit_ets)

autoplot(forecast::forecast(fit_ets, h = 4)) + xlab("Week") +
        ylab("Number of Transactions") + ggtitle("Group 3: Forecast using ETS Method")
forecast::forecast(fit_ets, h = 4)
