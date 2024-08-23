library(dplyr)
library(lubridate)
library("RColorBrewer")
library(scales)
library(ggplot2)
library(ggnetwork)
library(igraph)
library(scales)
library(network)
library(sna)
library(ggraph)
library("intergraph")
library(caTools)
library(nnet)
library(broom)
library(caret)

setwd('/Users/lowshawne/Desktop/ST2195 Coursework/dataverse_files')

airports = read.csv("airports.csv")

carriers = read.csv("carriers.csv")
planes = read.csv("plane-data.csv")

df_2005 = read.csv("2005.csv")
df_2006 = read.csv("2006.csv")
df_2007 = read.csv("2007.csv")

# Merging all files
df = rbind(df_2005, df_2006, df_2007)
# Sample 50% of total data
df = df %>% sample_frac(0.5, replace = FALSE)
# Remove Cancelled flights
df = df[df$Cancelled != 1, ]
# Remove NAs
df = na.omit(df)
summary(df)

week_dict = list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
names(week_dict) = c(1, 2, 3, 4, 5, 6, 7)
month_dict = list("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")
names(month_dict) = c(1:12)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Q1(a)  When is the best time of day to fly to minimise delays?

# Remove values >2400
df = df[df$DepTime <= 2359,]

# Convert DepTime to character format
df$DepTime = sprintf("%04d", df$DepTime)
df$DepTime = as.character(df$DepTime)
# Extract the first two characters (hours) and convert to numeric
df$dep_hour = as.numeric(substr(df$DepTime, 1, 2))

# Create hourly delay
hourly_delay = aggregate(DepDelay ~ dep_hour, data = df, FUN = mean)

# Set the width of the plotting window
options(repr.plot.width=6, repr.plot.height=4)

# Create the bar plot
barplot(hourly_delay$DepDelay, names.arg = hourly_delay$dep_hour, xlab = "Hour of the day", ylab = "Average delay (minutes)", main = "Best time of day to fly to minimize delays")

# Best time to fly with no delays: 5am

# Q1b Best day of the week to fly to minimize delays
# Calculate average delay by day of the week
daily_delay = aggregate(DepDelay ~ DayOfWeek, data = df, FUN = mean)

# Sort the data frame by DepDelay column in ascending order
daily_delay_sorted = daily_delay[order(daily_delay$DepDelay),]

# Create a barplot with sorted data
barplot(daily_delay_sorted$DepDelay, names.arg = week_dict[daily_delay_sorted$DayOfWeek], 
        xlab = "Day of the week", ylab = "Average delay (minutes)", 
        main = "Average delay by day of the week")

# Create pie chart for days in week
# Calculate percentages
daily_delay$percent = round(100*daily_delay$DepDelay/sum(daily_delay$DepDelay), 1)
# Create pie chart with percentage labels and week day names
pie(daily_delay$DepDelay, labels = paste(week_dict[daily_delay$DayOfWeek], daily_delay$percent, "%"), main = "Best day of Week to fly to minimize delays", names.arg = week_dict)

# Best Day Of Week to avoid delay: Saturday

# Q1c Best time of year to fly to minimize delays?
# Best month to fly to minimize delays
Monthly_delay = aggregate(DepDelay ~ Month, data = df, FUN = mean)
# Sort Monthly_delay by DepDelay in ascending order
Monthly_delay_sorted = Monthly_delay[order(Monthly_delay$DepDelay),]

# Create barplot with sorted data
barplot(Monthly_delay_sorted$DepDelay, 
        names.arg = month_dict[as.character(Monthly_delay_sorted$Month)], 
        xlab = "Month", 
        ylab = "Average delay (minutes)", 
        main = "Average delay by month")

# Best Month to fly to mimimize delays is September

# Best quarter to fly to minimize delay
# create a new column 'Quarter'
df$Quarter = cut(df$Month, breaks = c(0, 3, 6, 9, 12), labels = c("Q1", "Q2", "Q3", "Q4"))

# Calculate average delay per quarter
quarterly_delay = aggregate(DepDelay ~ Quarter, data = df, FUN = mean)
# Sort quarterly_delay by DepDelay
quarterly_delay_sorted = quarterly_delay[order(quarterly_delay$DepDelay),]
# Get the sorted order of quarter names
order_quarters = order(quarterly_delay_sorted$Quarter)

# Generate the barplot with sorted order
barplot(quarterly_delay_sorted$DepDelay[order_quarters], 
        names.arg = quarterly_delay_sorted$Quarter[order_quarters], 
        xlab = "Quarter of the year", 
        ylab = "Average delay (minutes)", 
        main = "Best Quarter to fly to minimize delays")

# Calculate percentages
quarterly_delay$percent = round(100*quarterly_delay$DepDelay/sum(quarterly_delay$DepDelay), 1)
# Create pie chart with percentage labels
pie(quarterly_delay$DepDelay, labels = paste(quarterly_delay$Quarter, quarterly_delay$percent, "%"), main = "Best Quarter to fly to minimize delays")
# Best Quarter to fly to minimize delay is Q2

# EXTRA: Does number of flights affect Departure Delay?
# Merge df and airports datasets based on iata column
AK_airports = airports %>% filter(state == 'AK')
# Rename iata to origin for merging
colnames(AK_airports)[which(names(AK_airports) == "iata")] = "Origin"
# merge with origin iata
AK_airports_data = merge(AK_airports,df, by ='Origin')
#subset data
AK_airports_data =subset(AK_airports_data,select = c(DayofMonth,DepDelay))
# Count number of flights in each day of month
AK_airports_data <- AK_airports_data %>%
  group_by(DayofMonth) %>%
  mutate(flight_count = n())
# Average delay by day
AK_airports_data <- AK_airports_data %>%
  group_by(DayofMonth) %>%
  mutate(flight_count = n(),
         avg_delay_by_day = mean(DepDelay, na.rm = TRUE)) %>%
  ungroup()
#Remove outlier ==31 (only have 1172 flights on average)
AK_airports_data = filter(AK_airports_data,DayofMonth != 31)
# plot scatter plot
plot(AK_airports_data$flight_count, AK_airports_data$avg_delay_by_day,
     xlab = "Average Flight Counts in a Day", ylab = "Average Delay by Day",
     main = "Effect of Air Traffic on Delay",
     sub = 'In AK state')

# Add regression line
abline(lm(avg_delay_by_day ~ flight_count, data = AK_airports_data), col = "red")
# Linear Regression results
model_airtraffic_AK = lm(avg_delay_by_day ~ flight_count, data = AK_airports_data)
summary(model_airtraffic_AK)

# Q2 Do older planes suffer more delays?~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Processing
planes = na.omit(planes)
colnames(planes)[which(names(planes) == "tailnum")] = "TailNum"
merged_df = merge(df, planes, by = "TailNum", all.x = TRUE)
merged_df = na.omit(merged_df)
colnames(merged_df)[which(names(merged_df) == "Year")] = "Flight Year"
planesdelay_df = merged_df[c('TailNum','DepDelay','year','model')]
planesdelay_df = planesdelay_df[planesdelay_df$year != 'None', ]
planesdelay_df = planesdelay_df[planesdelay_df$year != "",]
planesdelay_df = planesdelay_df[planesdelay_df$year != "0000",]

# Create age column
base_year = 2008
planesdelay_df$year = as.numeric(planesdelay_df$year)
planesdelay_df['age']= base_year - planesdelay_df['year']
planesdelay_df$year = NULL # Remove year column

# Calculate average departure delay by age
avg_delay_by_age = aggregate(planesdelay_df$DepDelay, by=list(age=planesdelay_df$age), FUN=mean)

# Plot age vs. average departure delay
plot(avg_delay_by_age$age, avg_delay_by_age$x, xlab = "Age", ylab = "Average Departure Delay (minutes)", main = "Age vs. Avg Departure Delay")

# Add regression line
abline(lm(x ~ age, data = avg_delay_by_age), col = "red")

# Fit linear regression model
model = lm(x ~ age, data = avg_delay_by_age)

# View regression results
summary(model)
# The p-value for the age variable is 0.652, which is greater than 0.05, indicating that there is no significant linear relationship between age and DepDelay



#Question 3 – How does the number of people flying between different airport change over time?~~~~~~~~~~~~~~~~~~~~~~~

# Remove Cancelled flights
df_2005 = df_2005[df_2005$Cancelled != 1, ]
df_2006 = df_2006[df_2006$Cancelled != 1, ]
df_2007 = df_2007[df_2007$Cancelled != 1, ]

# Set Origin & Dest 
airport_2005 = df_2005[c('Origin','Dest','Year')]
airport_2006 = df_2006[c('Origin','Dest','Year')]
airport_2007 = df_2007[c('Origin','Dest','Year')]

# Create Origin & Dest Column
airport_2005$'Origin & Dest' = paste(airport_2005$Origin, airport_2005$Dest, sep = "")
airport_2006$'Origin & Dest' = paste(airport_2006$Origin, airport_2006$Dest, sep = "")
airport_2007$'Origin & Dest' = paste(airport_2007$Origin, airport_2007$Dest, sep = "")

# Remove NA
airport_2005 = na.omit(airport_2005)
airport_2006 = na.omit(airport_2006)
airport_2007 = na.omit(airport_2007)

# Combine df
total_airport = rbind(airport_2005,airport_2006,airport_2007)

# Find Most common airport paths 
top_values_2005 = airport_2005$`Origin & Dest` %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head(5)

print(top_values_2005)


top_values_2006 = airport_2006$`Origin & Dest` %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head(5)

print(top_values_2006)

top_values_2007 = airport_2007$`Origin & Dest` %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head(5)

print(top_values_2007)

# Most Common airport flights are: 
#2005 LAXSAN SANLAX BOSLGA LGABOS LAXLAS 
#2006 LAXSAN SANLAX OGGHNL HNLOGG LAXLAS 
#2007 LAXSAN SANLAX LAXLAS BOSLGA LASLAX 

# Group the data by Origin, Dest, and Year and count the number of flights between each airport pair
flights_by_route = total_airport[c('Origin', 'Dest', 'Year','Origin & Dest')]

# Filter for specific airport pairs and group by year to get the number of flights between each pair over time
lax_san_flights = flights_by_route %>%
  filter(Origin == "LAX" & Dest == "SAN") %>%
  group_by(Year) %>%
  summarise(count = n())

san_lax_flights = flights_by_route %>%
  filter(Origin == "SAN" & Dest == "LAX") %>%
  group_by(Year) %>%
  summarise(count = n())

bos_lga_flights = flights_by_route %>%
  filter(Origin == "BOS" & Dest == "LGA") %>%
  group_by(Year) %>%
  summarise(count = n())

lga_bos_flights = flights_by_route %>%
  filter(Origin == "LGA" & Dest == "BOS") %>%
  group_by(Year) %>%
  summarise(count = n())

lax_las_flights = flights_by_route %>%
  filter(Origin == "LAX" & Dest == "LAS") %>%
  group_by(Year) %>%
  summarise(count = n())

ogg_hnl_flights = flights_by_route %>%
  filter(Origin == "OGG" & Dest == "HNL") %>%
  group_by(Year) %>%
  summarise(count = n())

hnl_ogg_flights = flights_by_route %>%
  filter(Origin == "HNL" & Dest == "OGG") %>%
  group_by(Year) %>%
  summarise(count = n())

las_lax_flights = flights_by_route %>%
  filter(Origin == "LAS" & Dest == "LAX") %>%
  group_by(Year) %>%
  summarise(count = n())

hnl_lih_flights = flights_by_route %>%
  filter(Origin == "HNL" & Dest == "LIH") %>%
  group_by(Year) %>%
  summarise(count = n())


# Define colors and labels for the legend
colors = c("red", "blue", "green", "orange", "purple", "black", "yellow", "brown", "pink")
labels = c("LAX-SAN", "SAN-LAX", "BOS-LGA", "LGA-BOS", "LAX-LAS", "OGG-HNL", "HNL-OGG", "LAS-LAX", "HNL-LIH")

# Create the graph with the legend
ggplot() +
  geom_line(data = lax_san_flights, aes(x = Year, y = count, color = "LAX-SAN"), size = 1, linetype = "solid") +
  geom_line(data = san_lax_flights, aes(x = Year, y = count, color = "SAN-LAX"), size = 1, linetype = "solid") +
  geom_line(data = bos_lga_flights, aes(x = Year, y = count, color = "BOS-LGA"), size = 1, linetype = "solid") +
  geom_line(data = lga_bos_flights, aes(x = Year, y = count, color = "LGA-BOS"), size = 1, linetype = "solid") +
  geom_line(data = lax_las_flights, aes(x = Year, y = count, color = "LAX-LAS"), size = 1, linetype = "solid") +
  geom_line(data = ogg_hnl_flights, aes(x = Year, y = count, color = "OGG-HNL"), size = 1, linetype = "solid") +
  geom_line(data = hnl_ogg_flights, aes(x = Year, y = count, color = "HNL-OGG"), size = 1, linetype = 'solid') +
  geom_line(data = las_lax_flights, aes(x = Year, y = count, color = "LAS-LAX"), size = 1, linetype = 'solid') +
  geom_line(data = hnl_lih_flights, aes(x = Year, y = count, color = "HNL-LIH"), size = 1, linetype = 'solid') +
  scale_color_manual(name = "Airport Pairs", 
                     values = colors, 
                     labels = labels) +
  labs(x = "Year", y = "Number of Flights", 
       title = "Number of Flights by Year for Most Popular Airports") +
  theme_minimal()

# Q4. Can you detect cascading failures as delays in one airport create delays in others?~~~~~~~~~~~~~~~~~~~~~~~~
#Calculate the number of delayed flights for each origin-destination pair
flights = df[c('Year',"Origin", "Dest", "DepDelay")]
# filter 2007 data only
flights_2007 = flights %>%
  filter(Year == 2007)
flights_2007 = flights_2007 %>%
  select(-Year)
# Total Number of flights for each unique Origin & Dest
flights_2007 = flights_2007 %>%
  group_by(Origin, Dest) %>%
  mutate(totalflights = n()) %>%
  ungroup()
# Create new df for avg delay for each unique flight
flights_2007_grouped = flights_2007 %>%
  group_by(Origin, Dest) %>%
  summarize(sum_delay = sum(DepDelay),
            totalflights = n()) %>%
  mutate(avg_delay = sum_delay / totalflights)
# Create column to show if delayed
flights_2007_grouped = flights_2007_grouped %>%
  mutate(delayed = ifelse(avg_delay > 0, "1", "0"))

summary(flights_2007_grouped) # 3rd Quantile for avg_delay = 14.65

# Create colour cateogory for avg delay
flights_2007_grouped = flights_2007_grouped %>%
  mutate(color_category = case_when(
    avg_delay <= 0 ~ "green",
    avg_delay < 14.64 ~ "yellow",
    TRUE ~ "red"
  ))

# Extract Origin & Dest
flights_pair = data_frame(flights_2007_grouped$Origin, flights_2007_grouped$Dest)
net = graph.data.frame(flights_pair,directed = T)
# Label the vertex
V(net)$label = igraph::degree(net)
# Add weight
V(net)$degree = igraph::degree(net)

# Network diagram
set.seed(1244)
plot(net, 
     vertex.color = flights_2007_grouped$color_category,
     vertex.size = 5,
     edge.arrow.size = 0.05,
     vertex.label.cex=0.8)

# Q5. Use the available variables to construct a model that predicts delays.
# Discard unwanted data
# Filter out rows where year is 0 or None
merged_df = merged_df[!(merged_df$year %in% c("0", "None")),]

# Replace '0000' with NA
merged_df$year[merged_df$year == '0000'] = NA

# Drop rows with NA in the 'year' column
merged_df = na.omit(merged_df)

# Calculate age of the planes
merged_df$`Plane age` = base_year - as.numeric(merged_df$year)

ml_data = merged_df %>%
  select(-c(issue_date, year, ActualElapsedTime, ArrDelay, CarrierDelay, 
            WeatherDelay, SecurityDelay, LateAircraftDelay, NASDelay, TailNum,
            status, DepTime, Quarter, CancellationCode, Cancelled, Diverted,
            UniqueCarrier, type, CRSArrTime, ArrTime, manufacturer, engine_type,
            model, dep_hour)) %>%
  na.omit()

# Convert Origin to a numeric variable
ml_data$Origin_num = as.numeric(factor(ml_data$Origin))

# Convert Dest to a numeric variable
ml_data$Dest_num = as.numeric(factor(ml_data$Dest))

# Convert aircraft_type to a numeric variable
ml_data$aircraft_type_num = as.numeric(factor(ml_data$aircraft_type))
# remove original columns
ml_data = subset(ml_data,select = -c(Origin,Dest, aircraft_type))

# Train-test split for Plane age & Distance~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set seed for reproducibility
set.seed(200)

split = createDataPartition(ml_data$DepDelay, p = 0.7, list = FALSE)
train = ml_data[split, ]
test = ml_data[-split, ]

# Scale data and convert factor columns to numeric
train_scaled = scale(train)
test_scaled = scale(test)
train_dum = model.matrix(~ ., data = train)
test_dum = model.matrix(~ ., data = test)
train_dum[,-1] = train_scaled
test_dum[, -1] = test_scaled

# Fit linear regression model on train set
m = lm(DepDelay ~ ., data = as.data.frame(train_dum))
summary(m) %>% tidy()

# Make predictions on test set
predictions = predict(m, newdata = as.data.frame(test_dum))

# Calculate RMSE on test set
RMSE = sqrt(mean((as.data.frame(test)$DepDelay - predictions)^2))
RMSE

# Logistic Regression with categorical Y values.~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Binary column for DepDelay
set.seed(1999)
ml_data$delayed = ifelse(ml_data$DepDelay > 0, 1,0)
# Remove DepDelay column
ml_data2 = subset(ml_data,select=-c(DepDelay))
# Sample of 1,000,000 due to size 
ml_data2 <- ml_data2 %>%
  sample_n(size = 5000, replace = FALSE)
# Convert to factor variables
ml_data2$delayed = as.factor(ml_data2$delayed)
ml_data2$Origin_num = as.factor(ml_data2$Origin_num)
ml_data2$Dest_num = as.factor(ml_data2$Dest_num)
ml_data2$Month = as.factor(ml_data2$Month)
ml_data2$aircraft_type_num = as.factor(ml_data2$aircraft_type_num)
ml_data2$DayofMonth = as.factor(ml_data2$DayofMonth)
ml_data2$DayOfWeek = as.factor(ml_data2$DayOfWeek)

# Logistic Regression with categorical Y values.
delayed.fit = multinom(delayed ~ Origin_num+Dest_num+Month+aircraft_type_num+DayofMonth
                        +DayOfWeek, data = ml_data2)
summary(delayed.fit)

OR = exp(coef(delayed.fit))
OR

OR.CI = exp(confint(delayed.fit))
OR.CI
# Z value calculation
z = summary(delayed.fit)$coefficients/summary(delayed.fit)$standard.errors
pvalue = (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue

# Output the logistic function prob for all cases.
prob = predict(delayed.fit, type = 'prob')
prob

# Model predicted anger index based on max prob among the categories.
predicted_class = predict(delayed.fit)

# Create a confusion matrix with actuals on rows and predictions on columns based on the entire dataset.
table(ml_data2$delayed, predicted_class)

# Overall Accuracy
mean(predicted_class == ml_data2$delayed)

# Significant variable: month

