knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
setwd("~/Documents/Programming_for_DS/Harvard Dataverse/Assignment 03")


library(DBI)
library(dplyr)
conn <- dbConnect(RSQLite::SQLite(), "airline_project.db")


#Above codes are not needed for question 1

########## Q1 - time of year analysis ########## 


install.packages('ggplot')
install.packages('gridExtra')


library(ggplot2)
library(gridExtra)

ave_min <- c()
ave_max <- c()
pct_min <- c()
pct_max <- c()

leg <- c()
for (i in 2006) {
  leg <- c(leg, as.character(i))
  
  filename <- paste0(i, ".csv")
  df <- read.csv(filename)
  df <- df[, c("Month", "ArrDelay","Cancelled", "Diverted")]
  
  df <- df[df$Cancelled==0 & df$Diverted==0,]
  
  df <- df[, c("Month","ArrDelay")]
  
  df$Month <- as.factor(df$Month)
  
  
  # Group the data frame 'df' by the column 'Month' and count the number of rows in each group
  
  
  total_flights = as.data.frame(aggregate(df$Month,by=list(df$Month),length))[,2]
  
  # Create a new data frame 'df1' that contains only the rows from 'df' where 'ArrDelay' is greater than 0
  df1 <- df[df$ArrDelay > 0,]
  
  # Group the data frame 'df1' by the column 'Month' and count the number of rows in each group, and add the result as a new column 'delays' in 'count_df' dataframe

  delays = as.data.frame(aggregate(df1$Month,by=list(df1$Month),length))[,2]
  
  # Group the data frame 'df1' by the column 'Month' and compute the mean of 'ArrDelay' in each group, and add the result as a new column 'Delay_mean' in 'count_df' dataframe
  Delay_mean <- as.data.frame(aggregate(df1$ArrDelay,by=list(df1$Month),mean))[,2]
  
  
  pct_delays <- round((delays/total_flights)*100,2)
  
  
  Month = as.data.frame(aggregate(df1$ArrDelay,by=list(df1$Month),mean))[,1]
  
  count_df = data.frame(total_flights, delays, Delay_mean, pct_delays, Month )
  
  
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  count_df$Month = months
  

  ave_min <- c(ave_min, min(count_df$Delay_mean))
  ave_max <- c(ave_max, max(count_df$Delay_mean))
  
  pct_min <- c(pct_min, min(count_df$pct_delays))
  pct_max <- c(pct_max, max(count_df$pct_delays))
  
  #to get correct month order in plots
  count_df$Month <- as.character(count_df$Month)
  #Then turn it back into a factor with the levels in the correct order
  count_df$Month <- factor(count_df$Month, levels=unique(count_df$Month))
  
  
  p1 <- ggplot(count_df, aes(x = Month, y = pct_delays, group=1)) + 
    geom_line() + geom_point() +
    scale_fill_discrete(leg)+
    ggtitle("Percentage of delays flights vs Months in 2006 ") + 
    xlab("Month") + ylab("Pct of delay flights") + 
    theme_light() 
  

  
  p2 <- ggplot(count_df, aes(x = Month, y = Delay_mean, group=1)) + 
    geom_point() + geom_line() + 
    ggtitle("Average delay time in minutes vs Months in 2006") + 
    xlab("Month") + ylab("Average delay time (minutes)") + 
    theme_light()
  
  grid.arrange(p1, p2, ncol = 2)
}


########## Q1 - day of week analysis ##########


ave_min <- c()
ave_max <- c()
pct_min <- c()
pct_max <- c()


leg <- c()
for (i in 2006) {
  leg <- c(leg, as.character(i))
  
  filename <- paste0(i, ".csv")
  df <- read.csv(filename)
  df <- df[, c("DayOfWeek", "ArrDelay","Cancelled", "Diverted")]
  
  df <- df[df$Cancelled==0 & df$Diverted==0,]
  
  df <- df[, c("DayOfWeek","ArrDelay")]
  
  df$DayOfWeek <- as.factor(df$DayOfWeek)

  
 
  
  
  total_flights = as.data.frame(aggregate(df$DayOfWeek,by=list(df$DayOfWeek),length))[,2]
  
  df1 <- df[df$ArrDelay > 0,]
  
  
  delays = as.data.frame(aggregate(df1$DayOfWeek,by=list(df1$DayOfWeek),length))[,2]
  
 
  Delay_mean <- as.data.frame(aggregate(df1$ArrDelay,by=list(df1$DayOfWeek),mean))[,2]
  
  
  pct_delays <- round((delays/total_flights)*100,2)
  
  week <- c("Mon", 'Tue', "Wed", "Thu", "Fri", "Sat","Sun")
  
  
  count_df = data.frame(total_flights, delays, Delay_mean, pct_delays, week )
  
  
  ave_min <- c(ave_min, min(count_df$Delay_mean))
  ave_max <- c(ave_max, max(count_df$Delay_mean))
  
  pct_min <- c(pct_min, min(count_df$pct_delays))
  pct_max <- c(pct_max, max(count_df$pct_delays))
  
  #to get correct month order in plots
  count_df$week <- as.character(count_df$week)
  #Then turn it back into a factor with the levels in the correct order
  count_df$week <- factor(count_df$week, levels=unique(count_df$week))
  
  
  p1 <- ggplot(count_df, aes(x = week, y = pct_delays, group=1)) + 
    geom_line() + geom_point() +
    scale_fill_discrete(leg)+
    ggtitle("Percentage of delays flights vs Day of week in 2006 ") + 
    xlab("Day") + ylab("Pct of delay flights") + 
    theme_light() 
  
  
  
  p2 <- ggplot(count_df, aes(x = week, y = Delay_mean, group=1)) + 
    geom_point() + geom_line() + 
    ggtitle("Average delay time in minutes vs Day of week in 2006") + 
    xlab("Day") + ylab("Average delay time (minutes)") + 
    theme_light()
  
  grid.arrange(p1, p2, ncol = 2)
}

################# Q1 Day analysis ###########################

ave_min <- c()
ave_max <- c()
pct_min <- c()
pct_max <- c()


leg <- c()
for (i in 2006) {
  leg <- c(leg, as.character(i))
  
  filename <- paste0(i, ".csv")
  df <- read.csv(filename)
  df <- df[, c('DepTime','CRSDepTime', "ArrDelay","Cancelled", "Diverted")]
  
  df <- df[df$Cancelled==0 & df$Diverted==0,]
  
  
  df <- df %>%
    mutate(CRSDepTime = case_when(DepTime >= 500 & DepTime < 1200 ~ "Morning",
                                  DepTime >= 1200 & DepTime < 1700 ~ "Afternoon",
                                  DepTime >= 1700 & DepTime < 2100 ~ "Evening",
                                  DepTime >= 2100 | DepTime < 500 ~ "Night"))
  df <- rename(df, Time_of_day = CRSDepTime)
  
  df <- df[, c('Time_of_day','ArrDelay')]
  
  df$Time_of_day<- as.factor(df$Time_of_day)

  
  
  total_flights = as.data.frame(aggregate(df$Time_of_day,by=list(df$Time_of_day),length))[,2]
  
  df1 <- df[df$ArrDelay > 0,]
  

  delays = as.data.frame(aggregate(df1$Time_of_day,by=list(df1$Time_of_day),length))[,2]
  
 
  Delay_mean <- as.data.frame(aggregate(df1$ArrDelay,by=list(df1$Time_of_day),mean))[,2]
  
  Time_of_day<- as.data.frame(aggregate(df1$ArrDelay,by=list(df1$Time_of_day),mean))[,1]
  
  
  pct_delays <- round((delays/total_flights)*100,2)
  
  count_df = data.frame(total_flights, delays, Delay_mean, pct_delays, Time_of_day )
  
  
  day_time_list <- c("Morning", "Afternoon", "Evening", "Night")
  
  count_df$Time_of_day <- factor(count_df$Time_of_day, levels = day_time_list, ordered = TRUE)
  
  count_df <- count_df[order(count_df$Time_of_day),]
  
  ave_min <- c(ave_min, min(count_df$Delay_mean))
  ave_max <- c(ave_max, max(count_df$Delay_mean))
  
  pct_min <- c(pct_min, min(count_df$pct_delays))
  pct_max <- c(pct_max, max(count_df$pct_delays))
  
  
  
  p1 <- ggplot(count_df, aes(x = Time_of_day, y = pct_delays, group=1)) + 
    geom_line() + geom_point() +
    scale_fill_discrete(leg)+
    ggtitle("Percentage of delays flights vs Time of day in 2006 ") + 
    xlab("Time") + ylab("Pct of delay flights") + 
    theme_light() 
  
  
  
  p2 <- ggplot(count_df, aes(x = Time_of_day, y = Delay_mean, group=1)) + 
    geom_point() + geom_line() + 
    ggtitle("Average delay time in minutes vs Time of day in 2006") + 
    xlab("Time") + ylab("Average delay time (minutes)") + 
    theme_light()
  
  grid.arrange(p1, p2, ncol = 2)
}


#################################### QUESTION 2 ###############################


q1 <- dbGetQuery(conn, 
                 "SELECT ontime.Year, Month, DayofMonth, planes.issue_date, ArrDelay FROM ontime 
INNER JOIN planes ON ontime.TailNum=planes.TailNum
WHERE ontime.Cancelled=0 AND ontime.Diverted=0 AND issue_date IS NOT NULL AND issue_date IS NOT 'None'
AND ontime.Year = 2006 OR ontime.Year = 2007")


df <- as.data.frame(q1,stringsAsFactors = FALSE)

df <- na.omit(df)
df <- subset(df, issue_date != "None")

colnames(df)[colnames(df) == "DayofMonth"] <- "Day"
df$fly_date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"), "%Y-%m-%d")
df$issue_date <- as.Date(df$issue_date, format = "%m/%d/%Y")
df$age <- as.numeric((df$fly_date - df$issue_date) / 365.25)

df <- df[df$age >= 0, ]


library(ggplot2)

ggplot(data = df, aes(x = age)) +
  geom_histogram(bins = 20, color = "black", fill = "white") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Age Distribution") +
  xlab("Age (year)") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

new_old_cut_off <- 25

new_delays_pct <- 100*sum(df$ArrDelay > 0 & df$age < new_old_cut_off)/sum(df$age < new_old_cut_off)
old_delays_pct <- 100*sum(df$ArrDelay > 0 & df$age >= new_old_cut_off)/sum(df$age >= new_old_cut_off)

message("Percentage of delayed journeys made by new planes = ", new_delays_pct)
message("Percentage of delayed journeys made by old planes = ", old_delays_pct)


################### QUESTION 3 #######################################################
unicom2 <- function(df){
  uni_com <- unique(df$comb)
  uni_com2 <- unique(df$comb)
  uni_com3 <- unique(df$comb)
  
  for (c1 in uni_com){
    x <- unlist(strsplit(c1, "_"))
    for (c2 in uni_com2){
      y <- unlist(strsplit(c2, "_"))
      if (x[1] == y[2] & x[2] == y[1]){
        uni_com3 <- uni_com3[uni_com3 != c2]
      }
    }
  }
  return(uni_com3)
}



existing_unicom <- function(countdf, uni_com2){
  comlist <- countdf$comb
  for (c1 in uni_com2) {
    x <- strsplit(c1, "_")[[1]]
    i <- 1
    for (c2 in comlist) {
      y <- strsplit(c2, "_")[[1]]
      if (y[1] == x[2] && y[2] == x[1]) {
        comlist[i] <- c1
      }
      i <- i + 1
    }
  }
  return(comlist)
}



for (k in 2003:2007) {

  dfname <- paste0(k, "temp.csv")
  filename <- paste0(k, ".csv")
  
  df <- read.csv(filename)
  df <- subset(df, Cancelled == 0 & Diverted == 0)
  df <- df[, c("Origin", "Dest")]
  
  df$comb <- paste(df$Origin, df$Dest, sep = "_")
  
  uni_com <- unique(df$comb)
  
  uni_com2 <- unicom2(df)
  
  countdf <- as.data.frame(table(df$comb))
  colnames(countdf) <- c("comb", "count")
  
  comlist <- existing_unicom(countdf, uni_com2)
  
  countdf$unicom <- comlist
  
  
  countdf_grouped <- countdf %>% group_by(unicom) %>% summarize(sum = sum(count))
  countdf_sorted <- countdf_grouped %>% arrange(desc(sum))
  
  final_df <- countdf_sorted
  
  
  colname <- paste0("num_journeys_", k)
  
  colnames(final_df)[colnames(final_df) == "sum"] <- colname
  
  
  if (k == 2003) {
    merged <- final_df
  } else {
    merged <- merge(merged, final_df, by = "unicom", all = FALSE)
  }

}



library(ggplot2)
library(gridExtra)



ggplot(merged, aes(x = num_journeys_2005)) +
  geom_histogram(aes(fill = "2005"), alpha = 0.2) +
  geom_histogram(aes(x = num_journeys_2006, fill = "2006"), alpha = 0.2) +
  geom_histogram(aes(x = num_journeys_2007, fill = "2007"), alpha = 0.2) +
  scale_fill_manual(values = c("yellow","blue","red"), name = "Year") +
  theme_classic() +
  labs(x = "Number of journeys", y = "Counts", title = "Histogram of journeys by year")


subset <- merged[order(-merged$`num_journeys_2005`),]
subset <- subset[1:12,]



subset = subset[order(subset$num_journeys_2005),]


library(ggplot2)



ggplot(subset, aes(x = unicom, group=1)) +
  geom_line(aes(y = num_journeys_2003, color = "2003")) +
  geom_line(aes(y = num_journeys_2004, color = "2004")) +
  geom_line(aes(y = num_journeys_2005, color = "2005")) +
  geom_line(aes(y = num_journeys_2006, color = "2006")) +
  geom_line(aes(y = num_journeys_2007, color = "2007")) +
  scale_color_manual(values = c("2003" = "red", "2004" = "blue",
                                "2005" = "green","2006" = "black","2007" = "orange"),
                     name = "Year") +
  ggtitle("Line Plot of number of journeys")+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))


##################################### QUESTION 4 ####################################

q1 <- dbGetQuery(conn, 
                 "SELECT ontime.Year, Month, DayofMonth, CRSArrTime, CRSDepTime, 
    ontime.TailNum, ArrDelay, DepDelay, Origin, Dest, 
    CarrierDelay, WeatherDelay, NASDelay,SecurityDelay, LateAircraftDelay
FROM ontime
INNER JOIN planes ON ontime.TailNum=planes.TailNum
WHERE ontime.Cancelled=0 AND ontime.Diverted=0
AND ontime.Year = 2006")


df <- as.data.frame(q1,stringsAsFactors = FALSE)

colnames(df)[colnames(df) == 'DayofMonth'] <- 'Day'

df$fly_date <- as.Date(paste(df$Year, df$Month, df$Day, sep="-"))

df <- df[, !(names(df) %in% c("Year", "Month", "Day"))]

df <- df[(df$CarrierDelay > 0 | df$WeatherDelay > 0 | df$NASDelay > 0 | df$SecurityDelay > 0 | df$LateAircraftDelay > 0),]
df <- df[-1,]

tailnum <- unique(df$TailNum)

df1 <- df[df$TailNum == tailnum[1], ]

df1 <- df1[order(df1$fly_date, df1$CRSArrTime, df1$ArrDelay), ]

day <- names(sort(table(df1$fly_date), decreasing = TRUE)[1])

df2 <- df1[df1$fly_date == day, ]

df2 <- df2[, -1]

airports <- read.csv("airports.csv")

airports$Origin <- airports$iata

airports <- airports[, c("Origin", "lat", "long")]

airports2 <- airports

colnames(airports) <- c("Origin", "ori_lat", "ori_long")

colnames(airports2) <- c("Dest", "des_lat", "des_long")

# Add an index column to df2
df2$index <- 1:nrow(df2)

result <- merge(df2, airports, by = "Origin", sort = FALSE, all.x = TRUE)

result <- merge(result, airports2, by = "Dest", sort = FALSE, all.x = TRUE)

result <- result[order(result$index), ]

result$index <- NULL

install.packages('plotly')

library(plotly)


########################### Q-4 plottings ################################



make_arrow <- function(i, df) {
  A <- matrix(c(df$ori_long[i], df$ori_lat[i]), nrow = 1)
  B <- matrix(c(df$des_long[i], df$des_lat[i]), nrow = 1)
  v <- B - A
  w <- v / norm(v)
  u <- matrix(c(-v[1], v[2]), nrow = 1) # u orthogonal to w
  
  P <- B - l * w
  S <- P - widh * u
  T <- P + widh * u
  
  return(list(S, T, B, A))
}



fig <- plot_ly()

# Add the trace for destinations
fig <- fig %>%
  add_trace(data = result,
            type = 'scattergeo',
            locationmode = 'USA-states',
            lon = ~des_long,
            lat = ~des_lat,
            text = ~Dest,
            textfont = list(color = 'black', family = 'Times New Roman', size = 14),
            textposition = 'top left',
            name = 'Destinations',
            mode = 'markers+text+lines',
            marker = list(size = 10, color = 'black', line = list(color = 'black', width = 0.5), opacity = 0.5, sizemode = 'area')
  )

# Add the trace for origins
fig <- fig %>%
  add_trace(data = result,
            type = 'scattergeo',
            locationmode = 'USA-states',
            lon = ~ori_long,
            lat = ~ori_lat,
            text = ~Origin,
            textfont = list(color = 'black', family = 'Times New Roman', size = 14),
            textposition = 'top left',
            name = 'Origins',
            mode = 'markers+text+lines',
            marker = list(size = 10, color = 'black', line = list(color = 'black', width = 0.5), opacity = 0.5, sizemode = 'area')
  )

fig


# Set the arrow length and width
l <- 0.5
widh <- 0.035

# Create a list of colors
color_list <- c("blue", "red", "orange", "green", "purple", "brown", "pink")

# Iterate through the data

fig <- plot_ly()

for (i in 1:nrow(result)) {
  
  # Create the arrow coordinates
  S <- make_arrow(i, result)[[1]]
  T <- make_arrow(i, result)[[2]]
  B <- make_arrow(i, result)[[3]]
  A <- make_arrow(i, result)[[4]]
  
  # Add the arrow trace to the plot
  fig <- fig %>%
    add_trace(fig, x = c(S[1], T[1], B[1], S[1]),
                   y = c(S[2], T[2], B[2], S[2]),
                   mode = "lines",
                   fill = "toself",
                   fillcolor = color_list[i],
                   line_color = color_list[i])
  
  # Add the text trace to the plot
  fig <- fig %>%
    add_trace(fig, x = 0.25*A[1] + 0.75*B[1],
                   y = 0.25*A[2] + 0.75*B[2],
                   type = "scatter",
                   mode = "text",
                   text = paste("Flight", i, ", Delay = ", result$ArrDelay[i], "mins"),
                   textfont = list(color = color_list[i],
                                   family = "Times New Roman",
                                   size = 14))
}

# Update the layout of the plot
fig <- layout(fig,
              width = 1900,
              height = 1000,
              showlegend = F,
              mapbox_style = "carto-positron",
              geo = list(scope = "usa",
                         landcolor = "rgb(217, 217, 217)"))



# Add labels and title
fig <- fig %>%
  layout(title = "Origins and Destinations", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))



######################################## QUESTION 5 ########################
library(DBI)
library(dplyr)

library(plyr)

conn <- dbConnect(RSQLite::SQLite(), "airline_project.db")


q1 <- dbGetQuery(conn, 
                 "SELECT  t1.Month, t1.DayofWeek, t1.CRSDepTime, t1.CRSArrTime,
                    t1.UniqueCarrier, t1.TailNum, t1.CRSElapsedTime, t1.Distance,
                    t1.Ostate, t1.Olat, t1.Olong, t1.ArrDelay,
                    state AS Dstate, lat AS Dlat, long AS Dlong
                    FROM
                    (SELECT 
                    ontime.Year, Month, DayofMonth, DayOfWeek,
                    CRSDepTime, CRSArrTime,
                    UniqueCarrier, ontime.TailNum,
                    CRSElapsedTime, Distance, Dest,
                    state AS Ostate, lat AS Olat, long AS Olong,
                    ArrDelay
                    FROM ontime
                    LEFT JOIN airports ON ontime.Origin=airports.iata
                    WHERE ontime.Cancelled=0 AND ontime.Diverted=0 
                    AND ontime.Year = 2007) t1
                    LEFT JOIN airports ON t1.Dest=airports.iata WHERE Ostate IS NOT NULL AND Dstate iS NOT NULL")


df <- as.data.frame(q1,stringsAsFactors = FALSE)


df <- df[sample(nrow(df),0.01*nrow(df)),]

df <- df[order(1:nrow(df)),]


days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

df$DayOfWeek <- mapvalues(df$DayOfWeek, from = c(1, 2, 3, 4, 5, 6, 7), to = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

Months <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
df$Month <- Months[as.numeric(df$Month)]


df$Month<- as.factor(df$Month)
df$DayOfWeek<- as.factor(df$DayOfWeek)
df$UniqueCarrier<- as.factor(df$UniqueCarrier)
df$TailNum<- as.factor(df$TailNum)
df$Ostate<- as.factor(df$Ostate)
df$Dstate<- as.factor(df$Dstate)

df[sapply(df, is.factor)] <- data.matrix(df[sapply(df, is.factor)])



install.packages('tree')
library(tree)

set.seed(1)
train <- sample(1:nrow(df), nrow(df)*0.7)
tree.delay <- tree(ArrDelay ~ ., df, subset = train)
summary(tree.delay)

plot(tree.delay)
text(tree.delay, pretty = 0)

yhat <- predict(tree.delay, newdata = df[-train, ])
delay.test <- df[-train, "ArrDelay"]
plot(yhat, delay.test)
abline(0, 1)
mean((yhat - delay.test)^2)





