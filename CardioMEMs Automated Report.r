### Clear environment
rm(list = ls())

library(fcuk)
library(ggplot2)
library(MASS)
library(dplyr)
library(RGoogleAnalytics)

### Google Analytics Import
setwd("~/Desktop/Giant/Clients/CardioMems")

token <- Auth("992211492004-p7melvhcdt081i26dpojf7bjh28co6al.apps.googleusercontent.com", "0B7-3WnbUiDZqz4jpW9a15Ed")

save(token,file="./token_file")

ValidateToken(token)

GetProfiles(token)

### Build the queries

# ID: Heart Failure Landing Pages, Segment: Campaign contains CardioMEMS Google display, Date: June
Q.Display <- Init(start.date = "2017-05-01",
                   end.date = "2017-05-30",
                   dimensions = "ga:date",
                   metrics = "ga:users, ga:avgTimeOnPage, ga:goalCompletionsAll, ga:impressions",
                   max.results = 10000,
                   table.id = "ga:142349263")

Q.Gender <- Init(start.date = "2017-06-01",
                  end.date = "2017-06-30",
                  dimensions = "ga:date, ga:userGender",
                  metrics = "ga:users, ga:avgTimeOnPage, ga:goalCompletionsAll, ga:sessions",
                  max.results = 10000,
                  segment = "gaid::Q8ZuKbDoRrKBx4LYUnrhvQ",
                  table.id = "ga:142349263")

Q.Channel <- Init(start.date = "2017-04-11",
                 end.date = "2017-07-01",
                 dimensions = "ga:date, ga:medium, ga:source, ga:hasSocialSourceReferral, ga:adDistributionNetwork",
                 metrics = "ga:users, ga:goalCompletionsAll, ga:sessions",
                 max.results = 10000,
                 segment = "gaid::Q8ZuKbDoRrKBx4LYUnrhvQ",
                 table.id = "ga:142349263")

Q.Video <- Init(start.date = "2017-06-01",
                  end.date = "2017-06-30",
                  dimensions = "ga:eventAction, ga:flashVersion",
                  metrics = "ga:totalEvents",
                  max.results = 10000,
                  segment = "gaid::Q8ZuKbDoRrKBx4LYUnrhvQ",
                  filter = "ga:eventCategory==Video",
                  table.id = "ga:142349263")

Q.Conversions <- Init(start.date = "2017-06-01",
                end.date = "2017-06-30",
                dimensions = "ga:deviceCategory, ga:userAgeBracket",
                metrics = "ga:users, ga:sessions, ga:goalCompletionsAll, ga:goalConversionRateAll",
                max.results = 10000,
                segment = "gaid::Q8ZuKbDoRrKBx4LYUnrhvQ",
                table.id = "ga:142349263")

Q.Age1 <- Init(start.date = "2017-06-01",
              end.date = "2017-06-30",
              dimensions = "ga:eventAction, ga:userAgeBracket",
              metrics = "ga:totalEvents",
              max.results = 10000,
              segment = "gaid::Q8ZuKbDoRrKBx4LYUnrhvQ",
              filter = "ga:eventCategory==Video",
              table.id = "ga:142349263")

### Execute the Queries
# Create the query object
ga.query <- QueryBuilder(Q.Age1)

# Fire the Query to the GA API
ga.df <- GetReportData(ga.query, token, split_daywise = T)

ga.df <- ga.df %>%
  mutate(GoalsByUser = goalCompletionsAll/users)

ga.df <- ga.df %>%
  mutate(GoalsBySession = goalCompletionsAll/sessions)

ga.df %>%
  group_by(adFormat) %>%
  summarise(N = n(), mean_goals_per_user = mean(GoalsByUser))

ga.df %>%
  group_by(userGender) %>%
  summarise(mean_goals_per_user = mean(GoalsByUser))

ga.df %>%
  group_by(adFormat) %>%
  summarise(mean_goals_per_session = mean(GoalsBySession))

## Mean Goals per session
ga.df %>%
  group_by(userGender) %>%
  summarise(mean_goals_per_session = mean(GoalsBySession))

## 65+ Conversions by device
ga.df %>%
  group_by(deviceCategory) %>%
  summarise(conversion_rate = mean(goalCompletionsAll/sessions), sessions = sum(sessions))

### Event Tracking
glimpse(ga.df)
str(ga.df)
ga.df$eventCategory <- as.factor(ga.df$eventCategory)
levels(ga.df$eventCategory)

clean.df <- ga.df[!grepl("^resize|fullscreen$|error|^seek", ga.df$eventAction),]

video.count <- clean.df %>%
  group_by(eventAction) %>%
  summarise(Clicks = sum(totalEvents))
video.count

### Age profiles
age55plus <- filter(ga.df, userAgeBracket == "55-64")
age65plus <- filter(ga.df, userAgeBracket == "65+")

age55plus %>%
  group_by(deviceCategory) %>%
  summarise(conversion_rate = mean(goalCompletionsAll/sessions), sessions = sum(sessions))

age65plus %>%
  group_by(deviceCategory) %>%
  summarise(conversion_rate = mean(goalCompletionsAll/sessions), sessions = sum(sessions))
