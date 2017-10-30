rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(RGoogleAnalytics)
library(lazyeval)

setwd("~/MAIN/BCC/Melinda Reports/Analysis/Data")

# Helper functions

SchoolYear <- function(data){
  data[data$month > 8 | data$month < 5,]
}

# This program generates the following reports:

## 1) iPad Usage of clubs for the current semester
### Strategic Question: Are iPads being used more frequently over time as a result of the efforts of the BCC
### This report displays over time the usage of iPads at club events to check in students. A zero in the kiosk swipe log for an event
### indicates that no iPad was used.

iPads <- read.csv("2017-2018 Club Event Attendees **ADRIEL**.csv")

## 2) OCR Job Application time series

OCRapp <- read.csv("OCR Job Application Count **ADRIEL**.csv")
OCRapp$Date <- as.POSIXct(OCRapp$OCR.Schedule.Data..Date, format = "%Y-%m-%d")
OCRapp$Job..Employer <- as.character(OCRapp$Job..Employer)
OCRapp <- OCRapp %>% filter(!grepl("fake", Job..Employer, ignore.case = TRUE)) %>% 
  filter(is.na(OCR.Schedule.Data..ID)==F) %>%
  filter(Job..Employer != "") %>%
  filter(!grepl("STDEV", Job..Employer, ignore.case = TRUE)) %>% 
  .[,-which(colnames(.) %in% c("Job..OCR.Status", "OCR.Schedule.Data..Date"))]
OCRapp$year <- as.factor(year(OCRapp$Date))
OCRapp$week <- week(OCRapp$Date)
OCRapp$month <- month(OCRapp$Date)
OCRapp$months <- months(OCRapp$Date)

### Remove any dates after the current date
OCRapp <- OCRapp[!(OCRapp$Date > Sys.time()),]

### Defines Academic School Years
OCRapp$SchoolYear <- ifelse(OCRapp$Date >= as.Date("2014-09-01") & OCRapp$Date < as.Date("2015-04-19"), "2014-2015",
                            ifelse(OCRapp$Date >= as.Date("2015-09-01") & OCRapp$Date < as.Date("2016-04-19"), "2015-2015",
                            ifelse(OCRapp$Date >= as.Date("2016-09-01") & OCRapp$Date < as.Date("2017-04-19"), "2016-2017",
                            ifelse(OCRapp$Date >= as.Date("2017-09-01") & OCRapp$Date < as.Date("2018-04-19"), "2017-2018",
                            "Spring/Summer"))))

### Creates monthly and weekly averages for Job applications
OCRapp <- OCRapp %>%
  group_by(year, month) %>%
  mutate(MonthlyAve = mean(Job..Applicants), MonthlySum = sum(Job..Applicants)) %>%
  ungroup() %>% group_by(year, week) %>%
  mutate(WeeklyAve = mean(Job..Applicants), WeeklySum = sum(Job..Applicants)) %>%
  ungroup() %>% group_by(SchoolYear) %>% 
  mutate(AvgAppsSchoolYear = mean(Job..Applicants))

### Removes any data outside of the school year (in between May and August)
OCRSchoolYear <- SchoolYear(OCRapp)

### Graphs!

anova(aov(Job..Applicants ~ SchoolYear, data=OCRapp))
fit <- lm(WeeklyAve ~ SchoolYear, data = OCRapp)
summary(fit)

ggplot(OCRapp, aes(x = SchoolYear, y = Job..Applicants, col = SchoolYear)) +
  geom_boxplot() +
  ggtitle("Job Applications per School Year")

ggplot(OCRapp, aes(x = Date, y = WeeklySum, group = year, color = SchoolYear)) + 
  geom_line() +
  ylab("Job Applications") +
  ggtitle("Total Job Application per Week: 2014 - 2018")

ggplot(OCRapp, aes(x = Date, y = WeeklyAve, group = year, color = SchoolYear)) + 
  geom_line() +
  ylab("Job Applications") +
  ggtitle("Weekly Average Applications per Job: 2014 - 2018")

ggplot(OCRapp, aes(x = Date, y = MonthlySum, group = year, color = SchoolYear)) + 
  geom_line() +
  ylab("Job Applications") +
  ggtitle("Total Job Application per Month: 2014 - 2018")

ggplot(OCRapp, aes(x = Date, y = MonthlyAve, group = year, color = year)) + 
  geom_line() +
  ylab("Job Applications") +
  ggtitle("Monthly Average Applications per Job: 2014 - 2018")

OCRapp1 <- OCRapp[-which(OCRapp$SchoolYear=="Spring/Summer" | is.na(OCRapp$SchoolYear)==T),]

ggplot(OCRapp1, aes(x = SchoolYear, y = Job..Applicants, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Job Applications") +
  ggtitle("Total Job Applications for Each School Year: 2014-2018")


ggplot(OCRapp1, aes(x = month, y = MonthlyAve, group = SchoolYear, color = year)) + 
  geom_line() +
  ylab("Job Applications") +
  ggtitle("Monthly Average Applications per Job: 2014 - 2018")


OCRapp %>%
  group_by(SchoolYear) %>%
  summarize(MeanAppsPerJob = mean(Job..Applicants))

####  Conclusion: The total amount of applications is decreasing YoY, although the average applicants per job is increasing


## 3) Company Info Session Attendance time series analysis

InfoSess <- read.csv("Company info sessions **ADRIEL**.csv")
InfoSess$Date <- as.POSIXct(InfoSess$Start.Date.Time, format = "%b %d, %Y, %l")
InfoSess$year <- as.factor(year(InfoSess$Date))
InfoSess$week <- week(InfoSess$Date)
InfoSess$month <- month(InfoSess$Date)
InfoSess$months <- months(InfoSess$Date)
InfoSess$day <- day(InfoSess$Date)

### Filter out values by criteria
InfoSess <- InfoSess %>% filter(!grepl("fake", Organization.Name, ignore.case = TRUE)) %>% 
  filter(Organization.Name!="") %>%
  filter(!grepl("Bilbo", Organization.Name, ignore.case = TRUE)) %>% 
  filter(!grepl("BYU", Organization.Name, ignore.case = TRUE)) %>% 
  filter(!grepl("ALPFA", Organization.Name, ignore.case = TRUE)) %>% 
  filter(!grepl("PreMa", Organization.Name, ignore.case = TRUE)) %>% 
  .[,-which(colnames(.) %in% c("Symplicity.Information.Session.ID", "Information.Session.Type"))]

### Only consider events with one or more attendee
InfoSess$Attendees <- InfoSess$Kiosk.Swipe.Log..Information.Sesssion.count.
InfoSess1 <- subset(InfoSess, Attendees > 0)

InfoSess1$SchoolYear <- ifelse(InfoSess1$Date >= as.Date("2014-09-01") & InfoSess1$Date <= as.Date("2015-04-19"), "2014-2015",
                            ifelse(InfoSess1$Date >= as.Date("2015-09-01") & InfoSess1$Date <= as.Date("2016-04-19"), "2015-2015",
                                   ifelse(InfoSess1$Date >= as.Date("2016-09-01") & InfoSess1$Date <= as.Date("2017-04-19"), "2016-2017",
                                          ifelse(InfoSess1$Date >= as.Date("2017-09-01") & InfoSess1$Date <= as.Date("2018-04-19"), "2017-2018",
                                                 "Spring/Summer"))))


InfoSess1 <- InfoSess1 %>%
  group_by(year, month) %>%
  mutate(MonthlyAve = mean(Attendees), MonthlySum = sum(Attendees)) %>%
  ungroup() %>% group_by(year, week) %>%
  mutate(WeeklyAve = mean(Attendees), WeeklySum = sum(Attendees)) %>%
  ungroup() %>% group_by(Organization.Name) %>% 
  mutate(AvgAttendOrg = mean(Attendees)) %>% 
  ungroup() %>% group_by(SchoolYear) %>% 
  mutate(AvgAttendSchoolYear = mean(Attendees)) %>%
  ungroup() %>% group_by(year, day) %>%
  mutate(AvgAttendDay = mean(Attendees))
  
### Graphs!

anova(aov(Attendees ~ SchoolYear, data=InfoSess1))
fit <- lm(Attendees ~ SchoolYear, data = InfoSess1)
summary(fit)

InfoSess1 <- InfoSess1[which(InfoSess1$Attendees < 125),]
InfoSess2 <- InfoSess1[which(InfoSess1$SchoolYear!="Spring/Summer"),]

ggplot(InfoSess2, aes(x = SchoolYear, y = Attendees, col = SchoolYear)) +
  geom_boxplot() +
  ggtitle("Information Session Attendees per School Year")

ggplot(InfoSess1, aes(x = Date, y = WeeklySum, group = year, color = SchoolYear)) + 
  geom_line() +
  ylab("Attendees") +
  ggtitle("Total Info Session Attendees per Week: 2014 - 2018")

ggplot(InfoSess1, aes(x = Date, y = WeeklyAve, group = year, color = SchoolYear)) + 
  geom_line() +
  ylab("Attendees") +
  ggtitle("Weekly Average Attendees per Info Session: 2014 - 2018")

ggplot(InfoSess2, aes(x = Date, y = AvgAttendDay, group = year, color = SchoolYear)) + 
  geom_line() +
  ylab("Attendees") +
  ggtitle("Daily Average Attendees per Info Session: 2014 - 2018")

ggplot(InfoSess2, aes(x = Date, y = AvgAttendDay, group = SchoolYear, col = SchoolYear)) + 
  geom_line(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Attendees") +
  ggtitle("Daily Average Attendees per Info Session: 2014 - 2018")

InfoSess1 %>%
  group_by(SchoolYear) %>%
  summarize(MeanAttendeesPerInfoSess = mean(Attendees))


ggplot(InfoSess2, aes(x = Attendees, fill = year)) +
  scale_color_manual(values = c("#89C5DA", "#DA5724", "#74D944", "#CE50CA")) +
  geom_histogram(binwidth = 10, position = "dodge") +
  facet_grid(.~ SchoolYear) +
  ggtitle("Spread of Attendees for Each School Year: 2014-2018")

## 4) CareerLaunch Traffic time series analysis


