## clean up working environment
rm(list=ls())

#set working directory
setwd("~/Documents/Rcode")

#Elo Rating of Mexican National Team (El Triinstall.packages("rvest")
library(rvest)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
mexico <- read_html("http://www.eloratings.net/Mexico.htm")

#read data from 2nd table
# help from www.reed.edu/data-at-reed/resources/R/rvest.html
mexico2  <- mexico %>% html_nodes("table") %>% .[[2]] %>% html_table()
mexico2$X4 <- NULL
mexico2$X3 <- NULL
mexico2$X5 <- NULL
mexico2$X8 <- NULL
mexico2$X7 <- NULL
mexico2 <- mexico2[-1,]
mexico2 <- mexico2[-1,]
names(mexico2)[1]  <- "date"
names(mexico2)[2]  <- "team"
names(mexico2)[3]  <- "elo"

##now fix dates (no space between day and year)
mexico2$begdate = substr(mexico2$date, 1, nchar(mexico2$date)- 4) #Month and day
mexico2$enddate = substr(mexico2$date, nchar(mexico2$date)-3, nchar(mexico2$date))#year
mexico2$date2 <- paste(mexico2$begdate, mexico2$enddate, sep=" ") #put back together with paste
mexico2$date3 <- mdy(mexico2$date2) #convert to date in R with lubridate
mexico2$date <- mexico2$date3 #put back into date
mexico2$begdate <- mexico2$enddate <- mexico2$date2 <- mexico2$date3 <-  NULL #clear out intermediate variables

#fix elo rating, concatenated rating of both teams in one 8 digit number
mexico2$rate1 <- round(mexico2$elo/10000) #split into 1st vs. 2nd
mexico2$rate2 <- mexico2$elo- mexico2$rate1*10000

mexico2$loc <- str_locate(mexico2$team, "Mexico") #locate whether Mexico 1st or 2nd
mexico2$loc2 <- mexico2$loc[,1] #select just 1st column (start and end are the two columns)
mexico2$elo2  <- ifelse(mexico2$loc2==1, mexico2$rate1, mexico2$rate2) #pick the Mexico rating
mexico2$rate1 <- mexico2$rate2 <- mexico2$loc <-  NULL #clear out intermediate variables
##fix two Bahamas matches in 1987, elo of <1000 for Bahamas
mexico2$elo2[364] <- 1846
mexico2$elo2[365] <- 1846

mexico2$team <- "Mexico"

ggplot(mexico2, aes(x=date,y=elo2)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_labels = "%Y") +
  ylab("Elo Rating") +
  xlab("Date") + 
  ylim(1450,1990) +
  geom_text(data=mexico2[720,], y=1980, label= "Peak Mexico?", size=6, color="dark green") +
  geom_text(data=mexico2[720,], y=1950, label= "Elo=1960", size=6, color="red")