library(readr)


#Getting the current dir and setting the working dir
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

messy_data <- read_csv("messy-data.csv")
View(messy_data)

summary(messy_data)

#Creates a vector of Trues and Falses for completeness of data
complete.cases(messy_data)

#You can use this to index the data
messy_data[!complete.cases(messy_data),]

#To exclude the NA values
na.omit(messy_data)

summary(na.omit(messy_data))

#Checks a specific column for completeness
na_prices <-is.na(messy_data$price)
#Gives you which values are true
which(na_prices)

#You can also subtract indices from a dataframe to remove values
removed_missing_prices <- messy_data[-which(na_prices),]
removed_missing_prices

#Shows a better job of creating a summary of non-numerical data
table(messy_data$item)

#To clean up formatting of item names
messy_data$item <-tolower(messy_data$item)
table(messy_data$item)

# Will replace values in the given column of the dataframe
messy_data$item<- sub("nachoes", "nachos", messy_data$item)
table(messy_data$item)

#You need to be careful though, because this will change "french fries" to "french french fries"
sub("french fries", "fries", messy_data$item)

#ifeslse statement will fix this though
messy_data$item <- ifelse(messy_data$item == "fries", "french fries", messy_data$item)
table(messy_data$item)

#You can also use pattern matching with regular expressions (* means anything beginning or end of the expression)
regexpr("*mac*",messy_data$item)

#You can do the same thing to get just true or false values
mac_list <-grepl("*mac*",messy_data$item)
messy_data$item <- ifelse(mac_list, "mac-n-cheese", messy_data$item)
table(messy_data$item)

#Cleaning up the date format to convert it to a date datatype
messy_data$date <- as.Date(messy_data$date, format="%m/%d/%y")
summary(messy_data)

#We can fill in the data for the missing prices using the average prices
messy_data[which(is.na(messy_data$price)),]

#To do this we can find the mean price of each item by item type and month
aggregate(messy_data$price, list(messy_data$item,months(messy_data$date)),mean, na.rm=T)

#string split function can be used to break up the addtnl column
split_addTnl <-strsplit(messy_data$addtnl,"-")
#strsplit(messy_data$addtnl, "-")[[900]][2]
#strsplit(messy_data$addtnl, "-")[900]

#Create a new column for the weather data
messy_data$weather <- ""
#Then we loop through the data and insert the split off weather value from our list into the new column
for (x in 1:12142) messy_data[x,7] <- split_addTnl[[x]][1]

messy_data$name <-""
for (x in 1:12142) messy_data[x,8] <- split_addTnl[[x]][2]

messy_data$tipped <- ""
for (x in 1:12142) messy_data[x,9] <- split_addTnl[[x]][3]

messy_data <- messy_data[-c(6)]
