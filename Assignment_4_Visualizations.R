# Visualizations for Assignment 4, Data Science
# By Hannah De los Santos

# Load Data/Libraries --------------
# load libraries
require(ggplot2)
require(ggmap)
require(zoo)

# load Boston
mydat_boston<-read.csv("C:\\Users\\delosh\\Documents\\Grad_Year_1_Fall_Semester_2016\\Data_Science\\Assignment 4\\Boston Food Inspections.csv", header=TRUE)

# load Chicago
mydat_chicago<-read.csv("C:\\Users\\delosh\\Documents\\Grad_Year_1_Fall_Semester_2016\\Data_Science\\Assignment 4\\Chicago Food Inspections.csv", header=TRUE)

# Map Visualization ------
# plotting latitude versus longitude

# mapboston <- get_map(location = c(lon = mean(df$lon,na.rm = TRUE), lat = mean(df$lat,na.rm = TRUE)), zoom = 10, maptype = "roadmap")

# boston plot
p <- ggplot(mydat_boston, aes(latitude,longitude))
p+  geom_point(aes(colour = factor(result)))

# chicago plot
p <- ggplot(mydat_chicago, aes(latitude,longitude))
p+  geom_point(aes(colour = factor(result))) + scale_color_manual(values=c("red3", "orange1", "magenta2","mediumblue","green3","mediumpurple1"))

# Time Series Analysis ----------
# Boston
par(las=2)
# finding which dates correspond to failures
fail_dates_boston <- mydat_boston$date[which(mydat_boston$result == "Fail")]
pass_dates_boston <- mydat_boston$date[which(mydat_boston$result == "Pass")]
# remove na
# fail_dates_boston <- na.omit(fail_dates_boston)
# pass_dates_boston <- na.omit(pass_dates_boston)

# figuring out the amount of failures per day
fails_per_day_boston <- as.data.frame(table(fail_dates_boston))
fails_per_day_boston$fail_dates_boston <- as.Date(fails_per_day_boston$fail_dates_boston, "%m/%d/%Y")
fails_per_day_boston<-fails_per_day_boston[order(fails_per_day_boston$fail_dates_boston),]
fails_per_day_boston<-fails_per_day_boston[nrow(fails_per_day_boston)*-1,] # removing na
# plotting the amount of failures per day for Boston
plot(Freq ~ fail_dates_boston, fails_per_day_boston, xaxt = "n", type = "l",cex.axis=.70)
axis.Date(1, at=seq(min(fails_per_day_boston$fail_dates_boston), max(fails_per_day_boston$fail_dates_boston), by="12 mon"), format="%m-%Y",cex.axis=.70)
# axis(1, fails_per_day_boston$fail_dates_boston, format(fails_per_day_boston$fail_dates_boston, "%b %d"), cex.axis = .7)

par(las=2)
# figuring out the amount of passes per day
pass_per_day_boston <- as.data.frame(table(pass_dates_boston))
pass_per_day_boston$pass_dates_boston <- as.Date(pass_per_day_boston$pass_dates_boston, "%m/%d/%Y")
pass_per_day_boston<-pass_per_day_boston[order(pass_per_day_boston$pass_dates_boston),]
pass_per_day_boston <- pass_per_day_boston[nrow(pass_per_day_boston)*-1,] # removing na
# plotting the amount of passes per day for Chicago
plot(Freq ~ pass_dates_boston, pass_per_day_boston, xaxt = "n", type = "l",cex.axis=.70)
axis.Date(1, at=seq(min(pass_per_day_boston$pass_dates_boston), max(pass_per_day_boston$pass_dates_boston), by="12 mon"), format="%m-%Y",cex.axis=.70)

# Chicago
par(las=2)
# finding which dates correspond to failures
fail_dates_chicago <- mydat_chicago$date[which(mydat_chicago$result == "Fail")]
pass_dates_chicago <- mydat_chicago$date[which(mydat_chicago$result == "Pass")]

# figuring out the amount of failures per day
fails_per_day_chicago <- as.data.frame(table(fail_dates_chicago))
fails_per_day_chicago$fail_dates_chicago <- as.Date(fails_per_day_chicago$fail_dates_chicago, "%m/%d/%Y")
fails_per_day_chicago<-fails_per_day_chicago[order(fails_per_day_chicago$fail_dates_chicago),]
fails_per_day_chicago<-fails_per_day_chicago[nrow(fails_per_day_chicago)*-1,] # removing na
# plotting the amount of failures per day for chicago
plot(Freq ~ fail_dates_chicago, fails_per_day_chicago, xaxt = "n", type = "l",cex.axis=.70)
axis.Date(1, at=seq(min(fails_per_day_chicago$fail_dates_chicago), max(fails_per_day_chicago$fail_dates_chicago), by="12 mon"), format="%m-%Y",cex.axis=.70)

par(las=2)
# figuring out the amount of passes per day
pass_per_day_chicago <- as.data.frame(table(pass_dates_chicago))
pass_per_day_chicago$pass_dates_chicago <- as.Date(pass_per_day_chicago$pass_dates_chicago, "%m/%d/%Y")
pass_per_day_chicago<-pass_per_day_chicago[order(pass_per_day_chicago$pass_dates_chicago),]
pass_per_day_chicago <- pass_per_day_chicago[nrow(pass_per_day_chicago)*-1,] # removing na
# plotting the amount of passes per day for Chicago
plot(Freq ~ pass_dates_chicago, pass_per_day_chicago, xaxt = "n", type = "l",cex.axis=.70)
axis.Date(1, at=seq(min(pass_per_day_chicago$pass_dates_chicago), max(pass_per_day_chicago$pass_dates_chicago), by="12 mon"), format="%m-%Y",cex.axis=.70)
