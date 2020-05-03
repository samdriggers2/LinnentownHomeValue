#######################################################################################################
# Project Title: "Racial Disparities in Athens' Urban Renewal Project GA R-50"                        #
# Code Title: "Linnentown_Project_HomeValues_Code"													  #
#																									  #
# Author: Samuel Driggers (samtdriggers@gmail.com) is the author of this code and its associated      #
# analysis/Tableau dashboard.																		  #
#																									  #
# Data is drawn from the University of Georgia's Community Mapping Lab and Special Collections Library#
#######################################################################################################

##clean-up
rm(list=ls())

##import data
getwd()
setwd("/Users/SamDriggers/Desktop/CGIS_Final_Proj")
data <- read.csv("Linnentown_Project_HomeValues_Data.csv")
summary(data)
nrow(data)

##recode race and overall condition
data$race <- as.numeric(data$race)
data$cond_overall <- as.numeric(data$cond_overall)
summary(data)

#####################################################################################################
##### Is payment significantly different based on various factors?
# I'll run a series of t-tests to assess differences in the communities

#Were black homes condemned more often? 
condemn_t_test <- t.test(condemned ~ linnentown, data=data)
condemn_t_test #not significantly so, but yes. 

#Was the ultimate price paid significantly different?
price_t_test <- t.test(ULT_PRICE ~ linnentown, data=data)
price_t_test #yes. Linnentown sites had statistically significantly lower acquisition prices. 

#Was the condition of linnentown homes the same as NOB homes? 
cond_t_test <- t.test(cond_overall ~ linnentown, data=data)
cond_t_test #yes, Linnentown sites were assessed to be in statistically significantly worse condition than NOB parcels. Note this data could be biased. 

#Was the parcel size different?
size_t_test <- t.test(sq_ft ~ linnentown, data=data)
size_t_test #yes, Linnentown sites were statistically significantly smaller than NOB parcels

#Was the room number different?
rooms_t_test <- t.test(rooms ~ linnentown, data=data)
rooms_t_test #yes, Linnentown sites had statistically significantly less rooms than NOB parcels

#Was price-per-sq_ft different?
data$price_sq_ft <- data$ULT_PRICE / data$sq_ft
ppsqft_t_test <- t.test(price_sq_ft ~ linnentown, data=data)
ppsqft_t_test #yes, the price per square foot was statistically significantly lower for Linnetown parcels than NOB parcels. 

#Was price-per-room different?
data$price_room <- data$ULT_PRICE / data$rooms
data2 <- data
is.na(data2) <- sapply(data2, is.infinite) #this accounts for vacant lots (price / 0 rooms = Inf)
ppr_t_test <- t.test(price_room ~ linnentown, data=data2)
ppr_t_test #the price paid per room is not significantly different

#####################################################################################################
##### What is the problem with t-tests?
# It's hard to know how these factors collectively affected payment. Moreover, what is the role of race?
# Although underpowered due to data quality concerns (the prescence of NAs) and limited data extent (a small-N), I beleive regression may reveal some effects of interest. Importantly, however, parameters must be kept to a minimum, with at least ~10 data points for every model parameter.

#basic regrresions
OLS_all <- lm(ULT_PRICE ~ sq_ft + rooms + linnentown + cond_overall, data = data) #too few observations (40 for 5 vars)

OLS_sq_ft <- lm(ULT_PRICE ~ sq_ft + linnentown + cond_overall, data = data) #sufficient observations (42 for 4 variables)

OLS_rooms <- lm(ULT_PRICE ~ rooms + linnentown + cond_overall, data = data) #sufficient observations (40 for 4 variables)

OLS_no_condition <- lm(ULT_PRICE ~ sq_ft + rooms + linnentown, data = data) #sufficient observations (45 for 4 variables)

summary(OLS_all) #ignore, data is too weak
summary(OLS_sq_ft) #sq_ft, Linnentown have strong effect. Condition has weak, stat significant effect.
summary(OLS_rooms) #rooms, Linnentown have strong effect. Condition has weak, insignificant effect.
summary(OLS_no_condition) #sq_ft, rooms have strong effect. Linnentown have some stat-significant effect.

#Let's assume I go with that last one, selecting it as a result of its inclusion of the variables that are most commonly found to be coorelated. More scientific methods of model selection (e.g., AIC or an Anova) is impossible due to use of such a narrow dataset that adds/drops points based on what regression is used (a result of varying NAs and my unwillingness to lose datapoints through a dataset-wide na.omit()). Notably, however, when I did try running the data after using na.omit() on the dataset -- reducing the dataset to a mere 33 data points -- the AIC and Anova suggested I also use the fourth model.

##What's the predicted value of a median home/parcel in both neighborhoods? (Assuming the 4th model)
#Across parcel size, with median rooms
fake_data_white <- data.frame(sq_ft = seq(2500, 55000, by=500), rooms = median(na.omit(data$rooms)), linnentown = 0)
fake_data_black <- data.frame(sq_ft = seq(2500, 55000, by=500), rooms = median(na.omit(data$rooms)), linnentown = 1)

white <- predict(OLS_no_condition, newdata=fake_data_white, interval="confidence")
black <- predict(OLS_no_condition, newdata=fake_data_black, interval="confidence")
white <- data.frame(white)
black <- data.frame(black)
x_vals <- data.frame(seq(2500, 55000, by=500))

plot(1, type="n", xlim = c(2500,55000), ylim = c(0,30000), ylab="Predicted Acquisition Value", xlab="Parcel Size")
polygon(x=c(x_vals[1:106,], rev(x_vals[1:106,])), y=c(white$upr[1:106], rev(white$lwr[1:106])), col=rgb(1,0,0,0.5), border=NA)
polygon(x=c(x_vals[1:106,], rev(x_vals[1:106,])), y=c(black$upr[1:106], rev(black$lwr[1:106])), col=rgb(0,1,0,0.5), border=NA)
lines(x_vals[1:106,], white$fit, lty=1)
lines(x_vals[1:106,], black$fit, lty=2)

#Across roooms, with median parcel size
fake_data_white2 <- data.frame(sq_ft = median(na.omit(data$sq_ft)), rooms = seq(0, 17, by=1), linnentown = 0)
fake_data_black2 <- data.frame(sq_ft = median(na.omit(data$sq_ft)), rooms = seq(0, 17, by=1), linnentown = 1)

white2 <- predict(OLS_no_condition, newdata=fake_data_white2, interval="confidence")
black2 <- predict(OLS_no_condition, newdata=fake_data_black2, interval="confidence")
white2 <- data.frame(white2)
black2 <- data.frame(black2)
x_vals2 <- data.frame(seq(0, 17, by=1))

plot(1, type="n", xlim = c(0,17), ylim = c(0,30000), ylab="Predicted Acquisition Value", xlab="Room Number")
polygon(x=c(x_vals2[1:17,], rev(x_vals2[1:17,])), y=c(white2$upr[1:17], rev(white2$lwr[1:17])), col=rgb(1,0,0,0.5), border=NA)
polygon(x=c(x_vals2[1:17,], rev(x_vals2[1:17,])), y=c(black2$upr[1:17], rev(black2$lwr[1:17])), col=rgb(0,1,0,0.5), border=NA)
lines(x_vals2[1:17,], white2$fit, lty=1)
lines(x_vals2[1:17,], black2$fit, lty=2)

##Barplot, for median residential property
y <- c(9981, 7060)
y_1 <- c(NA, 7060)
y_2 <- c(9981, NA)
bp <- barplot(y, ylim=c(0, 11000), main="Compensation for Median Residential Property", xlab="Race", ylab = "Predicted Compensation", names.arg = c("White Owner", "African-American Owner"), axes = TRUE, width=.5, xlim=c(0,1.27), col=rgb(.8,0,0,.8), axisnames = TRUE, axis.lty = 1, font.lab=2, family="mono")
text(bp, 7600, round(y_1, 1), cex=1, pos=1, family="mono")
text(bp, 10521, round(y_2, 1), cex=1, pos=1, family="mono")
box()
