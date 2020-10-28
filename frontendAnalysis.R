#IST 387 Final
#19 December 2018
#This is my code with help from the course textbook


#load dataset from blackboard using read.csv
#print summary statistics in order to have an actual idea of the missing values

library('purrr')
library('tidyr')
library('ggplot2')
library('ggmap')

flights <- read.csv("/Users/kenobrien/Desktop/repos/IST387FinalProject/kwobrien.csv")
summary(flights)
View(flights)


#-------Phase 1-------#
#check the sum of missing values for each column of the data set
colSums(is.na(flights))


#columns 'flight time in minutes[44]' & 'arrival delay in minutes'[44] & 'departure delay in minutes'[38]
#use mean substitution for numeric variables (both columns are numeric)

#create new dataframe to house new columns
#get mean of columns with missing  values (for best practice)
#assign column mean for each column which has NA values
#check to make sure that the means are correct (for best practice)
cleanFlights <- flights

#Departure.Delay.Mean <- mean(cleanFlights$Departure.Delay.in.Minutes)
#Arrival.Delay.Mean <- mean(cleanFlights$Arrival.Delay.in.Minutes)
#Flight.time <- mean(cleanFlights$Flight.time.in.minutes)

cleanFlights$Departure.Delay.in.Minutes[is.na(cleanFlights$Departure.Delay.in.Minutes)] <- mean(cleanFlights$Departure.Delay.in.Minutes, na.rm = TRUE)
cleanFlights$Arrival.Delay.in.Minutes[is.na(cleanFlights$Arrival.Delay.in.Minutes)] <- mean(cleanFlights$Arrival.Delay.in.Minutes, na.rm = TRUE)
cleanFlights$Flight.time.in.minutes[is.na(cleanFlights$Flight.time.in.minutes)] <- mean(cleanFlights$Flight.time.in.minutes, na.rm = TRUE)

#check to make sure the values are replaced
colSums(is.na(cleanFlights))
View(cleanFlights)

#-----Phase 2-----#

#get names of all numeric columns
names(cleanFlights[,numerics])

#plot all numeric columns in histograms--->
#use purrr keep to select all numeric variables
#facet_wrap to plot each column without the need of a loop
#create key and value with gather()
#plot in histogram

cleanFlights %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()

#Age = Symmetric
#Arrival Delay in Minutes = Negative Skewed
#Day of month = Symmetric
#Departure Delay in Minutes = Negative Skewed
#dlat - not significant
#dlong - not significant
#Eating and Drinking at Airport 
#flight distance = Negatively Skewed
#flight time in minutes = Negative Skewed
#Number of other loyalty cards  = Negative Skewed
#No of flights per airline = Negative Skewed
#olat - not significant 
#olong - not significant
#Price Sensitivity = Negative Skewed
#Satisfaction = Positively Skewed
#Scheduled Departure Hour = Positively Skewed
#Shopping amount at Airport = Negatively skewed
#Numer of Flights with other airlines = Negative Skewed
#Years of First Flight = Symmetric

#Create new dataframe that is only factor columns from cleanFlights
#apply filter function that creates tables for factor columns
factor_df <- Filter(is.factor, cleanFlights)
View(factor_df)

lapply(factor_df, function(x) {
  if(is.factor(x)) return(table(x))
})

#-----Phase 3-----#
str(cleanFlights) #see the variable type of each column

#create new dataframe only for applicable variables for satisfaction
#dfSatisfaction <- cleanFlights[,c("Satisfaction", "Airline.Status", "Age", "Gender", 
#                                  "Price.Sensitivity", "Year.of.First.Flight", "No.of.Flights.p.a.", "Type.of.Travel", "Shopping.Amount.at.Airport", "Class", "Scheduled.Departure.Hour", "Flight.cancelled", "Arrival.Delay.greater.5.Mins")]
#View(dfSatisfaction) #make sure it worked
#str(dfSatisfaction)
#create linear model with all applicable variables for customer satisfaction
lmSatisfaction <- lm(lm(formula = Satisfaction ~ Airline.Status + Age + Gender + 
                          Price.Sensitivity + Year.of.First.Flight + No.of.Flights.p.a. + X..of.Flight.with.other.Airlines +
                          Type.of.Travel + No..of.other.Loyalty.Cards + Shopping.Amount.at.Airport + 
                          Eating.and.Drinking.at.Airport + as.factor(Class) + Day.of.Month + Flight.date + Airline.Code + Airline.Name +
                          Orgin.City + Origin.State + Destination.City + Destination.State + Scheduled.Departure.Hour + 
                          Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + as.numeric(Flight.cancelled) + 
                          Flight.time.in.minutes + Flight.Distance + Arrival.Delay.greater.5.Mins, 
                        data = cleanFlights))
options(max.print = 99999999) #increase max print to see all significance variables
summary(lmSatisfaction) 

#Not Applicable to Satisfaction = OriginCity, OriginState, DestinationCity, DestinationState, FlightDate, AirlineName
#Not Statisitfcally Significant for Satisfaction = XofFlightsWithOtherAirlines, No..of.loyalty.cards, eating.and.drinkings.at.airport
#day.of.month, airline.code, departure.delay.in.minutes, arrival.delay.in.minutes, flight.time.in.minutes, flight.distance

#Columns that were significant (* or more) = Airline.Status + Age, + Gender + Price.Sensitivty + No.of.Flights.p.a. +
#Type.of.Travel + Shopping.Amount.at.Airport + Arrival Delay Greater Than 5 mins


trimmedLM <- summary(lm(formula = Satisfaction ~ Airline.Status + Age + Gender + 
                           Price.Sensitivity + No.of.Flights.p.a. + Type.of.Travel  + 
                           Shopping.Amount.at.Airport + Arrival.Delay.greater.5.Mins, data = cleanFlights))
trimmedLM

#-----Phase 4-----#
#create subset of data for only those with the lowest satisfaction 2 or lower
#5 = highest, 4 = satisfied, 3=neutral, 2=unsatisfied, 1=not satisfied at all
dfLowSatisfaction <- subset(cleanFlights, Satisfaction <= 2)
dfRoutes <- dfLowSatisfaction[c(-4:-28)] #remove unneeded columns
View(dfRoutes)
#create 2 dataframes for map..one with satisfaction of 1 and another with s atisfaction of 2
dfRoutes1 <-subset(dfRoutes, Satisfaction == 1)
dfRoutes2 <-subset(dfRoutes, Satisfaction == 2)
dfRoutes5 <-subset(cleanFlights, Satisfaction == 5)

View(dfRoutes1) #view it to make sure the command worked properly - it did
View(dfRoutes2)

#plot flight curves on us map using ggplot geom_curve

#generate map of US
#plot only level 1 satisfaction
#plot only level 2 satisfaction
#map of both 1 + 2 satisfaction levels
usMap <- borders("state", colour = "black", fill = "white")

#level 1 satisfaction
routeMap1 <- ggplot() + usMap +
  geom_curve(data = dfRoutes1, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "red", size = .3, curvature = .5, show.legend = TRUE) +
  ggtitle(("Route Map of Level 1 Satisfaction")) + geom_point(data = dfRoutes1, aes(x = olong, y = olat, col = "red")) + geom_point(data = dfRoutes1, aes(x = dlong, y = dlat, col = "blue")) + 
  geom_text(data = dfRoutes1, aes(x = olong, y = olat, label = dfRoutes1$Orgin.City), col = "black", size = 2)
routeMap1

#level 2 sasifaction
routeMap2 <- ggplot() + usMap +
  geom_curve(data = dfRoutes2, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "blue", size = .3, curvature = .5, show.legend = TRUE) +
  ggtitle(("Route Map of Level 2 Satisfaction")) + geom_point(data = dfRoutes2, aes(x = olong, y = olat, col = "red")) + geom_point(data = dfRoutes2, aes(x = dlong, y = dlat, col = "blue")) +
  geom_text(data = dfRoutes2, aes(x = olong, y = olat, label = dfRoutes2$Orgin.City), col = "black", size = 2)
routeMap2

#level 5 satisfaction just for visualization purposes/ comparison purposes
routeMap5 <- ggplot() + usMap +
  geom_curve(data = dfRoutes5, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "red", size = .3, curvature = .5, show.legend = TRUE) +
  ggtitle(("Route Map of Level 5 Satisfaction")) + geom_point(data = dfRoutes5, aes(x = olong, y = olat, col = "red")) + geom_point(data = dfRoutes5, aes(x = dlong, y = dlat, col = "blue")) + 
  geom_text(data = dfRoutes5, aes(x = olong, y = olat, label = dfRoutes5$Orgin.City), col = "black", size = 2)
routeMap5



#both routes mapped together
routeMapCombined <- ggplot() + usMap +
  geom_curve(data = dfRoutes1, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "red", size = .3, curvature = .5, show.legend = TRUE) +
  geom_curve(data = dfRoutes2, aes(x = olong, y = olat, xend = dlong, yend = dlat), col = "blue", size = .1, curvature = .5, show.legend = TRUE) + 
  ggtitle("Route Map of Low Satisfaction") + geom_point(data = dfRoutes, aes(x = olong, y = olat, col = "red")) + geom_point(data = dfRoutes, aes(x = dlong, y = dlat, col = "blue")) +
  geom_text(data = dfRoutes, aes(x = dlong, y = dlat, label = dfRoutes$Destination.City), col = "black", size = 2)
routeMapCombined  
