
data <- read.csv(file="acs_ny.csv", header=TRUE)

#Add target variable
data$Target[data$FamilyIncome >= 150000] <- 1
data$Target[data$FamilyIncome < 150000] <- 0
data$Target <- factor(data$Target, levels=c(0,1), labels=c("FALSE", "TRUE"))


library(mice)
md.pattern(data)

## Exploratory Data Analysis 

library(sm)
library(vcd)
attach(data)

plot(density(data$FamilyIncome), 
     main="Density plot of Family Income", 
     xlab="Family Income")
summary(data$FamilyIncome)
print(paste("Percentage of income higher than 150,000: " , round(nrow(data[data$Target == "TRUE",])/nrow(data)*100,digits=2)))

par(mfrow=c(2,2))
#Number of Bed Rooms
count <- table(Target, NumBedrooms)
barplot(count, xlab="Number of Bedrooms", ylab="Frequency",
        beside=TRUE, col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#Number of children
count <- table(Target, NumChildren)
barplot(count, xlab="Number of Children", ylab="Frequency",
        beside=TRUE,col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#Number of People
count <- table(Target, NumPeople)
barplot(count, xlab="Number of People", ylab="Frequency",
        beside=TRUE,col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#Number of Rooms
count <- table(Target, NumRooms)
barplot(count, xlab="Number of Rooms", ylab="Frequency",
        beside=TRUE,col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#Number of Vehicles
count <- table(Target, NumVehicles)
barplot(count, xlab="Number of Vehicles", ylab="Frequency",
        beside=TRUE,col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#Number of Workers
count <- table(Target, NumWorkers)
barplot(count, xlab="Number of Workers", ylab="Frequency",
        beside=TRUE,col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#YearBuilt
count <- table(Target, YearBuilt)
barplot(count, xlab="Year Built", ylab="Frequency",
        beside=TRUE,col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#HouseCosts
sm.density.compare(HouseCosts, Target, xlab="House Costs")
colfill <-c(2: (1+length(levels(Target))))
legend("topright", levels(Target), fill=colfill, cex=0.5)

#ElectricBill
sm.density.compare(ElectricBill, Target, xlab="Electric Bill")
colfill <-c(2: (1+length(levels(Target))))
legend("topright", levels(Target), fill=colfill, cex=0.5)

#HeatingFuel
count <- table(Target, HeatingFuel)
barplot(count, xlab="Heating Fuel", ylab="Frequency",
        beside=TRUE, col=1:2)
legend("topright", legend=rownames(count), fill=1:2, cex=0.5)

#Insurance
sm.density.compare(Insurance, Target, xlab="Insurance")
colfill <-c(2: (1+length(levels(Target))))
legend("topright", levels(Target), fill=colfill, cex=0.5)

#Acres
par(mfrow=c(1,1), cex=0.7)
plot(table(Target, Acres), main="Acres")

#Family Type
plot(table(Target, FamilyType), main="Family Type")

#NumUnits
plot(table(Target, NumUnits), main="Number of Units")

#OwnRent
plot(table(Target, OwnRent), main="Own or Rent")

#FoodStamp
plot(table(Target, FoodStamp), main="Food Stamp")

#Language
plot(table(Target, Language), main="Language")


fit.full <- glm(Target~Acres+FamilyType+NumBedrooms+NumChildren+NumPeople+NumRooms+NumUnits+NumVehicles+NumWorkers+OwnRent+YearBuilt+HouseCosts+ElectricBill+FoodStamp+HeatingFuel+Insurance+Language,data=data, family=binomial())
summary(fit.full)


fit.reduced <- glm(Target~FamilyType+NumBedrooms+NumPeople+NumRooms+NumUnits+NumVehicles+NumWorkers+OwnRent+HouseCosts+ElectricBill+FoodStamp+Insurance, data=data, family=binomial())
summary(fit.reduced)


options(scipen=10)
AIC <- c(fit.full$aic, fit.reduced$aic, (fit.reduced$aic-fit.full$aic)/fit.full$aic*100)
names <- c("AIC full var", "AIC reduced var", "Percentage of increase")
names(AIC) <- names
AIC

dev <- c(fit.full$deviance, fit.reduced$deviance, (fit.reduced$deviance-fit.full$deviance)/fit.full$deviance*100)
names <- c("Deviance Residual full var", "Deviance Resudial reduced var", "Percentage of increase")
names(dev) <- names
dev
options(scipen=0)

### Interpreting coefficient value

options(scipen=10)
coef_odds <- sort(exp(coef(fit.reduced)), decreasing=TRUE)

par(cex=0.7)

round(coef_odds, digits=2)

barplot(coef_odds, names.arg=names(coef_odds), las=2, main="Coefficient plot of odds")
options(scipen=0)

### Prediction

data$prob <- predict(fit.reduced, newdata=data, type="response")
write.csv(data, file="acs_ny_predict.csv", col.names=TRUE)


