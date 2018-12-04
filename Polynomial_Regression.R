#--------------------------------------------------------------------------------------------------
#                         Experimenting with Polynomial Regression                -RUHI GHOSH
#--------------------------------------------------------------------------------------------------             

#--------------------------------------------------------------------------------------------------
# 1) Fitting Polynomial Regression of order 7 for Sample Size range: 10,20,30,40,50,70,100,200,350
#--------------------------------------------------------------------------------------------------

# Loading Cars Data into a dataframe
carsdata=read.csv("C:/Users/HP smart pc/Downloads/Carsdata.csv")
# Setting location to save the graphs
setwd("G:/Analytics_Praxis/R/R-Codes/PolyGraphs")

# Seting seed value so that sample size doesnot change
set.seed(0)

# Selecting train samples of size 350
rand = sample(1:nrow(carsdata),350)
maintrain = carsdata[rand, ]
testdemo=carsdata[-rand,]
nrow(testdemo)

# Selecting test samples of size 50
rand1 = sample(1:nrow(testdemo),50)
maintest = testdemo[rand1, ]

#checking row count for train and test samples
nrow(maintrain)
nrow(maintest)

#creating two new list to store error values
Testerrorlist=list()
Trainerrorlist=list()

# creating a for loop for diffrent sample sizes
for (i in c(10,20,30,40,50,70,100,200,350)){
  
  rand2 = sample(1:nrow(maintrain),i)
  trainsample1=maintrain[rand2,]
  # Fitting the polynomial regression line of degree 7
  modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample1)
  print(modeldegree7)
  
  # Plotting and saving the graphs
  #jpeg(file=paste(i,'.jpeg',sep=" "))
  plot(trainsample1$Weight,trainsample1$MPG, ylab = "Miles Per Gallon", xlab = "Weight", pch=19, cex=0.5)
  title(paste (main="Polynomial Regression Plot of order 7 for sample size: ",i,sep=""))
  lines(sort(trainsample1$Weight), fitted(modeldegree7)[order(trainsample1$Weight)], col='blue', type='l')
  #dev.off()
  
  # Train Test Accuracy
  TrainError=c(sum(modeldegree7$residuals^2))
  pred = predict(modeldegree7, newdata=maintest)
  TestError=sum((pred-maintest$MPG)^2)
  Testerrorlist=c(Testerrorlist,TestError)
  Trainerrorlist=c(Trainerrorlist,TrainError)
} 

# Plotting and Saving the graphs of sample size of 20,30,40,50,70,100,300
samplesize=c(10,20,30,40,50,70,100,200,350)
jpeg("TestError vs SampleSize for Order 7NEW.jpeg")
plot(samplesize,Testerrorlist,ylab="Test Error", xlab="Sample Size",col='blue',type='l')
title(main="Test Error vs Sample Size Plot")
dev.off()

#****************************************************************************************************
# 2)  Fitting Regression Line of order 1,2,7,8,9,10 on 4 diffrent random sample of size: 20
#****************************************************************************************************

#----------------------------------------------------------------------------------------------------
# First random sample of size 20 from the main traing data set
#----------------------------------------------------------------------------------------------------
set.seed(7)
rand2 = sample(1:nrow(maintrain),20)
trainsample1=maintrain[rand2,]
nrow(trainsample1)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample1)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample1)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample1)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample1)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample1)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample1)

# Plotting and saving the graphs
jpeg("FirstSamplesize20_AllorderPolyLATESTNEW.jpeg")
plot(sort(trainsample1$Weight), fitted(modeldegree1)[order(trainsample1$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for first random sample size: 20 for diff degree")
lines(sort(trainsample1$Weight), fitted(modeldegree2)[order(trainsample1$Weight)], col='green', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree7)[order(trainsample1$Weight)], col='black', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree8)[order(trainsample1$Weight)], col='orange', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree9)[order(trainsample1$Weight)], col='magenta', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree10)[order(trainsample1$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()

# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist1=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample1)
  pred = predict(newmodel, newdata=maintest)
  RMSE=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist1=c(Testerrorlist1,RMSE)
}

print(Testerrorlist1)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE vs Model Complexity for first random sample size=20.jpeg")
plot(ModelComplexity,Testerrorlist1,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for first random sample size=20")
dev.off()

#----------------------------------------------------------------------------------------------------
# Second random sample of size 20 from the main traing data set
#----------------------------------------------------------------------------------------------------
set.seed(13)
rand3 = sample(1:nrow(maintrain),20)
trainsample2=maintrain[rand3,]
nrow(trainsample2)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample2)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample2)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample2)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample2)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample2)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample2)

# Plotting and saving the graphs
jpeg("SecondSamplesize20_AllorderPolyLATESTNEW1.jpeg")
plot(sort(trainsample2$Weight), fitted(modeldegree1)[order(trainsample2$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for second random sample size:20 for diff degree")
lines(sort(trainsample2$Weight), fitted(modeldegree2)[order(trainsample2$Weight)], col='green', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree7)[order(trainsample2$Weight)], col='black', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree8)[order(trainsample2$Weight)], col='orange', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree9)[order(trainsample2$Weight)], col='magenta', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree10)[order(trainsample2$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()

# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist2=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample2)
  pred = predict(newmodel, newdata=maintest)
  RMSE2=sqrt(sum((pred-maintest$MPG)^2))
  print(RMSE2)
  Testerrorlist2=c(Testerrorlist2,RMSE2)
}

print(Testerrorlist2)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE vs Model Complexity for second random sample size=20.jpeg")
plot(ModelComplexity,Testerrorlist2,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for second random sample size=20")
dev.off()



#-----------------------------------------------------------------------------------------------------
# Third random sample of size 20 from the main traing data set
#-----------------------------------------------------------------------------------------------------
set.seed(30)
rand4 = sample(1:nrow(maintrain),20)
trainsample3=maintrain[rand4,]
nrow(trainsample3)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample3)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample3)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample3)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample3)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample3)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample3)

# Plotting and saving the graphs
jpeg("ThirdSample20AllorderPolyLATESTNEW.jpeg")
plot(sort(trainsample3$Weight), fitted(modeldegree1)[order(trainsample3$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for third random sample size: 20 for diff degree")
lines(sort(trainsample3$Weight), fitted(modeldegree2)[order(trainsample3$Weight)], col='green', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree7)[order(trainsample3$Weight)], col='blue', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree8)[order(trainsample3$Weight)], col='orange', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree9)[order(trainsample3$Weight)], col='magenta', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree10)[order(trainsample3$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()

# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist3=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample3)
  pred = predict(newmodel, newdata=maintest)
  RMSE3=sqrt(sum((pred-maintest$MPG)^2))
  print(RMSE3)
  Testerrorlist3=c(Testerrorlist3,RMSE3)
}

print(Testerrorlist3)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE vs Model Complexity for third random sample size=20.jpeg")
plot(ModelComplexity,Testerrorlist3,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for third random sample size=20")
dev.off()



#-----------------------------------------------------------------------------------------------------
# Fourth random sample of size 20 from the main traing data set
#-----------------------------------------------------------------------------------------------------
set.seed(15)
rand5 = sample(1:nrow(maintrain),20)
trainsample4=maintrain[rand5,]
nrow(trainsample4)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample4)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample4)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample4)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample4)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample4)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample4)

# Plotting and saving the graphs
jpeg("FourthSample20AllorderPolyLATESTNEW.jpeg")
plot(sort(trainsample4$Weight), fitted(modeldegree1)[order(trainsample4$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for fourth random sample size: 20 for diff degree")
lines(sort(trainsample4$Weight), fitted(modeldegree2)[order(trainsample4$Weight)], col='green', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree7)[order(trainsample4$Weight)], col='black', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree8)[order(trainsample4$Weight)], col='orange', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree9)[order(trainsample4$Weight)], col='magenta', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree10)[order(trainsample4$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()

# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist4=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample4)
  pred = predict(newmodel, newdata=maintest)
  RMSE4=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist4=c(Testerrorlist4,RMSE4)
}

print(Testerrorlist4)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE vs Model Complexity for fourth random sample size=20.jpeg")
plot(ModelComplexity,Testerrorlist4,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for fourth random sample size=20")
dev.off()

#--------------------------------------------------------------------------------------------------
# Plotting and saving all the RMSE of four diffrent sample against model complexity in one graph
#--------------------------------------------------------------------------------------------------

ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE for all samples vs Model Complexity for first random sample size=20.jpeg")
plot(ModelComplexity,Testerrorlist1, col='blue',type='l',ylab="RMSE for all samples",xlab='Model Complexity')
title(main="RMSE vs Model Complexity for four random sample size=20")
lines(ModelComplexity,Testerrorlist2, col='magenta',type='l')
lines(ModelComplexity,Testerrorlist3, col='green',type='l')
lines(ModelComplexity,Testerrorlist4, col='orange',type='l')
legend("topleft", legend=c("RMSE Sample1", "RMSE Sample2","RMSE Sample3","RMSE Sample4"),
       col=c("blue",'magenta','green','orange'), lty=1:2, cex=0.8)
dev.off()

#****************************************************************************************************
# 3)  Fitting Regression Line of order 1,2,7,8,9,10 on 4 diffrent random sample of size: 100
#****************************************************************************************************

#-----------------------------------------------------------------------------------------------------
# First random sample of size 100 from the main traing data set
#-----------------------------------------------------------------------------------------------------

set.seed(10)
rand2 = sample(1:nrow(maintrain),100)
trainsample1=maintrain[rand2,]
nrow(trainsample1)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample1)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample1)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample1)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample1)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample1)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample1)

# Plotting and saving the graphs
jpeg("FirstSample100AllorderPolyLATEST.jpeg")
plot(sort(trainsample1$Weight), fitted(modeldegree1)[order(trainsample1$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for first random sample size: 100 for diff degree")
lines(sort(trainsample1$Weight), fitted(modeldegree2)[order(trainsample1$Weight)], col='green', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree7)[order(trainsample1$Weight)], col='black', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree8)[order(trainsample1$Weight)], col='orange', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree9)[order(trainsample1$Weight)], col='magenta', type='l')
lines(sort(trainsample1$Weight), fitted(modeldegree10)[order(trainsample1$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()


# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist1=list()
for (k in c(1,2,7,8,9,10)){
  newm = lm(MPG ~ poly(Weight,k), trainsample1)
  pred = predict(newm, newdata=maintest)
  RMSE1=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist1=c(Testerrorlist1,RMSE1)
}

print(Testerrorlist1)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE vs Model Complexity for First random sample size=100.jpeg")
plot(ModelComplexity,Testerrorlist1,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for First random sample size=100")
dev.off()

#--------------------------------------------------------------------------------------------------------
# Second random sample of size 100 from the main traing data set
#--------------------------------------------------------------------------------------------------------

set.seed(6)
rand2 = sample(1:nrow(maintrain),100)
trainsample2=maintrain[rand2,]
nrow(trainsample2)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample2)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample2)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample2)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample2)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample2)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample2)

# Plotting and saving the graphs
jpeg("secondSample100AllorderPolyLATEST.jpeg")
plot(sort(trainsample2$Weight), fitted(modeldegree1)[order(trainsample2$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for second random sample size: 100 for diff degree")
lines(sort(trainsample2$Weight), fitted(modeldegree2)[order(trainsample2$Weight)], col='green', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree7)[order(trainsample2$Weight)], col='black', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree8)[order(trainsample2$Weight)], col='orange', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree9)[order(trainsample2$Weight)], col='magenta', type='l')
lines(sort(trainsample2$Weight), fitted(modeldegree10)[order(trainsample2$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()

# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist2=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample2)
  pred = predict(newmodel, newdata=maintest)
  RMSE2=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist2=c(Testerrorlist2,RMSE2)
}

print(Testerrorlist2)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE vs Model Complexity for Second random sample size=100.jpeg")
plot(ModelComplexity,Testerrorlist2,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for Second random sample size=100")
dev.off()


#-------------------------------------------------------------------------------------------------------
# Third random sample of size 100 from the main traing data set
#-------------------------------------------------------------------------------------------------------

set.seed(40)
rand3 = sample(1:nrow(maintrain),100)
trainsample3=maintrain[rand3,]
nrow(trainsample3)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample3)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample3)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample3)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample3)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample3)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample3)

# Plotting and saving the graphs
jpeg("ThirdSample100AllorderPolyLATEST.jpeg")
plot(sort(trainsample3$Weight), fitted(modeldegree1)[order(trainsample3$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for third random sample size: 100 for diff degree")
lines(sort(trainsample3$Weight), fitted(modeldegree2)[order(trainsample3$Weight)], col='green', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree7)[order(trainsample3$Weight)], col='black', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree8)[order(trainsample3$Weight)], col='orange', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree9)[order(trainsample3$Weight)], col='magenta', type='l')
lines(sort(trainsample3$Weight), fitted(modeldegree10)[order(trainsample3$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()


# Plotting and saving RMSE(test error) Vs Model Complexity

Testerrorlist3=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample3)
  pred = predict(newmodel, newdata=maintest)
  RMSE3=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist3=c(Testerrorlist3,RMSE3)
}
print(Testerrorlist3)
ModelComplexity=c(1,2,7,8,9)
jpeg("RMSE vs Model Complexity for Third random sample size=100.jpeg")
plot(ModelComplexity,Testerrorlist3,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for Third random sample size=100")
dev.off()

#-----------------------------------------------------------------------------------------------------
# Fourth random sample of size 100 from the main traing data set
#-----------------------------------------------------------------------------------------------------
set.seed(40)
rand4= sample(1:nrow(maintrain),100)
trainsample4=maintrain[rand4,]
nrow(trainsample4)

# Fitting the polynomial regression model of degree 1,2,7,8,9
modeldegree1 = lm(MPG ~ poly(Weight,1), trainsample4)
modeldegree2 = lm(MPG ~ poly(Weight,2), trainsample4)
modeldegree7 = lm(MPG ~ poly(Weight,7), trainsample4)
modeldegree8 = lm(MPG ~ poly(Weight,8), trainsample4)
modeldegree9 = lm(MPG ~ poly(Weight,9), trainsample4)
modeldegree10 = lm(MPG ~ poly(Weight,10), trainsample4)

# Plotting and saving the graphs
jpeg("FourthSample100AllorderPolyLATEST.jpeg")
plot(sort(trainsample4$Weight), fitted(modeldegree1)[order(trainsample4$Weight)],ylab = "Miles Per Gallon", xlab = "Weight", col='blue', type='l')
title(main = "Regression Plot for Fourth random sample size: 100 for diff degree")
lines(sort(trainsample4$Weight), fitted(modeldegree2)[order(trainsample4$Weight)], col='green', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree7)[order(trainsample4$Weight)], col='black', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree8)[order(trainsample4$Weight)], col='orange', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree9)[order(trainsample4$Weight)], col='magenta', type='l')
lines(sort(trainsample4$Weight), fitted(modeldegree10)[order(trainsample4$Weight)], col='red', type='l')
legend("topright", legend=c("Degree 1", "Degree 2","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("blue", "green",'black','orange','magenta','red'), lty=1:2, cex=0.8)
dev.off()

# Plotting and saving RMSE(test error) Vs Model Complexity
Testerrorlist4=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample4)
  pred = predict(newmodel, newdata=maintest)
  RMSE4=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist4=c(Testerrorlist4,RMSE4)
}

print(Testerrorlist4)
ModelComplexity=c(1,2,7,8,9,10)
typeof(ModelComplexity)
jpeg("RMSE vs Model Complexity for Fourth random sample size=100.jpeg")
plot(ModelComplexity,Testerrorlist4,ylab="RMSE", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE vs Model Complexity for Fourth random sample size=100")
dev.off()


#*************************************************************************************************
# 4)  Ploting and saving RMSE of Test  and Train  Vs Model Complexity for sample size 20 & 100
#     of polynomial regression of order 1,2,7,8,9
#**************************************************************************************************

#---------------------------------------------------------------------------------------------------
#  Random sample of size 20 from the main traing data set
#---------------------------------------------------------------------------------------------------

#Taking two new empty list
Testerrorlist1=list()
Trainerrorlist1=list()

#Plotting for sample size 20
set.seed(40)
rand2 = sample(1:nrow(maintrain),20)
trainsample1=maintrain[rand2,]
nrow(trainsample1)

for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample1)
  print(newmodel)
  TrainError=sqrt(sum(newmodel$residuals^2))
  pred = predict(newmodel, newdata=maintest)
  Testerror1=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist1=c(Testerrorlist1,Testerror1)
  Trainerrorlist1=c(Trainerrorlist1,TrainError)
}

print(Testerrorlist1)
print(Trainerrorlist1)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE(Test & Train Error) vs ModelComplexity samplesize:20.jpeg")
plot(ModelComplexity,Trainerrorlist1,ylab="TestError and TrainError", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE(Test & Train Error) vs ModelComplexity samplesize:20")
lines(ModelComplexity,Testerrorlist1,col='red',type='l' )
legend("topleft", legend=c("Train Error", "Test Error"),
 col=c("red", "blue"), lty=1:2, cex=0.8)
dev.off()

#---------------------------------------------------------------------------------------------------------
#  Random sample of size 100 from the main traing data set
#---------------------------------------------------------------------------------------------------------
set.seed(10)
rand2 = sample(1:nrow(maintrain),100)
trainsample1=maintrain[rand2,]
nrow(trainsample1)

# Plotting and Saving RSS vs Model Complexity 
Testerrorlist1=list()
Trainerrorlist1=list()
for (k in c(1,2,7,8,9,10)){
  newmodel = lm(MPG ~ poly(Weight,k), trainsample1)
  TrainError=sqrt(sum(newmodel$residuals^2))
  pred = predict(newmodel, newdata=maintest)
  Testerror1=sqrt(sum((pred-maintest$MPG)^2))
  Testerrorlist1=c(Testerrorlist1,Testerror1)
  Trainerrorlist1=c(Trainerrorlist1,TrainError)
}

print(Testerrorlist1)
print(Trainerrorlist1)
ModelComplexity=c(1,2,7,8,9,10)
jpeg("RMSE(Test & Train Error) vs ModelComplexity samplesize:100.jpeg")
plot(ModelComplexity,Testerrorlist1,ylab="TestError and TrainError", xlab="Model Complexity",col='blue',type='l')
title(main="RMSE(Test & Train Error) vs ModelComplexity samplesize:100")
lines(ModelComplexity,Trainerrorlist1,col='red',type='l' )
legend("topleft", legend=c("Train Error", "Test Error"),
       col=c("red", "blue"), lty=1,cex=0.8)
dev.off()

#----------------------------------------END--------------------------------------------------------------