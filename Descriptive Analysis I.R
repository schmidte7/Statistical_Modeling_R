setwd("C:\\Users\\emssc\\R") #set the folder containing dataset as your working directory
getwd()

AgeSal = read.csv("AgeSal.csv")
Haies110 = read.csv("Haies110.csv")
MarkVal = read.csv("MarkVal.csv", row.names=1)
TVAds = read.csv("TVAds.csv", row.names=1)
Tracteurs = read.csv("Tracteurs.csv")
PretBanc = read.csv("PretBanc.csv")

# SCATTERPLOT
# The function plot() visualises the relationship between two variables with R
    # plot(x, y, main=..., xlim=..., ylim=..., cex=..., pch=...)

# MATRIX OF SCATTERPLOTS (scatterplot matrix)
# The function pairs() visualises the relationship between all the variables in a data set:
    # pairs(x, cex=..., pch=...)

# DETECTION & IDENTIFICATION OF OUTLIERS
# It is always interesting to be able to identify the points in the scatterplot in order to detect those
# observations that result far from the bulk of the data. For the scatterplot it is possible to identify
# these observations by adding a label to the points with the help of the function text():
    # text(x, y, labels, cex=...)

# EXAMPLE 
# Let us perform a descriptive analysis for the data set TVAds. In what follows we check the relationship
# between the variables Spend and Impress, and identify all the points.
attach(TVAds) # Attach the data set.
plot(Spend, Impress) # Or pairs(TVAds)
plot(Spend, Impress, pch=19) # Plot with different character for the points.

#And the identification of all the points on the scatterplot can be done using
text(Spend, Impress+2, rownames(TVAds), cex=0.5)
detach("TVAds") # Detach the data set.


# EXERCISES
# Produce all the scatterplots for all the data sets introduced at the beginning of these practicals. Give
# a first interpretation about the relationship between the variables and try to detect (and identify)
# eventual outliers.

attach(AgeSal)
AgeSal
plot(Age, Salaire)
plot(Age, Salaire, pch=3)

text(Age, Salaire, row.names(AgeSal), cex=0.5)
detach("AgeSal")
cor(Age, Salaire)
#######################################################

attach(Haies110)
Haies110
plot(Vent, Course)
plot(Vent, Course, pch=20)

text(Vent, Course, row.names(Haies110), cex=0.5)
detach("Haies110")
cor(Vent, Course)
summary(Haies110[2])
sd(Haies110$Course)
sd(Haies110$Vent)

#######################################################

attach(MarkVal)
MarkVal
plot(Employees, Sales)
plot(Employees, Sales, pch=10)

detach("MarkVal")
# cor(Employees, Sales) ???
summary(MarkVal[5])
sd(MarkVal$Employees)
sd(MarkVal$Sales)
mean(MarkVal$Sales)


#######################################################

attach(Tracteurs)
Tracteurs
plot(Cout, Age, pch=5)
help(plot)

detach("Tracteurs")
cor(Cout, Age)
summary(Tracteurs[2])
sd(Tracteurs$Cout)
sd(Tracteurs$Age)
mean(Tracteurs$Age)



#######################################################

attach(PretBanc)
PretBanc
plot(prets, ethnique, pch=5)
help(plot)

detach("PretBanc")
cor(prets, ethnique)
summary(PretBanc[2])
sd(PretBanc$prets)
sd(PretBanc$ethnique)
mean(PretBanc$ethnique)
