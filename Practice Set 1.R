
install.packages(c("ellipse", "leaps", "robust"), repos="http://cran.ch.r-project.org")

update.packages(c("ellipse", "leaps", "robust"),repos="http://cran.ch.r-project.org")

library(robust)
library(leaps)
library(ellipse)

path = "C:\\Users\\emssc\\R"
path

weights = read.table(paste(path, "\\weights.txt", sep=""), header=TRUE)
weights
GrandConseil = read.csv(paste(path, "\\GrandConseil.csv", sep=""), row.names=1)
GrandConseil


head(weights)
dim(weights)

boxplot(weights)

fivenum(weights$H) #contains the five values that allow you to build a boxplot
fivenum(weights$F, na.rm=TRUE) #The option na.rm=TRUE deletes the missing values, denoted by NA (`Non Available')

# To show the label of the observations and of the variables of the data
GrandConseil
dimnames(GrandConseil)

# To show only the label of the observations or of the variables
dimnames(GrandConseil)[[1]]
rownames(GrandConseil)

dimnames(GrandConseil)[[2]]
colnames(GrandConseil)

# If you want to show only a certain value, or variable of the data
GrandConseil[1, 6]
GrandConseil["PS", "Siege.01"]

GrandConseil[, 2]
GrandConseil[, "Siege.85"]

GrandConseil[3, ]
GrandConseil["PRD", ]

# It is possible to `attach' the data with the command attach such that the variables (columns) are directly
# accessible. To `detach' the object, use detach and the columns are not available anymore. Try the
# following commands
Siege.97
search()
attach(GrandConseil)
search()
GrandConseil$siege.97
Siege.97
detach("GrandConseil")
search()

# Important: if a column has the same name as an object already existing in R, it will not be attached. It
# is necessary to delete or rename the object having the same name. As an example, if there is an object
# called Siege.01, you should type
Siege.01 = Siege.01
rm(Siege.01)

# If we want to know the parties that had more than 15 seats in 2001, type
GrandConseil[,"Siege.01"] > 15

# To show how many parties had more than 15 seats in 2001
sum(GrandConseil[, "Siege.01"] > 15)

sum(GrandConseil[, "Siege.01"])
GrandConseil["UDC", "Siege.01"] - GrandConseil["UDC", "Siege.97"]
GrandConseil["PL", "Siege.89"] - GrandConseil["PL", "Siege.85"]
max(GrandConseil)
sum(GrandConseil[, "Siege.01"] == GrandConseil[, "Siege.97"])

