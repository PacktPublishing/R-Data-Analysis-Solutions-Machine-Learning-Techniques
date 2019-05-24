##1.2

auto <- read.csv("auto-mpg.csv", header=TRUE, sep = ",")

names(auto)

#read.csv("<file name>", sep="\t", dec=",")

auto <- read.csv("auto-mpg-noheader.csv", header=FALSE)

head(auto,2)

auto <- read.csv("auto-mpg-noheader.csv")

head(auto,2)

auto <- read.csv("auto-mpg-noheader.csv",
                 header=FALSE, col.names =
                   c("No", "mpg", "cyl", "dis","hp",
                     "wt", "acc", "year", "car_name"))

head(auto,2)

auto <- read.csv("auto-mpg.csv", na.strings="")
#na.strings= "N/A" or na.strings = "NA"

auto <- read.csv("auto-mpg.csv",stringsAsFactors=FALSE)

dat <- read.csv("http://www.exploredata.net/ftp/WHO.csv")


##1.3


install.packages("XML")

library(XML)
library(RCurl)

url <- getURL("https://www.w3schools.com/xml/cd_catalog.xml")

xmldoc <- xmlParse(url)

rootNode <- xmlRoot(xmldoc)

rootNode[1]

data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))

cd.catalog <- data.frame(t(data),row.names=NULL)

cd.catalog[1:2,]

url <- getURL("https://en.wikipedia.org/wiki/World_population")

tables <- readHTMLTable(url)





world.pop <- tables[[6]]

world.pop

table <- readHTMLTable(url,which=6)

table

##1.4

install.packages("jsonlite")

library(jsonlite)

dat.1 <- fromJSON("students.json")
dat.2 <- fromJSON("student-courses.json")

url <- "https://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json"

jsonDoc <- fromJSON(url)

jsonDoc

dat <- jsonDoc$list$resources$resource$fields

dat[1:2,]

dat.1[1:3,]

dat.2[,c(1,2,4:5)]


##1.5


student <- read.fwf("student-fwf.txt",
              widths=c(4,15,20,15,4),
              col.names=c("id","name","email","major","year"))

student <- read.fwf("student-fwf-header.txt",
                    widths=c(4,15,20,15,4),
                    header=TRUE, sep="\t",skip=2)

student <- read.fwf("student-fwf.txt",widths=c(4,15,-20,15,4),
               col.names=c("id","name","major","year"))

customer <- c("John", "Peter", "Jane")
orderdate <- as.Date(c('2014-10-1','2014-1-2','2014-7-6'))
orderamount <- c(280, 100.50, 40.25)
order <- data.frame(customer,orderdate,orderamount)
names <- c("John", "Joan")
save(order, names, file="test.Rdata")
saveRDS(order,file="order.rds")
remove(order)

load("test.Rdata")

ord <- readRDS("order.rds")

data(iris)

ds= c("cars","iris")

data(list= ds)

save.image(file = "all.RData")

odd <- c(1,3,5,7)
even <- c(2,4,6,8)

save(list=c("odd","even"),file="OddEven.Rdata")


attach("test.Rdata")

data()



##1.6


dat <- read.csv("missing-data.csv", na.strings="")

dat.cleaned <- na.omit(dat)

is.na(dat[4,2])

is.na(dat$Income)

dat.income.cleaned <- dat[!is.na(dat$Income),]

nrow(dat.income.cleaned)

complete.cases(dat)

dat.cleaned <- dat[complete.cases(dat),]
nrow(dat.cleaned)

dat$Income[dat$Income==0] <- NA

mean(dat$Income)

mean(dat$Income, na.rm = TRUE)


##1.7

dat <- read.csv("missing-data.csv", na.strings = "")

dat$Income.imp.mean <- ifelse(is.na(dat$Income),
                mean(dat$Income, na.rm=TRUE), dat$Income)

dat$Income

dat$Income.imp.mean


rand.impute <- function(a) {
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
random.impute.data.frame <- function(dat, cols) {
  nms <- names(dat)
  for(col in cols) {
    name <- paste(nms[col],".imputed", sep = "")
    dat[name] <- rand.impute(dat[,col])
  }
  dat
}

random.impute.data.frame(dat, c(1,2))

salary <- c(20000, 30000, 25000, 40000, 30000, 34000, 30000)

family.size <- c(4,3,2,2,3,4,3)

car <- c("Luxury", "Compact", "Midsize", "Luxury",
         "Compact", "Compact", "Compact")

prospect <- data.frame(salary, family.size, car)

prospect.cleaned <- unique(prospect)

nrow(prospect)

nrow(prospect.cleaned)

duplicated(prospect)

prospect[duplicated(prospect), ]


##1.8

install.packages("scales")

library(scales)

students <- read.csv("data-conversion.csv")

students$Income.rescaled <- rescale(students$Income)

rescale(students$Income)

(students$Income - min(students$Income)) /
  (max(students$Income) - min(students$Income))

rescale(students$Income, to = c(1, 100))

rescale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".rescaled", sep = "")
    dat[name] <- rescale(dat[,col])
  }
  cat(paste("Rescaled ", length(column.nos),
            " variable(s)\n"))
  dat
}

rescale.many(students, c(1,4))

##1.9


housing <- read.csv("BostonHousing.csv")

housing.z <- scale(housing)

housing.z

scale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- scale(dat[,col])
  }
  cat(paste("Scaled ", length(column.nos), " variable(s)\n"))
  dat
}

housing <- scale.many(housing, c(1,3,5:7))

names(housing)


##1.10

students <- read.csv("data-conversion.csv")

b <- c(-Inf, 10000, 31000, Inf)

names <- c("Low", "Medium", "High")

students$Income.cat <- cut(students$Income, breaks = b,
                           labels =names)
students

students$Income.cat1 <- cut(students$Income, breaks = b)
students

students$Income.cat2 <- cut(students$Income,breaks = 4,
                            labels = c("Level1", "Level2",
                                       "Level3","Level4"))
students



##1.11

install.packages("dummies")

library(dummies)

students <- read.csv("data-conversion.csv")

students.new <- dummy.data.frame(students, sep = ".")

names(students.new)

dummy(students$State, sep = ".")

students.new <- dummy.data.frame(students, sep = ".",
                                 all= FALSE)

names(students.new)

students.new1 <- dummy.data.frame(students,
                            names = c("State","Gender") ,
                            sep = ".")

names(students.new1)












