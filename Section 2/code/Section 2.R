##2.1

auto <- read.csv("auto-mpg.csv", header = TRUE,
                 stringsAsFactors = FALSE)

auto$cylinders <- factor(auto$cylinders,
                      levels = c(3,4,5,6,8),
                      labels = c("3cyl", "4cyl", 
                                 "5cyl", "6cyl", "8cyl"))

summary(auto)

str(auto)

summary(auto$cylinders)

summary(auto$mpg)

mean(auto$mpg)
sd(auto$mpg)


##2.2


auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

auto[1:3, 8:9]

auto[1:3, c(8,9)]

auto[1:3,c("model_year", "car_name")]

auto[auto$mpg == max(auto$mpg) | auto$mpg == min(auto$mpg),]

auto[auto$mpg>30 & auto$cylinders==6, c("car_name","mpg")]

auto[auto$mpg >30 & auto$cyl==6, c("car_name","mpg")]

subset(auto, mpg > 30 & cylinders == 6,
       select=c("car_name","mpg"))

subset(auto, mpg > 30 & cylinders == 6)

# incorrect
auto[auto$mpg > 30]

# correct
auto[auto$mpg > 30, ]

auto[,c(-1,-9)]

auto[,-c(1,9)]

# -c("No", "car_name")

auto[, !names(auto) %in% c("No", "car_name")]

auto[auto$mpg %in% c(15,20),c("car_name","mpg")]

auto[1:2,c(FALSE,FALSE,TRUE)]

##2.3

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

carslist <- split(auto, auto$cylinders)

names(carslist)

str(carslist[1])

str(carslist[[1]])

names(carslist[[1]])

unsplit(carslist, auto$cylinders)

##2.4

install.packages("caret")

library(caret)

bh <- read.csv("BostonHousing.csv")

#Case 1 - numerical target variable and two partitions

trg.idx <- createDataPartition(bh$MEDV, p = 0.8, list = FALSE)

trg.part <- bh[trg.idx, ]
val.part <- bh[-trg.idx, ]

#Case 2 - numerical target variable and three partitions

trg.idx <- createDataPartition(bh$MEDV, p = 0.7, list = FALSE)

trg.part <- bh[trg.idx, ]
temp <- bh[-trg.idx, ]

val.idx <- createDataPartition(temp$MEDV, p = 0.5, list = FALSE)
val.part <- temp[val.idx, ]
test.part <- temp[-val.idx, ]
#Case 3 - categorical target variable and two partitions

bh2 <- read.csv("boston-housing-classification.csv")

trg.idx <- createDataPartition(bh2$MEDV_CAT, p=0.7,list =FALSE)
trg.part <- bh2[trg.idx, ]
val.part <- bh2[-trg.idx, ]
#Case 4 - categorical target variable and three partitions

bh3 <- read.csv("boston-housing-classification.csv")

trg.idx <- createDataPartition(bh3$MEDV_CAT, p=0.7,list =FALSE)
trg.part <- bh3[trg.idx, ]
temp <- bh3[-trg.idx, ]

val.idx <- createDataPartition(temp$MEDV_CAT, p=0.5,list=FALSE)
val.part <- temp[val.idx, ]
test.part <- temp[-val.idx, ]

rda.cb.partition2 <- function(ds, target.index, prob) {
  library(caret)
  train.idx <- createDataPartition(y=ds[,target.index],
                                   p = prob, list = FALSE)
  list(train = ds[train.idx, ], val = ds[-train.idx, ])
}


rda.cb.partition3 <- function(ds,
                    target.index, prob.train, prob.val) {
  library(caret)
  train.idx <- createDataPartition(y=ds[,target.index],
                    p = prob.train, list = FALSE)
  train <- ds[train.idx, ]
  temp <- ds[-train.idx, ]
  val.idx <- createDataPartition(y=temp[,target.index],
                    p = prob.val/(1-prob.train), list = FALSE)
  list(train = ds[train.idx, ],
       val = temp[val.idx, ], test = temp[-val.idx, ])
}

dat1 <- rda.cb.partition2(bh, 14, 0.8)

dat2 <- rda.cb.partition3(bh, 14, 0.7, 0.15)

names(dat1)

names(dat2)

dat1$train

dat1$val

sam.idx <- sample(1:nrow(bh), 50, replace = FALSE)

sam.idx

##2.5


auto <- read.csv("auto-mpg.csv")

auto$cylinders <- factor(auto$cylinders, 
        levels = c(3,4,5,6,8),
        labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

attach(auto)

# Histogram

hist(acceleration)

hist(acceleration, col="blue", xlab = "acceleration",
     main = "Histogram of acceleration", breaks = 15)

hist(mpg, col = rainbow(12))

#Boxplot

boxplot(mpg, xlab = "Miles per gallon") #auto$mpg ~ auto$cylinders

boxplot(mpg ~ model_year, xlab = "Miles per gallon")

boxplot(mpg ~ cylinders)

#Scatterplot

plot(mpg ~ horsepower)

#Scatterplot matrices

pairs(~mpg+displacement+horsepower+weight)


hist(mpg, prob=TRUE)
lines(density(mpg))

plot(mpg ~ horsepower)

reg <- lm(mpg ~ horsepower)

abline(reg)

plot(mpg ~ horsepower, type = "n")

with(subset(auto, cylinders == "8cyl"),
     points(horsepower, mpg, col = "blue"))

with(subset(auto, cylinders == "6cyl"),
     points(horsepower, mpg, col = "red"))

with(subset(auto, cylinders == "5cyl"),
     points(horsepower, mpg, col = "yellow"))

with(subset(auto, cylinders == "4cyl"),
     points(horsepower, mpg, col = "green"))

with(subset(auto, cylinders == "3cyl"),
       points(horsepower, mpg))


## 2.6


auto <- read.csv("auto-mpg.csv")

cylinders <- factor(cylinders,
              levels = c(3,4,5,6,8),
              labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

attach(auto)

old.par = par()

par(mfrow = c(1,2))

with(auto, {
  plot(mpg ~ weight, main = "Weight vs. mpg")
  plot(mpg ~ acceleration, main = "Acceleration vs. mpg")
}
)

par(old.par)


##2.7


auto <- read.csv("auto-mpg.csv")

auto$cylinders <- factor(cylinders, levels = c(3,4,5,6,8),
            labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

attach(auto)

postscript(file = "auto-scatter.ps")

boxplot(mpg)

dev.off()

pdf(file = "auto-scatter.pdf")

boxplot(mpg)

dev.off()



##2.8


auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

cyl.factor <- factor(auto$cylinders,
             labels=c("3cyl","4cyl","5cyl","6cyl","8cyl"))

library(lattice)

bwplot(~auto$mpg|cyl.factor, main="MPG by Number of Cylinders",
       xlab="Miles per Gallon")

xyplot(mpg~weight|cyl.factor, data=auto,
       main="Weight Vs MPG by Number of Cylinders",
       ylab="Miles per Gallon", xlab="Car Weight")

trellis.par.set(theme = col.whitebg())

bwplot(~mpg|cyl.factor, data=auto,main="MPG by Number Of Cylinders",
       xlab="Miles per Gallon",layout=c(2,3),aspect=1)

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

auto$cylinders <- factor(auto$cylinders,
                         labels=c("3cyl","4cyl","5cyl","6cyl","8cyl"))



install.packages("ggplot2")

library(ggplot2)

plot <- ggplot(auto, aes(weight, mpg))

plot + geom_point()

plot + geom_point(alpha=1/2, size=5,aes(color=factor(cylinders)))+
  geom_smooth(method="lm", se=FALSE, col="green") +
  facet_grid(cylinders~.) +
  theme_bw(base_family = "Calibri", base_size = 10) + 
  labs(x ="Weight") + 
  labs(y = "Miles Per Gallon") + 
  labs(title = "MPG Vs Weight")


#generic
#qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=,
#     formula=, facets=, xlim=, ylim= xlab=, ylab=, main=, sub=)

# Boxplots of mpg by number of cylinders
qplot(cylinders, mpg, data=auto, geom=c("jitter"),
        color=cylinders, fill=cylinders,
        main="Mileage by Number of Cylinders",
        xlab="", ylab="Miles per Gallon")

# Regression of mpg by weight for each type of cylinders
qplot(weight, mpg, data=auto, geom=c("point", "smooth"),
        method="lm", formula=y~x, color=cylinders,
        main="Regression of MPG on Weight")

# Cut with desired range
breakpoints <- c(8,13,18,23)

# Cut using Quantile function (another approach)
breakpoints <- quantile(auto$acceleration, seq(0, 1,
                                length= 4), na.rm = TRUE)

auto$accelerate.factor <- cut(auto$acceleration, breakpoints)

qplot(auto$accelerate.factor, mpg, data=auto, geom=c("jitter"),
      color=cylinders, fill=cylinders,
      main="Mileage by Number of Cylinders",
      xlab="", ylab="Miles per Gallon")

##2.9


bike <- read.csv("daily-bike-rentals.csv")

bike$season <- factor(bike$season, levels = c(1,2,3,4),
               labels = c("Spring", "Summer", "Fall", "Winter"))

attach(bike)

par(mfrow = c(2,2))

spring <- subset(bike, season == "Spring")$cnt
summer <- subset(bike, season == "Summer")$cnt
fall <- subset(bike, season == "Fall")$cnt
winter <- subset(bike, season == "Winter")$cnt

hist(spring, prob=TRUE,
     xlab = "Spring daily rentals", main = "")
lines(density(spring))

hist(summer, prob=TRUE,
     xlab = "Summer daily rentals", main = "")
lines(density(summer))

hist(fall, prob=TRUE,
     xlab = "Fall daily rentals", main = "")
lines(density(fall))

hist(winter, prob=TRUE,
     xlab = "Winter daily rentals", main = "")
lines(density(winter))

#Using ggplot2

qplot(cnt, data = bike) + facet_wrap(~ season, nrow=2) +
  geom_histogram(fill = "blue")

qplot(cnt, data = bike, fill = season)

qplot(season, cnt, data = bike, 
      geom = c("boxplot"), fill = season)

ggplot(bike, aes(x = season, y = cnt)) + geom_boxplot()


##2.10

bike <- read.csv("daily-bike-rentals.csv")

bike$season <- factor(bike$season, levels = c(1,2,3,4),
               labels = c("Spring", "Summer", "Fall", "Winter"))

bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),
              labels = c("Clear", "Misty/cloudy", "Light snow"))

attach(bike)

qplot(weathersit, cnt, data = bike, geom = c("boxplot"),
      fill = weathersit)

qplot(weathersit, cnt, data = bike, geom = c("boxplot",
                       "jitter"), fill = weathersit)

##2.11


bike <- read.csv("daily-bike-rentals.csv")

library(ggplot2)

bike$season <- factor(bike$season, levels = c(1,2,3,4),
             labels = c("Spring", "Summer", "Fall", "Winter"))

bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),
             labels = c("Clear", "Misty/cloudy", "Light snow"))

bike$windspeed.fac <- cut(bike$windspeed, breaks=3,
             labels=c("Low", "Medium", "High"))

bike$weekday <- factor(bike$weekday, levels = c(0:6),
            labels = c("Sun", "Mon", "Tue", "Wed",
                       "Thur", "Fri", "Sat"))

attach(bike)

plot <- ggplot(bike,aes(temp,cnt))

plot + geom_point(size=3, aes(color=factor(windspeed.fac))) +
  geom_smooth(method="lm", se=FALSE, col="red") +
  facet_grid(weekday ~ season) +
  theme(legend.position="bottom")

