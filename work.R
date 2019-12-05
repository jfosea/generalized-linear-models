# TITLE: STAT 429 PROJECT
# NAME: JANA OSEA
# DATE: DECEMBER 5, 2019
# DESCRIPTON: This is the working R code used to analyse
# cardiovasculare disease dataset.


# set up working environment
setwd("C:/Users/surfacepro/Desktop/indiv_429_2")
library(readr)
library(car)
library(leaps)
library(readr)
library(ggplot2)
library(gridExtra)
d <- read_delim("cardio_train.csv", ";", 
                escape_double = FALSE, col_types = cols(active = col_factor(levels = c("0", 
                                                                                       "1")), alco = col_factor(levels = c("0", 
                                                                                                                           "1")), cholesterol = col_factor(levels = c("1", 
                                                                                                                                                                      "2", "3")), gender = col_factor(levels = c("1", 
                                                                                                                                                                                                                 "2")), gluc = col_factor(levels = c("1", 
                                                                                                                                                                                                                                                     "2", "3")), smoke = col_factor(levels = c("0", 
                                                                                                                                                                                                                                                                                           "1"))), trim_ws = TRUE)
# get to know the dataset & categorization
names(d)
d$bmi <- d$weight / (d$height / 100) ^2
d$ageT <- d$age / 365
median(d$ageT)
d$age.cat <- ifelse(d$ageT < 53.98, 0, d$ageT)
d$age.cat <- ifelse(d$ageT > 53.98, 1, d$age.cat)
d$age.cat <- as.factor(d$age.cat)
bp <- d$id
for (i in 1:nrow(d)) {
  sys <- d$ap_hi[i]
  di <- d$ap_lo[i]
  if (sys <= 120 & di <= 80) {
    bp[i] <- 1
  } else if ((sys > 120 & sys <= 130) || di <= 80) {
    bp[i] <- 2
  } else if ((sys > 130 & sys <= 140) || (di > 80 && di <= 90)) {
    bp[i] <- 3
  } else if ((sys > 140) || di > 90) {
    bp[i] <- 4
  }
}
d$bp.cat <- as.factor(bp)

# plot dataset
names(d)
p <- theme(plot.title = element_text(size=45, hjust=0.5))
a <- ggplot(data=d, aes(x=ageT)) + geom_histogram(col="Black", fill="#ebc16d") + ggtitle("Age") + p
b <- ggplot(data=d, aes(x=height)) + geom_histogram(col="Black", fill="#ebc16d") + ggtitle("Height") + p
c <- ggplot(data=d, aes(x=weight)) + geom_histogram(col="Black", fill="#ebc16d") + ggtitle("Weight") + p
e <- ggplot(data=d, aes(x=bmi)) + geom_histogram(col="Black", fill="#ebc16d") + ggtitle("BMI") + p
f <- ggplot(data=d, aes(x=ap_hi)) + geom_histogram(col="Black", fill="#ebc16d") + ggtitle("Systolic") + p
g <- ggplot(data=d, aes(x=ap_lo)) + geom_histogram(col="Black", fill="#ebc16d") + ggtitle("Diastolic") + p
grid.arrange(a,b,c,e,f,g, nrow=2, ncol=3)

p <- theme(plot.title = element_text(size=65, hjust=0.5))
h <- ggplot(data=d, aes(x=gender)) + geom_bar(col="Black", fill="#ebc16d") + ggtitle("Gender") + p
i <- ggplot(data=d, aes(x=cholesterol)) + geom_bar(col="Black", fill="#ebc16d") + ggtitle("Cholesterol") + p
j <- ggplot(data=d, aes(x=gluc)) +  geom_bar(col="Black", fill="#ebc16d") + ggtitle("Glucose") + p
k <- ggplot(data=d, aes(x=smoke)) +  geom_bar(col="Black", fill="#ebc16d") + ggtitle("Smoker") + p
l <- ggplot(data=d, aes(x=alco)) + geom_bar(col="Black", fill="#ebc16d") + ggtitle("Alcohol") + p
m <- ggplot(data=d, aes(x=active)) +  geom_bar(col="Black", fill="#ebc16d") + ggtitle("Active") + p
n <- ggplot(data=d, aes(x=cardio)) +  geom_bar(col="Black", fill="#ebc16d") + ggtitle("Cardiovascular") + p
o <- ggplot(data=d, aes(x=bp.cat)) + geom_bar(col="Black", fill="#ebc16d") + ggtitle("Blood Pressure") + p
grid.arrange(h,i,j,k,l,m,n,o, nrow=2, ncol=4)


# separate testing data vs training data
set.seed(25)
id <- rep(1, nrow(d))
d.train <- d[id==1,]
d.test <- d[id==2,]
sum(nrow(d.test) + nrow(d.train))
length(which(d$cardio == 1))/nrow(d) # 50-50 ratio
length(which(d$cardio == 0))/nrow(d) # good for the analyses
n <- nrow(d.train)
n

# model 1
m1 <- glm(cardio~.-id-weight-height-age, data=d.train)
summary(m1)
vif(m1)


# creating a better model
step(m1, direction="backward", k = log(n))


# model 2
m2 <- glm(formula = cardio ~ gender + cholesterol + gluc + smoke + 
            alco + active + bmi + ageT + bp.cat, data = d.train)
summary(m2)
vif(m2)


# showing some results: log odds
lodds <- function(x) {
  int <- 1
  gender2 <- 0
  cholesterol2 <- 0
  cholesterol3 <- 0
  gluc2 <- 0
  gluc3 <- 0
  smoke1 <- 0
  alco1 <- 0
  active1 <- 0
  bmi <- 21
  ageT <- 53
  bp.cat2 <- 0
  bp.cat3 <- 0
  bp.cat4 <- 0
  if (x[1] == "base") {
    a <- c(int, gender2, cholesterol2, cholesterol3, gluc2, gluc3, smoke1, 
           alco1, active1, bmi, ageT, bp.cat2, bp.cat3, bp.cat4)
    return(sum(m2$coefficients*a))
  }
  num <- length(x)
  for (i in 1:num) {
    if (x[i] == "gender2") {
      gender2 <- 1
    }
    if (x[i] == "cholesterol2") {
      cholesterol2 <- 1
    }
    if (x[i] == "cholesterol2") {
      cholesterol2 <- 1
    }
    if (x[i] == "cholesterol3") {
      cholesterol3 <- 1
    }
    if (x[i] == "gluc2") {
      gluc3 <- 1
    }
    if (x[i] == "gluc3") {
      cholesterol2 <- 1
    }
    if (x[i] == "smoke1") {
      smoke1 <- 1
    }
    if (x[i] == "alco1") {
      alco1 <- 1
    }
    if (x[i] == "active1") {
      active1 <- 1
    }
    if (x[i] == "bp.cat2") {
      bp.cat2 <- 1
    }
    if (x[i] == "bp.cat3") {
      bp.cat3 <- 1
    }
    if (x[i] == "bp.cat4") {
      bp.cat4 <- 1
    }
  }
  a <- c(int, gender2, cholesterol2, cholesterol3, gluc2, gluc3, smoke1, 
             alco1, active1, bmi, ageT, bp.cat2, bp.cat3, bp.cat4)
  return(sum(m2$coefficients*a))
}

odds <- function(x){
  a <- exp(lodds(x))
  return(a)
}

results <- function(x) {
  ds <- data.frame(name = 0, logodds = 0, odds = 0)
  for(i in 1:length(x)) {
    ds[i,1] <- x[i]
    ds[i,2] <- lodds(x[i])
    ds[i,3] <- odds(x[i])
  }
  return(ds)
}

a <- c("base", "gender2", "cholesterol2", "cholesterol3", "gluc2", "gluc3", "smoke1", 
       "alco1", "active1", "bmi", "ageT", "bp.cat2", "bp.cat3", "bp.cat4")
b <- results(a)
Book1 <- read_table2("Book1.csv")
write.csv(Book1, "results.csv")
