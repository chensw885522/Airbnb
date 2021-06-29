Airbnb=read.csv("listings.csv")
Airbnb=read.csv("listings_full_2.csv")
str(Airbnb)
head(Airbnb)
nrow(Airbnb)

library(ggmap)
library(ggplot2)

qmap(location = "Seattle")
qmap(location = "Seattle", zoom = 12)
#---------------------------------------------------------------------------------------
#some interesting findings

ggplot(Airbnb, aes(room_type))+geom_bar()
ggplot(Airbnb, aes(room_type))+geom_bar(aes(fill = factor(Airbnb$bedrooms)))
ggplot(Airbnb, aes(room_type))+geom_bar(aes(fill = factor(Airbnb$accommodates)))
ggplot(Airbnb, aes(neighbourhood_group_cleansed))+geom_bar(aes(fill = factor(Airbnb$bedrooms)))


table(Airbnb$room_type,Airbnb$bedrooms)
table(Airbnb$bedrooms,Airbnb$accommodates)
MonthIncome = Airbnb$reviews_per_month*Airbnb$minimum_nights*Airbnb$price
Airbnb.income=data.frame(Airbnb,MonthIncome)
str(Airbnb.income)
Airbnb.income=na.omit(Airbnb.income)
aggregate(Airbnb.income$MonthIncome, list(Airbnb.income$neighbourhood_group_cleansed), mean)

gd <- Airbnb.income %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  summarise(
    MonthIncome = mean(MonthIncome)
  )
head(gd)
gd$neighbourhood_group_cleansed <- factor(gd$neighbourhood_group_cleansed , levels = gd$neighbourhood_group_cleansed [order(gd$MonthIncome)])
ggplot(gd, aes(x=factor(neighbourhood_group_cleansed),y=MonthIncome)) +
  geom_bar(stat = "identity")+ 
  geom_text(data=gd,aes(label=round(MonthIncome,digits =0)),vjust=-0)


gd2 <- Airbnb.income %>% 
  group_by(neighbourhood_group_cleansed,bedrooms) %>% 
  summarise(
    MonthIncome = mean(MonthIncome)
  )
gd2$incomebybedrooms=gd2$MonthIncome/gd2$bedrooms

gd2= gd2 %>% filter(bedrooms!=0)
gd2$incomebybedrooms
ggplot(gd2, aes(x=factor(neighbourhood_group_cleansed), fill= factor(bedrooms),y=MonthIncome)) +
  geom_bar(stat="identity",color="white")+
  geom_text(data=gd2,size=3,aes(label=round(MonthIncome,digits =0)),position = position_stack(vjust = 0.5))

ggplot(gd2, aes(x=factor(neighbourhood_group_cleansed), fill= factor(bedrooms),y=incomebybedrooms)) +
  geom_bar(stat="identity",color="white")+
  geom_text(data=gd2,size=3,aes(label=round(incomebybedrooms,digits =0)),position = position_stack(vjust = 0.5))


gd2 <- na.omit(Airbnb) %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  summarise(
    occupancy = mean(reviews_per_month*minimum_nights )
  )

ggplot(gd2, aes(x=factor(neighbourhood_group_cleansed),y=occupancy)) +
  geom_bar(stat = "identity")+
  geom_text(data=gd,aes(label=round(occupancy,digits =0)),vjust=0)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))

#neighbourhood cleaned
table(Airbnb$neighbourhood_cleansed,Airbnb$neighbourhood_group_cleansed)
gd2 <- Airbnb.income %>% 
  group_by(neighbourhood_cleansed,bedrooms) %>% 
  summarise(
    MonthIncome = mean(MonthIncome)
  )
gd2$incomebybedrooms=gd2$MonthIncome/gd2$bedrooms

gd2= gd2 %>% filter(bedrooms!=0)

ggplot(gd2, aes(x=factor(neighbourhood_cleansed), fill= factor(bedrooms),y=incomebybedrooms)) +
  geom_bar(stat="identity",color="white")+
  geom_text(data=gd2,size=3,aes(label=round(incomebybedrooms,digits =0)),position = position_stack(vjust = 0.5))+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))


gd2 <- Airbnb.income %>% 
  group_by(neighbourhood_cleansed,bedrooms) %>% 
  summarise(
    MonthIncome = mean(MonthIncome)
  )
ggplot(gd2, aes(x=factor(neighbourhood_cleansed), fill= factor(bedrooms),y=MonthIncome)) +
  geom_bar(stat="identity",color="white")+
  geom_text(data=gd2,size=2,aes(label=round(MonthIncome,digits =0)),position = position_stack(vjust = 0.5))+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))
#-------------------------------------------------------------------------
# plotting the map with some points on it
require(ggmap)
require(ggplot2)
mapgilbert <- get_map(location = c(lon = mean(Airbnb.income$longitude), lat = mean(Airbnb.income$latitude)), 
                      zoom = 11,
                      scale = 2)

# plotting the map with some points on it
Airbnb.income=na.omit(Airbnb.income)
str(Airbnb.income)
ggmap(mapgilbert) +
  geom_point(data = Airbnb.income, aes(x = longitude, y = latitude,size =logprice, fill=neighbourhood_group_cleansed,alpha = 0.8),  shape = 21) +
  guides(size=FALSE)
  
#--------------------------------------------------------------------------------------------
#Monthly income as DV
logprice=log(Airbnb.income$MonthIncome)
Airbnb.income=data.frame(Airbnb.income,logprice)
par(mfrow = c(1, 2))
qqnorm(Airbnb.income$MonthIncome, main = 'Q-Q plot of price')
qqnorm(Airbnb.income$logprice, main = 'Q-Q plot of log price')
par(mfrow = c(1, 1))

ggplot(Airbnb.income, aes(x = factor(host_identity_veritied), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$host_identity_veritied), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(host_has_protile_pic), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$host_has_protile_pic), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(instant_bookable ), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$instant_bookable), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(require_guest_protile_picture ), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$require_guest_protile_picture), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(require_guest_phone_verification), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$require_guest_phone_verification), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(neighbourhood_group_cleansed ), y = logprice)) + geom_boxplot()
aggregate(Airbnb.income$bedrooms, list(Airbnb$neighbourhood_group_cleansed), mean)
df_aov = aov(logprice ~ neighbourhood_group_cleansed, data = Airbnb.income)
summary(df_aov)
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)

ggplot(Airbnb.income, aes(x = factor(room_type), y = logprice)) + geom_boxplot()
df_aov = aov(logprice ~ room_type, data = Airbnb.income)
summary(df_aov)
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)



ggplot(Airbnb.income, aes(x = factor(cancellation_policy), y = logprice)) + geom_boxplot()
df_aov = aov(logprice ~ cancellation_policy, data = Airbnb.income)
summary(df_aov)

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)

ggplot(Airbnb, aes(x = factor(bed_type), y = logprice)) + geom_boxplot()


library('corrplot') 
R1 = cor(na.omit(Airbnb.income[, c('logprice','accommodates', 'bedrooms', 'bathrooms','review_scores_rating', 'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                            'review_scores_communication', 'review_scores_location','review_scores_value','reviews_per_month')]), method = 'pearson')
print(R1)
corrplot(R1, method = "circle")
cor.test(Airbnb.income$logprice,Airbnb.income$review_scores_checkin)

str(Airbnb.income)

require(dplyr)
Airbnb2=Airbnb.income %>% select (-c(name,host_id,host_name,host_since,host_listings_count,host_total_listings_count,
                                     street,neighbourhood_cleansed,zipcode,latitude,longitude,beds,
                                     security_deposit,cleaning_fee,
                                     minimum_nights,number_of_reviews ,first_review,last_review,price,MonthIncome,reviews_per_month))
Airbnb2[, c('accommodates','bathrooms','bedrooms',
            'review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
            'review_scores_checkin' ,'review_scores_communication','review_scores_location',
            'review_scores_value')] = 
  lapply(Airbnb2[, c('accommodates','bathrooms','bedrooms',
                     'review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
                     'review_scores_checkin' ,'review_scores_communication','review_scores_location',
                     'review_scores_value')], 
         scale)
model=lm(logprice~host_identity_veritied+instant_bookable +factor(neighbourhood_group_cleansed)+
           factor(room_type)+accommodates+bathrooms+bedrooms+
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
           review_scores_checkin +review_scores_communication+review_scores_location+
           review_scores_value+ factor(cancellation_policy ),data=Airbnb2)
summary(model)
AIC(model)
library(MASS)
lm.step = stepAIC(model, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
AIC(lm.step)
x11()
par(mfrow = c(2, 2))
plot(lm.step)
detach(package:MASS)
#------------------------------------------------------------------------------------------
#occupancy
occupancy=Airbnb$reviews_per_month*Airbnb$minimum_nights
MonthIncome = Airbnb$reviews_per_month*Airbnb$minimum_nights*Airbnb$price
Airbnb.income=data.frame(Airbnb,MonthIncome,occupancy)
Airbnb.income=na.omit(Airbnb.income)
nrow(Airbnb.income)
Airbnb.income=Airbnb.income %>% filter(Airbnb.income$occupancy<32)
outlier = Airbnb.income %>% filter(Airbnb.income$occupancy>32)


logprice=log(Airbnb.income$occupancy)
Airbnb.income=data.frame(Airbnb.income,logprice)
par(mfrow = c(1, 2))
qqnorm(Airbnb.income$occupancy, main = 'Q-Q plot of price')
qqnorm(Airbnb.income$logprice, main = 'Q-Q plot of log price')
par(mfrow = c(1, 1))
ggplot(Airbnb.income, aes(x = factor(host_identity_veritied), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$host_identity_veritied), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(host_has_protile_pic), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$host_has_protile_pic), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(instant_bookable ), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$instant_bookable), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(require_guest_protile_picture ), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$require_guest_protile_picture), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(require_guest_phone_verification), y = logprice)) + geom_boxplot()
t.test(Airbnb.income$logprice~factor(Airbnb.income$require_guest_phone_verification), alternative = "two.sided")

ggplot(Airbnb.income, aes(x = factor(neighbourhood_group_cleansed ), y = logprice)) + geom_boxplot()
aggregate(Airbnb.income$bedrooms, list(Airbnb$neighbourhood_group_cleansed), mean)
df_aov = aov(logprice ~ neighbourhood_group_cleansed, data = Airbnb.income)
summary(df_aov)
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)

ggplot(Airbnb.income, aes(x = factor(room_type), y = logprice)) + geom_boxplot()
df_aov = aov(logprice ~ room_type, data = Airbnb.income)
summary(df_aov)
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)



ggplot(Airbnb.income, aes(x = factor(cancellation_policy), y = logprice)) + geom_boxplot()
df_aov = aov(logprice ~ cancellation_policy, data = Airbnb.income)
summary(df_aov)

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)

ggplot(Airbnb, aes(x = factor(bed_type), y = logprice)) + geom_boxplot()


library('corrplot') 
R1 = cor(na.omit(Airbnb.income[, c('logprice','price','accommodates', 'bedrooms', 'bathrooms','review_scores_rating', 'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                                   'review_scores_communication', 'review_scores_location','review_scores_value','reviews_per_month')]), method = 'pearson')
print(R1)
corrplot(R1, method = "circle")
cor.test(Airbnb.income$logprice,Airbnb.income$accommodates)

str(Airbnb.income)

require(dplyr)
Airbnb2=Airbnb.income %>% select (-c(name,host_id,host_name,host_since,host_listings_count,host_total_listings_count,
                                     street,neighbourhood_cleansed,zipcode,latitude,longitude,beds,
                                     security_deposit,cleaning_fee,
                                     minimum_nights,number_of_reviews ,first_review,last_review,MonthIncome,reviews_per_month))
str(Airbnb2)
Airbnb2[, c('price','accommodates','bathrooms','bedrooms',
            'review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
            'review_scores_checkin' ,'review_scores_communication','review_scores_location',
            'review_scores_value')] = 
  lapply(Airbnb2[, c('price','accommodates','bathrooms','bedrooms',
                     'review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
                     'review_scores_checkin' ,'review_scores_communication','review_scores_location',
                     'review_scores_value')], 
         scale)
model=lm(logprice~price+host_identity_veritied+instant_bookable +factor(neighbourhood_group_cleansed)+
           factor(room_type)+accommodates+bathrooms+bedrooms+
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
           review_scores_checkin +review_scores_communication+review_scores_location+
           review_scores_value+ factor(cancellation_policy ),data=Airbnb2)
summary(model)

library(MASS)
lm.step = stepAIC(model, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
AIC(model)
AIC(lm.step)
detach(package:MASS)
#----------------------------------------------------------------------------------------
#check normality
logprice=log(Airbnb$price)
Airbnb=data.frame(Airbnb,logprice)
par(mfrow = c(1, 2))
qqnorm(Airbnb$price, main = 'Q-Q plot of price')
qqnorm(Airbnb$logprice, main = 'Q-Q plot of log price')
par(mfrow = c(1, 1))



Airbnb$logprice
#t-test and ANOVA test
ggplot(Airbnb, aes(x = factor(host_identity_veritied), y = logprice)) + geom_boxplot()
t.test(Airbnb$logprice~factor(Airbnb$host_identity_veritied), alternative = "two.sided")

ggplot(Airbnb, aes(x = factor(host_has_protile_pic), y = logprice)) + geom_boxplot()
t.test(Airbnb$logprice~factor(Airbnb$host_has_protile_pic), alternative = "two.sided")

ggplot(Airbnb, aes(x = factor(instant_bookable ), y = logprice)) + geom_boxplot()
t.test(Airbnb$logprice~factor(Airbnb$instant_bookable), alternative = "two.sided")

ggplot(Airbnb, aes(x = factor(require_guest_protile_picture ), y = logprice)) + geom_boxplot()
t.test(Airbnb$logprice~factor(Airbnb$require_guest_protile_picture), alternative = "two.sided")

ggplot(Airbnb, aes(x = factor(require_guest_phone_verification), y = logprice)) + geom_boxplot()
t.test(Airbnb$logprice~factor(Airbnb$require_guest_phone_verification), alternative = "two.sided")

ggplot(Airbnb, aes(x = factor(neighbourhood_group_cleansed ), y = logprice)) + geom_boxplot()
aggregate(Airbnb$bedrooms, list(Airbnb$neighbourhood_group_cleansed), mean)
df_aov = aov(logprice ~ neighbourhood_group_cleansed, data = Airbnb)
summary(df_aov)
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)

ggplot(Airbnb, aes(x = factor(room_type), y = logprice)) + geom_boxplot()
df_aov = aov(logprice ~ room_type, data = Airbnb)
summary(df_aov)
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)



ggplot(Airbnb, aes(x = factor(cancellation_policy), y = logprice)) + geom_boxplot()
df_aov = aov(logprice ~ cancellation_policy, data = Airbnb)
summary(df_aov)

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)

ggplot(Airbnb, aes(x = factor(bed_type), y = logprice)) + geom_boxplot()
#correlation matrix
options(repr.plot.width=8, repr.plot.height=8)
require(car)
scatterplotMatrix(~ logprice+accommodates + bedrooms + bathrooms  , data = Airbnb)
scatterplotMatrix(~ logprice+ review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
                    review_scores_checkin +review_scores_communication+review_scores_location+review_scores_value 
                  , data = Airbnb)

library('corrplot') 
R1 = cor(na.omit(Airbnb[, c('logprice','accommodates', 'bedrooms', 'bathrooms','review_scores_rating', 'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                            'review_scores_communication', 'review_scores_location','review_scores_value')]), method = 'pearson')
print(R1)
corrplot(R1, method = "circle")
cor.test(Airbnb$logprice,Airbnb$review_scores_accuracy)


#linear regression model
require(dplyr)
Airbnb2=Airbnb %>% select (-c(name,host_id,host_name,host_since,host_listings_count,host_total_listings_count,
                              street,neighbourhood_cleansed,zipcode,latitude,longitude,beds,
                              security_deposit,cleaning_fee,
                             minimum_nights,number_of_reviews ,first_review,last_review,instant_bookable,price))
Airbnb2=na.omit(Airbnb2)
mod.mat = model.matrix(logprice ~ ., data = Airbnb2)
mod.mat[1:10, ]
#without intercept
mod.mat.2 = model.matrix(logprice ~ .-1, data = Airbnb2)
mod.mat.2[1:10, ]
str(Airbnb2)
#scale
Airbnb2[, c('accommodates','bathrooms','bedrooms',
                  'review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
                  'review_scores_checkin' ,'review_scores_communication','review_scores_location',
                  'review_scores_value','reviews_per_month')] = 
  lapply(Airbnb2[, c('accommodates','bathrooms','bedrooms',
                     'review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
                     'review_scores_checkin' ,'review_scores_communication','review_scores_location',
                     'review_scores_value','reviews_per_month')], 
         scale)
model=lm(logprice~host_has_protile_pic+host_identity_veritied +factor(neighbourhood_group_cleansed)+
           factor(room_type)+accommodates+bathrooms+bedrooms+
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
           review_scores_checkin +review_scores_communication+review_scores_location+
           review_scores_value+ factor(cancellation_policy )+require_guest_protile_picture+
           require_guest_phone_verification+reviews_per_month ,data=Airbnb2,na.action=na.omit)
summary(model)
table(Airbnb2$neighbourhood_group_cleansed)
Airbnb2$score <- predict(model, data = Airbnb2)
Airbnb2$resids <- Airbnb2$logprice - Airbnb2$score

options(repr.plot.width=8, repr.plot.height=4)
plot.svd.reg <- function(df, k = 60){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$logprice)
  SST <- sum((df$logprice - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}


plot.svd.reg(Airbnb2)

