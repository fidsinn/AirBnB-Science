if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rafalib)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rafalib)

#reading the csv-file
AirData <- read.csv("AirBnB_NYC.csv")

#factorizing of some data
AirData$neighbourhood_group <- as.factor(AirData$neighbourhood_group)
AirData$neighbourhood <- as.factor(AirData$neighbourhood)
AirData$room_type <- as.factor(AirData$room_type)
AirData <- AirData %>%
  mutate(manhattan=ifelse(neighbourhood_group=="Manhattan", "manhattan", "not_manhattan"))
AirData$manhattan <- as.factor(AirData$manhattan)

#inspect na values
AirData %>% filter(is.na(id)) %>% summarize(n())
AirData %>% filter(is.na(name)) %>% summarize(n())
AirData %>% filter(is.na(host_id)) %>% summarize(n())
AirData %>% filter(is.na(host_name)) %>% summarize(n())
AirData %>% filter(is.na(neighbourhood_group)) %>% summarize(n())
AirData %>% filter(is.na(neighbourhood)) %>% summarize(n())
AirData %>% filter(is.na(latitude)) %>% summarize(n())
AirData %>% filter(is.na(longitude)) %>% summarize(n())
AirData %>% filter(is.na(room_type)) %>% summarize(n())
AirData %>% filter(is.na(price)) %>% summarize(n())
AirData %>% filter(is.na(minimum_nights)) %>% summarize(n())
AirData %>% filter(is.na(number_of_reviews)) %>% summarize(n())
AirData %>% filter(is.na(reviews_per_month)) %>% summarize(n())
AirData %>% filter(is.na(calculated_host_listings_count)) %>% summarize(n())
AirData %>% filter(is.na(availability_365)) %>% summarize(n())

#clear na with 0 at reviews_per_month and delete last_review
AirData[is.na(AirData)] <- 0
AirData <- AirData %>%
  select(-last_review, 
         -host_name)
AirData %>% 
  group_by(neighbourhood_group) %>% 
  summarize(revMean=mean(reviews_per_month), 
            revDist=sum(reviews_per_month==0)/sum(reviews_per_month))
#NA values were splitted good through the neighbourhood_group for revCalc, so it is ok to set NA to 0

#count of offers per price
AirData %>%
  mutate(price_round = round(price, digits=-1)) %>%
  group_by(price_round) %>%
  ggplot(aes(price_round)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(trans="log10")

#reviewing price data for analysis
quantile(AirData$price, probs=c(0.005, 0.10, 0.25, 0.50, 0.75, 0.90, 0.995))

#reviewing the n() of prices <= 26
AirData %>% 
  filter(price <= 26) %>% 
  group_by(price) %>%
  arrange(price) %>% 
  summarize(n())

#Listings higher than 1000 Dollar and lower 10 Dollar are treated as outliers
AirData <- AirData %>%
  filter(price > 0 & price <= 1000)

#count of offers per minimum nights
AirData %>%
  ggplot(aes(minimum_nights)) +
  geom_histogram(bins = 20, fill = "#FF6666") +
  scale_x_log10()

#reviewing minimum nights data for analysis
quantile(AirData$minimum_nights, probs=c(0.005, 0.10, 0.25, 0.50, 0.75, 0.90, 0.995))

#Listings higher than 365 minimum nights are treated as outliers
AirData <- AirData %>%
  filter(minimum_nights < 365)

#count of offers per calculated_host_listings_count
AirData %>%
  ggplot(aes(calculated_host_listings_count)) +
  geom_histogram(bins = 20, fill = "#FF6666") +
  scale_x_log10()

#reviewing calculated_host_listings_count data for analysis
quantile(AirData$calculated_host_listings_count, probs=c(0.005, 0.10, 0.25, 0.50, 0.75, 0.90, 0.995))

#count of offers per availability_365
AirData %>%
  ggplot(aes(availability_365)) +
  geom_histogram(bins = 20, fill = "#FF6666") +
  scale_x_log10()

#count of listings per number_of_reviews
AirData %>%
  group_by(number_of_reviews) %>%
  ggplot(aes(number_of_reviews)) +
  geom_histogram(bins = 20, fill = "#FF6666")






#Split into train data and test data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = AirData$neighbourhood_group, times = 1, p = 0.2, list = FALSE)
train_set <- AirData[-test_index,]
test_set <- AirData[test_index,]

##PRE-ANALYSIS
#location
#x=longitude, y=latitude
train_set %>%
  ggplot(aes(longitude, latitude, col=neighbourhood_group)) +
  geom_point() #+ theme(legend.position = "none")

#groups count
train_set %>%
  group_by(neighbourhood_group) %>%
  summarize(offerings=n())

#price neighbourhood_group
train_set %>%
  group_by(neighbourhood_group) %>%
  summarize(mean_price=mean(price), med_price=median(price))

#price by longitude
train_set %>%
  ggplot(aes(longitude, price, col=neighbourhood_group)) +
  geom_point()

#price by neighbourhood boxplot
train_set %>%
  select(neighbourhood_group, neighbourhood, price) %>%
  ggplot(aes(y=price)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10") +
  facet_grid(. ~ neighbourhood_group) +
  xlab("")

#room_type in each neighbourhood_group
train_set %>%
  group_by(neighbourhood_group) %>%
  summarize(room=mean(room_type=="Private room"), 
            apt=mean(room_type=="Entire home/apt"), 
            shared=mean(room_type=="Shared room"))

train_set %>%
  group_by(neighbourhood_group) %>%
  summarize(room=mean(room_type=="Private room"), 
           apt=mean(room_type=="Entire home/apt"), 
           shared=mean(room_type=="Shared room")) %>%
  gather(room_type, percent, room:shared) %>%
  ggplot(aes(x=neighbourhood_group, y=percent, fill=room_type)) +
  geom_bar(position = "fill", stat="identity")

#minimum nights in each neighbourhood_group
train_set %>%
  group_by(neighbourhood_group) %>%
  summarize(min_nights=mean(minimum_nights))

#geographical availability
train_set %>%
  ggplot(aes(longitude, latitude, color=availability_365)) +
  geom_point()

#geographical ratings
train_set%>%
  filter(number_of_reviews<50) %>%
  ggplot(aes(longitude, latitude, color=number_of_reviews)) +
  geom_point(alpha=0.7)

#several observations on neighbourhood_groups
train_set %>%
  group_by(neighbourhood_group) %>%
  summarize(mean_price=mean(price),
            med_rpice=median(price),
            apt=mean(room_type=="Entire home/apt"),
            room=mean(room_type=="Private room"),
            shared=mean(room_type=="Shared room"),
            min_nights=mean(minimum_nights),
            calc_hlc=mean(calculated_host_listings_count),
            ava=mean(availability_365),
            # rev=mean(number_of_reviews)
  )

#METHODS

#smoothing of average price by longitude (local weighted regression)
train_set_mprice <- train_set %>%
  mutate(l=round(longitude, digits=2)) %>%
  group_by(l) %>% 
  summarize(n=n(), 
            mprice=mean(price), 
            manh=mean(neighbourhood_group=="Manhattan")) %>%
  filter(n>=10)

#min and max longitude of manhattan
min_Manh_long <- min(train_set %>% filter(neighbourhood_group=="Manhattan") %>% select(longitude))
max_Manh_long <- max(train_set %>% filter(neighbourhood_group=="Manhattan") %>% select(longitude))

#smoothline of price
train_set_mprice %>% 
  ggplot(aes(l, mprice, col=manh)) + 
  geom_point(size=3) + 
  geom_smooth(color="orange", 
              span = 0.25, 
              method = "loess", 
              method.args = list(degree=1)) +
  geom_vline(xintercept = min_Manh_long) +
  geom_vline(xintercept = max_Manh_long)

#ANALYSIS FOR LM
#filter rounded price groupings with n()>=5
glm_pre <- train_set %>%
  mutate(price_round = round(price, digits=-1)) %>%
  group_by(price_round) %>%
  mutate(n=n()) %>%
  filter(n>=5) %>%
  ungroup() %>%
  select(-price_round, -n)
#Proportion manhattan listings per price in tens 
glm_pre %>%
  mutate(price_round = round(price, digits=-1)) %>%
  group_by(price_round) %>%
  summarize(n=n(), proportion_manhattan=mean(neighbourhood_group=="Manhattan")) %>%
  mutate(proportion_manhattan = round(proportion_manhattan, digits=2)) %>%
  ggplot(aes(price_round, proportion_manhattan)) +
  geom_point() +
  geom_text(aes(label=proportion_manhattan))


#Proportion room type 'Apartment' per price
glm_pre %>%
  mutate(price_round = round(price, digits=-1)) %>%
  group_by(price_round) %>%
  summarize(n=n(), mrt=mean(room_type=="Entire home/apt")) %>%
  mutate(mrt = round(mrt, digits=2)) %>%
  ggplot(aes(price_round, mrt)) +
  geom_point() +
  geom_text(aes(label=mrt))

#logistic regression model (listing in manhattan with several predictors)
glm_fit <- glm_pre %>% 
  mutate(y = as.numeric(manhattan == "manhattan")) %>%
  glm(y ~ host_id +
        room_type +
        price +
        minimum_nights +
        number_of_reviews +
        reviews_per_month +
        calculated_host_listings_count +
        availability_365
      , data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

y_hat_logit <- ifelse(p_hat_logit > 0.5, "manhattan", "not_manhattan") %>% factor
confusionMatrix(y_hat_logit, test_set$manhattan)#$overall[["Accuracy"]]

#logistic regression model (listing in manhattan with price as predictor)
glm_fit <- glm_pre %>%
  mutate(y = as.numeric(manhattan == "manhattan")) %>%
  glm(y ~ price, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

y_hat_logit <- ifelse(p_hat_logit > 0.5, "manhattan", "not_manhattan") %>% factor
confusionMatrix(y_hat_logit, test_set$manhattan)#$overall[["Accuracy"]]

#logistic curve of the logistic regression model
tmp <- train_set %>% 
  mutate(x = round(price, digits=-1)) %>%
  group_by(x) %>%
  filter(n() >= 5) %>%
  summarize(prop = mean(manhattan == "manhattan"))
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2) +
  xlab("price") +
  ylab("proportion")

#10-FOLD CROSS VALIDATION
control <- trainControl(method = "cv", number = 10, p = .9)

#KNN train just for investigation purposes (needs long time)
m_knn <- train_set %>%
  train(manhattan~longitude+latitude,
        method="knn",
        data=.,
        tuneGrid = data.frame(k = seq(5))
        #trControl=control
  )
m_p_hat_knn <- predict(m_knn, test_set, type = "raw")
confusionMatrix(m_p_hat_knn, test_set$manhattan)#$overall["Accuracy"]

#DECISION TREE 
#(only using selected predictors for decision tree and random forest that are useful)
dec_fit <- train_set %>%
  select(host_id,
         room_type,
         price,
         minimum_nights,
         number_of_reviews,
         calculated_host_listings_count,
         availability_365,
         manhattan
  ) %>%
  rpart(manhattan ~ ., data=., model=TRUE)
fit_pred <- predict(dec_fit, test_set, type="class")
confusionMatrix(table(fit_pred, test_set$manhattan))

#ploting decision tree
rpart.plot(dec_fit)
#or
plot(dec_fit, margin=0.1)
text(dec_fit, cex = 0.75)

#predictors of decision tree in another plot
train_set %>%
  ggplot(aes(room_type, price, col=manhattan, size=calculated_host_listings_count)) +
  geom_point(alpha=0.2)+
  scale_y_continuous(trans="log2")

# #decision tree for price prediction
# dec_test_fit <- train_set %>%
#   select(host_id,
#          room_type,
#          neighbourhood_group,
#          minimum_nights,
#          number_of_reviews,
#          calculated_host_listings_count,
#          availability_365,
#          price
#   ) %>%
#   rpart(price ~ ., data=., model=TRUE)
# rpart.plot(dec_test_fit) ##???

#RANDOM FORESTS
#forest with multiple predictors
forest_fit <- train_set %>%
  select(host_id,
         room_type,
         price,
         minimum_nights,
         number_of_reviews,
         calculated_host_listings_count,
         availability_365,
         manhattan
  ) %>%
  randomForest(manhattan ~ ., data=., ntree=500)

forest_p_hat <- predict(forest_fit, test_set, type="response")
confusionMatrix(forest_p_hat, test_set$manhattan)#$overall["Accuracy"]

#plot trees vs errors
rafalib::mypar()
plot(forest_fit)

#result of random forest
rf_result <- data.frame(test_set$manhattan, forest_p_hat)
plot(rf_result)

#forest only with one predictor
forest_fit_price <- train_set %>%
  select(price,
         manhattan
  ) %>%
  randomForest(manhattan ~ ., data=., ntree=500)