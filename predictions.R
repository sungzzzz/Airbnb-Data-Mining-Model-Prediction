library(gbm)  
library(randomForest)
library(data.table)
library(stringr)
library(e1071)
library("zoo")
library(readr)
#Import data
airbnb_train_x <- read.csv("./airbnb_train_x.csv")
airbnb_train_y <- read.csv("./airbnb-train-y.csv")
airbnb_test_x <- read.csv("./airbnb_test_x.csv")
airbnb_train_x[airbnb_train_x==""]=NA
#Select features
df<- subset(airbnb_train_x, select = c('cancellation_policy','host_identity_verified','host_is_superhost','host_listings_count','host_response_rate',
                                       'host_response_time','instant_bookable','price','property_type','cleaning_fee', 'extra_people',
                                       'security_deposit','bedrooms','availability_30','availability_60','availability_90','availability_365',
                                       'is_location_exact','minimum_nights','maximum_nights','room_type','host_verifications','amenities',
                                       'accommodates','beds','host_has_profile_pic','guests_included','require_guest_phone_verification',
                                       'require_guest_profile_picture','monthly_price','weekly_price','host_since','first_review',
                                       'house_rules','transit','state','is_business_travel_ready'))
df = data.frame(df, high_booking_rate = airbnb_train_y$high_booking_rate)

#Create dummies
df['property_type_apartment'] = ifelse(df$property_type == 'Apartment', 1, 0)
df$property_type_apartment[is.na(df$property_type_apartment)] = 0
df['property_type_house'] = ifelse(df$property_type == 'House', 1, 0)
df$property_type_house[is.na(df$property_type_house)] = 0
df['room_type_entire'] = ifelse(df$room_type == 'Entire home/apt', 1, 0)
df$room_type_entire[is.na(df$room_type_entire)] = 0
df['room_type_private'] = ifelse(df$room_type == 'Private room', 1, 0)
df$room_type_private[is.na(df$room_type_private)] = 0
df['room_type_shared'] = ifelse(df$room_type == 'Shared room', 1, 0)
df$room_type_shared[is.na(df$room_type_shared)] = 0

#is_business_travel_ready
df$is_business_travel_ready[is.na(df$is_business_travel_ready)] = 'f'
df$is_business_travel_ready = ifelse((df$is_business_travel_ready == 'f'),0,1)

#state
df$state[is.na(df$state)] = 'NY'

#transit
df$transit = ifelse(is.na(df$transit),0,1)

#house_rules
df$house_rules = ifelse(is.na(df$house_rules),0,1)

#first_review
df$first_review = substring(df$first_review,1,4)
df$first_review[is.na(df$first_review)] = 2016
df$first_review[df$first_review == 'STR-'] = 2016
df$first_review = as.numeric(df$first_review)


#host_since
df$host_since = substring(df$host_since,1,4)
df$host_since[is.na(df$host_since)] = 2015
df$host_since[df$host_since == 'f'] = 2015
df$host_since = as.numeric(df$host_since)

#monthly_price
df$monthly_price = ifelse(is.na(df$monthly_price),0,1)

#weekly_price
df$weekly_price = ifelse(is.na(df$weekly_price),0,1)

#require_guest_profile_picture
df$require_guest_profile_picture[is.na(df$require_guest_profile_picture)] = 'f'

#require_guest_phone_verification
df$require_guest_phone_verification[is.na(df$require_guest_phone_verification)] = 'f'

#host_has_profile_pic
df$host_has_profile_pic[is.na(df$host_has_profile_pic)] = 't'

#beds
df$beds[is.na(df$beds)] = '1.0'

#accommodates
df$accommodates[is.na(df$accommodates)] = 2

#is_location_exact
df$is_location_exact[is.na(df$is_location_exact)] = 't'

#minimum_nights
df$minimum_nights[is.na(df$minimum_nights)] = 1

#maximum_nights
df$maximum_nights = ifelse(df$maximum_nights > 1095, 1095, df$maximum_nights)
df$maximum_nights[is.na(df$maximum_nights)] = 1095

#cancellation_policy
df$cancellation_policy = ifelse(df$cancellation_policy == 'flexible', 1, 
                                ifelse(df$cancellation_policy == 'moderate', 2, 
                                       ifelse(df$cancellation_policy == 'strict', 3, 4)))
df$cancellation_policy[is.na(df$cancellation_policy)] = 3

#host_identity_verified
df$host_identity_verified[is.na(df$host_identity_verified)] ='t'

#host_is_superhost
df$host_is_superhost[is.na(df$host_is_superhost)] ='f'

#host_listings_count
df$host_listings_count[is.na(df$host_listings_count)] = '1.0'

#host_response_time
df$host_response_time = ifelse(df$host_response_time == 'within an hour', 1, 
                               ifelse(df$host_response_time == 'within a few hours', 2, 
                                      ifelse(df$host_response_time == 'within a day', 3, 4)))
df$host_response_time[is.na(df$host_response_time)] = 1

#instant_bookable
df$instant_bookable[is.na(df$instant_bookable)] = 'f'

#bedrooms
df$bedrooms[is.na(df$bedrooms)] = '3.0'

#host_listings_count
df$host_listings_count = as.numeric(df$host_listings_count)

#host_response_rate
df$host_response_rate = as.numeric(gsub("[\\%]", "", df$host_response_rate))
df$host_response_rate[is.na(df$host_response_rate)] = 100

#price
df$price = as.numeric(gsub("[\\$,]", "", df$price))
df$price[is.na(df$price)] = mean(na.omit(df$price))
#remove price=0 form data
df <- df[!(df$price == 0),]

##cleaning_fee_level
df$cleaning_fee = as.numeric(gsub("[\\$,]", "", df$cleaning_fee))
df$cleaning_fee[is.na(df$cleaning_fee)] = mean(na.omit(df$cleaning_fee))
#cleaning_fee_percent
df$cleaning_fee_percent = df$cleaning_fee / df$price
df$cleaning_fee_level = ifelse(df$cleaning_fee_percent == 0, 1, 
                               ifelse(df$cleaning_fee_percent < 0.5, 2, 
                                      ifelse(df$cleaning_fee_percent < 1, 3, 4)))

#extra_people
df$extra_people = as.numeric(gsub("[\\$,]", "", df$extra_people))
df$extra_people[is.na(df$extra_people)] = 0
#security_deposit
df$security_deposit = as.numeric(gsub("[\\$,]", "", df$security_deposit))
df$security_deposit[is.na(df$security_deposit)] = mean(na.omit(df$security_deposit))

#availabilty
df$availability_30 = as.numeric(df$availability_30)
df$availability_30[is.na(df$availability_30)] = 1
df$availability_60 = as.numeric(df$availability_60)
df$availability_60[is.na(df$availability_60)] = 1
df$availability_90 = as.numeric(df$availability_90)
df$availability_90[is.na(df$availability_90)] = 1
df$availability_365 = as.numeric(df$availability_365)
df$availability_365[is.na(df$availability_365)] = 0

#verification_ways
df['verification_ways']=str_count(df$host_verifications,',')+1
df$verification_ways[is.na(df$verification_ways)] = 4
#Internet
df['internet_connection']= as.numeric(str_detect(df$amenities,regex('internet|wireless internet|wifi',ignore_case = TRUE)))
df$internet_connection[is.na(df$internet_connection)] = 1
#Parking
df['parking']= as.numeric(str_detect(df$amenities,regex('parking',ignore_case = TRUE)))
df$parking[is.na(df$parking)] = 1
#Kitchen
df['kitchen']= as.numeric(str_detect(df$amenities,regex('Kitchen',ignore_case = TRUE)))
df$kitchen[is.na(df$kitchen)] = 1
#24-hour check-in
df['checkin24']= as.numeric(str_detect(df$amenities,regex('24-hour check-in',ignore_case = TRUE)))
df$checkin24[is.na(df$checkin24)] = 0
#Breakfast
df['breakfast']= as.numeric(str_detect(df$amenities,regex('breakfast',ignore_case = TRUE)))
df$breakfast[is.na(df$breakfast)] = 0
#ac
df['ac']= as.numeric(str_detect(df$amenities,regex('air conditioning',ignore_case = TRUE)))
df$ac[is.na(df$ac)] = 1
#friendly
df['friendly']= as.numeric(str_detect(df$amenities,regex('family/kid friendly',ignore_case = TRUE)))
df$friendly[is.na(df$friendly)] = 1
#gym
df['gym']= as.numeric(str_detect(df$amenities,regex('gym',ignore_case = TRUE)))
df$ac[is.na(df$ac)] = 0
#tv
df['tv']= as.numeric(str_detect(df$amenities,regex('tv',ignore_case = TRUE)))
df$tv[is.na(df$tv)] = 1
#washer
df['washer']= as.numeric(str_detect(df$amenities,regex('washer',ignore_case = TRUE)))
df$washer[is.na(df$washer)] = 1
#dryer
df['dryer']= as.numeric(str_detect(df$amenities,regex('dryer',ignore_case = TRUE)))
df$dryer[is.na(df$dryer)] = 1
#shampoo
df['shampoo']= as.numeric(str_detect(df$amenities,regex('shampoo',ignore_case = TRUE)))
df$shampoo[is.na(df$shampoo)] = 1
#essentials
df['essentials']= as.numeric(str_detect(df$amenities,regex('essentials',ignore_case = TRUE)))
df$essentials[is.na(df$essentials)] = 1
#hair_dryer
df['hair_dryer']= as.numeric(str_detect(df$amenities,regex('hair dryer',ignore_case = TRUE)))
df$hair_dryer[is.na(df$hair_dryer)] = 1
#gym
df['gym']= as.numeric(str_detect(df$amenities,regex('gym',ignore_case = TRUE)))
df$gym[is.na(df$gym)] = 0
#pool
df['pool']= as.numeric(str_detect(df$amenities,regex('pool',ignore_case = TRUE)))
df$pool[is.na(df$pool)] = 0
#hot-tub
df['hot_tub']= as.numeric(str_detect(df$amenities,regex('hot tub',ignore_case = TRUE)))
df$hot_tub[is.na(df$hot_tub)] = 0
#elevator
df['elevator']= as.numeric(str_detect(df$amenities,regex('elevator',ignore_case = TRUE)))
df$elevator[is.na(df$elevator)] = 0

#email
df['email']= as.numeric(str_detect(df$host_verifications,regex('email',ignore_case = TRUE)))
df$email[is.na(df$email)] = 1
#phone
df['phone']= as.numeric(str_detect(df$host_verifications,regex('phone',ignore_case = TRUE)))
df$phone[is.na(df$phone)] = 1
#reviews
df['reviews']= as.numeric(str_detect(df$host_verifications,regex('reviews',ignore_case = TRUE)))
df$reviews[is.na(df$reviews)] = 1
#facebook
df['facebook']= as.numeric(str_detect(df$host_verifications,regex('facebook',ignore_case = TRUE)))
df$facebook[is.na(df$facebook)] = 0


#Drop column we do not need
df <- subset(df, select = -c(property_type, cleaning_fee, cleaning_fee_percent,room_type,host_verifications,amenities))
df = na.omit(df)

#partition traing, validation
#Set the random seed
set.seed(12345)
#randomly partition the data into 30% testing data and the 70% training data.
instn = sample(nrow(df), 0.3*nrow(df))
valid <- df[instn,]
train <- df[-instn,]


##random forest
#set mtry ~= sqrt(26) = 5
#set 1000 trees (this is something you can tune)
train$high_booking_rate = as.factor(train$high_booking_rate)
rf.mod <- randomForest(high_booking_rate~verification_ways+parking+internet_connection+cancellation_policy+host_identity_verified+host_is_superhost+host_listings_count+host_response_rate
                       +host_response_time+instant_bookable+price+property_type_apartment+property_type_house
                       +extra_people+security_deposit+bedrooms+availability_30+availability_60+availability_90+availability_365
                       +is_location_exact+minimum_nights+maximum_nights+room_type_entire+room_type_private+room_type_shared,data=train,mtry=4,ntree=1000,importance=TRUE)
rf_preds <- predict(rf.mod,newdata=valid)
rf_acc <- sum(ifelse(rf_preds==valid$high_booking_rate,1,0))/nrow(valid)

rf_acc

train$high_booking_rate = as.factor(train$high_booking_rate)
##Bayes
bayes.mod <- naiveBayes(high_booking_rate~.,data=train)
#Predict
bayes_preds <- predict(bayes.mod,newdata=valid)
#Accuracy
bayes_acc = sum(bayes_preds == valid$high_booking_rate)/nrow(valid)
bayes_acc

##Bagging
bag.mod <- randomForest(high_booking_rate~.,data=train,mtry=10,importance=TRUE) 
#Predict
bag_preds <- predict(bag.mod,newdata=valid)
#Accuracy
bag_acc <- sum(ifelse(bag_preds==valid$high_booking_rate,1,0))/nrow(valid)
bag_acc

#knn
library(class)
#subset the non numerical columns
train1 <- subset(train, select= -c( host_identity_verified, host_is_superhost, instant_bookable, is_location_exact, host_has_profile_pic,require_guest_phone_verification,
                                    require_guest_profile_picture,state))
valid1 <- subset(valid, select = -c( host_identity_verified, host_is_superhost, instant_bookable, is_location_exact, host_has_profile_pic,require_guest_phone_verification,
                                     require_guest_profile_picture,state))

train$high_booking_rate <- as.numeric(train$high_booking_rate)
knn.pred <- knn(train1 ,valid1,train1$high_booking_rate,k=1)
knn.pred1 <- knn(train1 ,valid1,train1$high_booking_rate,k=2)
knn.pred2 <- knn(train1 ,valid1,train1$high_booking_rate,k=3)
knn.pred3 <- knn(train1 ,valid1,train1$high_booking_rate,k=4)
knn.pred4 <- knn(train1 ,valid1,train1$high_booking_rate,k=5)

## Accuracy
correct1 <- sum(ifelse(knn.pred== valid1$high_booking_rate,1,0))
accuracy1 <- 1-((correct)/nrow(valid1))
correct2 <- sum(ifelse(knn.pred1== valid1$high_booking_rate,1,0))
accuracy2 <- 1-((correct1)/nrow(valid1))
accuracy3 <- 1-((correct2)/nrow(valid1))
correct3 <- sum(ifelse(knn.pred3== valid1$high_booking_rate,1,0))
accuracy4 <- 1-((correct3)/nrow(valid1))
correct4 <- sum(ifelse(knn.pred4== valid1$high_booking_rate,1,0))
accuracy5 <- 1-((correct4)/nrow(valid1))
accuracy1
accuracy2
accuracy3
accuracy4
accuracy5

##Boosting
boost.mod <- gbm(high_booking_rate~.,data=train,distribution="bernoulli",
                 n.trees=2000,interaction.depth=4)
#Predict
boost_preds <- predict(boost.mod,newdata=valid,type='response',n.trees=2000)
#Classify with a cutoff and compute accuracy
boost_class <- ifelse(boost_preds>.5,1,0)
#Accuracy
boost_acc <- sum(ifelse(boost_class==valid$high_booking_rate,1,0))/nrow(valid)
boost_acc