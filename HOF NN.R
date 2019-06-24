library(keras)
library(tidyverse)

baseball <- read.csv(file = "C:\\Users\\seanc\\Downloads\\baseballHOF.csv")

baseball2 <- sample_frac(baseball, 1L)
baseball2 <- baseball2[,-1]

split <- ceiling(nrow(baseball2) * 0.8)

train <- baseball2[1:split,]
train_lab <- train[,1]
train_lab <- ifelse(train_lab == "Yes", 1, 0)
train <- train[,-1]
test <- baseball2[(split+1):nrow(baseball2),]
test_lab <- test[,1]
test_lab <- ifelse(test_lab == "Yes", 1, 0)
test <- test[,-1]

mean <- apply(train, 2, mean)
std <- apply(train, 2, sd)

train <- scale(train, center = mean, scale = std)
test <- scale(test, center = mean, scale = std)

model <- keras_model_sequential() %>% layer_dense(units = 128, activation = "relu", 
                                                  input_shape = dim(train)[[2]]) %>%
  layer_dense(units = 64, activation = "relu") %>% 
  #layer_dense(units = 128, activation = "relu") %>% 
layer_dense(units = 1, activation = "sigmoid")

model %>% compile(optimizer = "adam", loss = "binary_crossentropy", metrics = c("accuracy"))


history <- model %>% fit(train, train_lab, epochs = 16, validation_data = list(test, test_lab))
 

train2 <- baseball2[1:split,]
test2 <- baseball2[(split+1):nrow(baseball2),]
View(train2)
str(train2)

a <- glm(HoF~Yrs+WAR+WAR7+JAWS+Jpos+JAWSratio+G+AB+R+H+HR+RBI+SB+BB+BA+OBP+SLG+OPS+OPSadj, data = train2, family = "binomial")
summary(a)
pred <- predict(a, test2[,-1])
probs <- exp(pred)/(1+exp(pred))
probs2 <- ifelse(probs >= .5, 1, 0)
cbind(test2[,1], probs2)
