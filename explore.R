train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

train$train = 1
test$train = 0
test$Footfall = -1

df = rbind(train, test)

df$Date = NULL
df$Location_Type = as.factor(df$Location_Type)
df$Park_ID = NULL

train_mod = subset(df, df$train == 1)
test_mod = subset(df, df$train == 0)

train_mod$train = NULL
test_mod$train = NULL

#train_mod$Park_ID = NULL
#levels(test_mod$Park_ID) = levels(train_mod$Park_ID)

model = glm(Footfall ~ . - ID, data = train_mod)
#rf = randomForest(Footfall ~ . - ID, data = train_mod)
pred = as.data.frame(predict(model, test_mod))
pred$ID = test_mod$ID
colnames(pred) = c("Footfall","ID")
write.csv(pred[,c(2,1)], "submission_lm.csv", row.names = FALSE)

library(lars)
df$Location_Type = NULL
df[is.na(df)] = 0
train_mod = subset(df, df$train == 1)
test_mod = subset(df, df$train == 0)
ind_var = as.matrix(train_mod[,2:14])
dep_var = as.matrix(train_mod[,15])
laa<- lars(ind_var, dep_var, type = 'lasso')
prediction <- predict.lars(laa,newx =as.matrix(test_mod[,2:14]), s=best_step, type= "fit")
pred = as.data.frame(prediction$fit)
pred$ID = test_mod$ID
colnames(pred) = c("Footfall","ID")
write.csv(pred[,c(2,1)], "submission_lasso.csv", row.names = FALSE)

