train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

train$train = 1
test$train = 0
test$Footfall = -1

df = rbind(train, test)

df$Direction_Of_Wind[is.na(df$Direction_Of_Wind)] = mean(df$Direction_Of_Wind[!is.na(df$Direction_Of_Wind)])
#put in quadrants as factors - failed

df$Var1[is.na(df$Var1)] = mean(df$Var1[!is.na(df$Var1)])
df$Average_Breeze_Speed[is.na(df$Average_Breeze_Speed)] = mean(df$Average_Breeze_Speed[!is.na(df$Average_Breeze_Speed)])
df$Average_Atmospheric_Pressure[is.na(df$Average_Atmospheric_Pressure)] = mean(df$Average_Atmospheric_Pressure[!is.na(df$Average_Atmospheric_Pressure)])
df$Average_Moisture_In_Park[is.na(df$Average_Moisture_In_Park)] = mean(df$Average_Moisture_In_Park[!is.na(df$Average_Moisture_In_Park)])

df$Min_Breeze_Speed[is.na(df$Min_Breeze_Speed)] = mean(df$Min_Breeze_Speed[!is.na(df$Min_Breeze_Speed)])
df$Min_Atmospheric_Pressure[is.na(df$Min_Atmospheric_Pressure)] = mean(df$Min_Atmospheric_Pressure[!is.na(df$Min_Atmospheric_Pressure)])
df$Min_Moisture_In_Park[is.na(df$Min_Moisture_In_Park)] = mean(df$Min_Moisture_In_Park[!is.na(df$Min_Moisture_In_Park)])
df$Min_Ambient_Pollution[is.na(df$Min_Ambient_Pollution)] = mean(df$Min_Ambient_Pollution[!is.na(df$Min_Ambient_Pollution)])

df$Max_Breeze_Speed[is.na(df$Max_Breeze_Speed)] = mean(df$Max_Breeze_Speed[!is.na(df$Max_Breeze_Speed)])
df$Max_Atmospheric_Pressure[is.na(df$Max_Atmospheric_Pressure)] = mean(df$Max_Atmospheric_Pressure[!is.na(df$Max_Atmospheric_Pressure)])
df$Max_Moisture_In_Park[is.na(df$Max_Moisture_In_Park)] = mean(df$Max_Moisture_In_Park[!is.na(df$Max_Moisture_In_Park)])
df$Max_Ambient_Pollution[is.na(df$Max_Ambient_Pollution)] = mean(df$Max_Ambient_Pollution[!is.na(df$Max_Ambient_Pollution)])

df$month = as.factor(substr(df$Date, 4, 5))
df$date_month = as.factor(substr(df$Date, 1, 5))
df$Date = as.Date(df$Date, format = "%d-%m-%Y")
df$weekday = as.factor(weekdays(df$Date))
df$Date = NULL
df$Location_Type = as.factor(df$Location_Type)

df$Park_ID = as.factor(df$Park_ID)

footfall_ave = aggregate(Footfall ~ Park_ID, data = train, FUN = mean)
colnames(footfall_ave) = c("Park_ID", "footfall_ave")
df = merge(df, footfall_ave, by = "Park_ID")

breeze_ave = aggregate(Average_Breeze_Speed ~ Park_ID, data = train, FUN = mean)
colnames(breeze_ave) = c("Park_ID", "breeze_ave")
df = merge(df, breeze_ave, by = "Park_ID", all.x = TRUE)

atmospheric_ave = aggregate(Average_Atmospheric_Pressure ~ Park_ID, data = train, FUN = mean)
colnames(atmospheric_ave) = c("Park_ID", "atmospheric_ave")
df = merge(df, atmospheric_ave, by = "Park_ID", all.x = TRUE)

moisture_ave = aggregate(Average_Moisture_In_Park ~ Park_ID, data = train, FUN = mean)
colnames(moisture_ave) = c("Park_ID", "moisture_ave")
df = merge(df, moisture_ave, by = "Park_ID", all.x = TRUE)

df$breeze_variance = df$Max_Breeze_Speed - df$Min_Breeze_Speed
df$atmospheric_variance = df$Max_Atmospheric_Pressure - df$Min_Atmospheric_Pressure
df$ambient_variance = df$Max_Ambient_Pollution - df$Min_Ambient_Pollution
df$moisture_variance = df$Max_Moisture_In_Park - df$Min_Moisture_In_Park

train_mod = subset(df, df$train == 1)
test_mod = subset(df, df$train == 0)
train_mod$train = NULL
test_mod$train = NULL

library(h2o)
h2o.init()
write.table(train_mod, gzfile('./train.csv.gz'),quote=F,sep=',',row.names=F)
write.table(test_mod, gzfile('./test.csv.gz'),quote=F,sep=',',row.names=F)
feature_names = names(train_mod)
feature_names = feature_names[! feature_names %in% c("ID", "Footfall")]
train_h2o = h2o.uploadFile("./train.csv.gz", destination_frame = "av_train")
test_h2o = h2o.uploadFile("./test.csv.gz", destination_frame = "av_test")
rf = h2o.randomForest(x = feature_names, y = "Footfall", training_frame = train_h2o, ntrees = 200)
res = predict(rf, test_h2o)
pred <- data.frame(ID = as.vector(test_mod$ID), Footfall = as.vector(res))
write.csv(pred, "submission_rf_h2o_2.csv", row.names = FALSE)


# library(caTools)
# set.seed(101) 
# 
# sample = sample.split(train_mod, SplitRatio = .75)
# train_dl = subset(train_mod, sample == TRUE)
# val_dl = subset(train_mod, sample == FALSE)
# 
# library(h2o)
# h2o.init()
# write.table(train_dl, gzfile('./train.csv.gz'),quote=F,sep=',',row.names=F)
# write.table(val_dl, gzfile('./val.csv.gz'),quote=F,sep=',',row.names=F)
# write.table(test_mod, gzfile('./test.csv.gz'),quote=F,sep=',',row.names=F)
# feature_names = names(train_dl)
# feature_names = feature_names[! feature_names %in% c("ID", "Footfall")]
# train_h2o = h2o.uploadFile("./train.csv.gz", destination_frame = "av_train")
# val_h2o = h2o.uploadFile("./val.csv.gz", destination_frame = "av_val")
# test_h2o = h2o.uploadFile("./test.csv.gz", destination_frame = "av_test")
# dlmodel <- h2o.deeplearning(x=feature_names, y="Footfall", training_frame = train_h2o, validation_frame = val_h2o, hidden=c(50,50), epochs = 10, activation="Rectifier", variable_importances = TRUE)
# res = predict(dlmodel, test_h2o)
# pred <- data.frame(ID = as.vector(test_mod$ID), Footfall = as.vector(res))
# write.csv(pred, "submission_dl.csv", row.names = FALSE)

# feature_names = names(train_dl)
# feature_names = feature_names[! feature_names %in% c("ID", "Footfall")]
# gbm <- h2o.gbm(x = feature_names, y = "Footfall", training_frame = train_h2o, ntrees = 200, max_depth = 30)
# res = predict(gbm, test_h2o)
# pred <- data.frame(ID = as.vector(test_mod$ID), Footfall = as.vector(res))
# write.csv(pred, "submission_gbm.csv", row.names = FALSE)
