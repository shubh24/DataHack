train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

train$train = 1
test$train = 0
test$Footfall = -1

df = rbind(train, test)

df$Direction_Of_Wind[is.na(df$Direction_Of_Wind)] = mean(df$Direction_Of_Wind[!is.na(df$Direction_Of_Wind)])
#put in quadrants as factors - failed

# df$Max_Breeze_Speed = NULL
# df$Min_Breeze_Speed = NULL
# df$Max_Atmospheric_Pressure = NULL
# df$Min_Atmospheric_Pressure = NULL
# df$Max_Ambient_Pollution = NULL
# df$Min_Ambient_Pollution = NULL
#df$Max_Moisture_In_Park = NULL
#df$Min_Moisture_In_Park = NULL

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
df$Date = as.Date(df$Date, format = "%d-%m-%Y")
df$weekday = as.factor(weekdays(df$Date))
df$Date = NULL
df$Location_Type = as.factor(df$Location_Type)
df$Park_ID = as.factor(df$Park_ID)

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
