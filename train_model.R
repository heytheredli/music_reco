setwd("/Users/DanielLi/Documents/Kaggle/music_reco")

library(data.table)
library(xgboost)
library(Matrix)

members = fread("members.csv")
songs = fread("songs.csv")
song_extra_info = fread("song_extra_info.csv")
train_data = fread("train.csv")
test_data = fread("test.csv")
#submission = fread("sample_submission.csv")

setkey(songs, song_id)
setkey(members, msno)
setkey(song_extra_info, song_id)

# Left join song data to train
setkey(train_data, song_id)
setkey(test_data, song_id)
train_data = merge(train_data, songs, all.x=TRUE)
test_data = merge(test_data, songs, all.x=TRUE)

# Left join song_extra_info to train
setkey(train_data, song_id)
setkey(test_data, song_id)
train_data = merge(train_data, song_extra_info, all.x=TRUE)
test_data = merge(test_data, song_extra_info, all.x=TRUE)

# Left join members data to train
setkey(train_data, msno)
setkey(test_data, msno)
train_data = merge(train_data, members, all.x=TRUE)
test_data = merge(test_data, members, all.x=TRUE)

train_data[,msno:=NULL]
train_data[,song_id:=NULL]
test_data[,msno:=NULL]
test_data[,song_id:=NULL]

# splitting registration date into year, month, and day
train_data$registration_init_year = floor(train_data$registration_init_time/10000)
train_data$registration_init_month = floor((train_data$registration_init_time - train_data$registration_init_year * 10000)/100)
train_data$registration_init_day = train_data$registration_init_time - (train_data$registration_init_year*10000 + train_data$registration_init_month * 100)
train_data[,registration_init_time:=NULL]
test_data$registration_init_year = floor(test_data$registration_init_time/10000)
test_data$registration_init_month = floor((test_data$registration_init_time - test_data$registration_init_year * 10000)/100)
test_data$registration_init_day = test_data$registration_init_time - (test_data$registration_init_year*10000 + test_data$registration_init_month * 100)
test_data[,registration_init_time:=NULL]

# splitting expiration date into year, month, and day
train_data$expiration_year = floor(train_data$expiration_date/10000)
train_data$expiration_month = floor((train_data$expiration_date - train_data$expiration_year * 10000)/100)
train_data$expiration_day = train_data$expiration_date - (train_data$expiration_year*10000 + train_data$expiration_month * 100)
train_data[,expiration_date:=NULL]
test_data$expiration_year = floor(test_data$expiration_date/10000)
test_data$expiration_month = floor((test_data$expiration_date - test_data$expiration_year * 10000)/100)
test_data$expiration_day = test_data$expiration_date - (test_data$expiration_year*10000 + test_data$expiration_month * 100)
test_data[,expiration_date:=NULL]


# if artist name only has alphanumeric, artist_name_eng = 1, else 0
train_data[,artist_name_eng := ifelse(grepl('^[A-Za-z0-9]+$', train_data$artist_name), 1, 0)]
test_data[,artist_name_eng := ifelse(grepl('^[A-Za-z0-9]+$', test_data$artist_name), 1, 0)]
sum(train_data$artist_name_eng)
train_data[,artist_name:=NULL]
test_data[,artist_name:=NULL]

# if composer only has alphanumeric, composer_eng = 1, else 0
train_data[,composer_eng := ifelse(grepl('^[A-Za-z0-9]+$', train_data$composer), 1, 0)]
test_data[,composer_eng := ifelse(grepl('^[A-Za-z0-9]+$', test_data$composer), 1, 0)]
sum(train_data$composer_eng)
train_data[,composer:=NULL]
test_data[,composer:=NULL]

# if lyricist only has alphanumeric, lyricist_eng = 1, else 0
train_data[,lyricist_eng := ifelse(grepl('^[A-Za-z0-9]+$', train_data$lyricist), 1, 0)]
test_data[,lyricist_eng := ifelse(grepl('^[A-Za-z0-9]+$', test_data$lyricist), 1, 0)]
sum(train_data$lyricist_eng)
train_data[,lyricist:=NULL]
test_data[,lyricist:=NULL]


# if song name only has alphanumeric, name_eng = 1, else 0
train_data[,name_eng := ifelse(grepl('^[A-Za-z0-9]+$', train_data$name), 1, 0)]
test_data[,name_eng := ifelse(grepl('^[A-Za-z0-9]+$', test_data$name), 1, 0)]
sum(train_data$name_eng)
train_data[,name:=NULL]
test_data[,name:=NULL]

# isrc
train_data[,isrc:=NULL]
test_data[,isrc:=NULL]

# Genre Ids
train_data[,genre_ids:=NULL]
test_data[,genre_ids:=NULL]

# target
target = train_data$target
train_data[,target:=NULL]

sparse_matrix = sparse.model.matrix(target~., data=train_data)
bst <- xgboost(data = sparse_matrix, label = target, max_depth = 9,
               eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")

# Predictions on Test dataset
test_pred = predict(bst, test_data)
print(head(test_pred))
test_data[,target := test_pred]

# Variable importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
print(importance)

#save model
xgb.save(bst, "xgboost.model")
write.table

head(temp$genre_ids)
colnames(temp)
str(temp)
unique(train_data[,source_type])


#tbd
#genre_ids - nulled
#artist_name
#composer
#lyricist
#name
#isrc - nulled

#good to be one-hot encoded:
#source_system_tab
#source_screen_name
#source_type
#gender

#leave as int:
#song_length
#language
#city
#bd
#registered_via



