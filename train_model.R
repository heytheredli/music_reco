setwd("/Users/DanielLi/Documents/Kaggle/music_reco")

library(data.table)
library(xgboost)
library(Matrix)

members = fread("members.csv")
songs = fread("songs.csv")
song_extra_info = fread("song_extra_info.csv")
train_data = fread("train.csv")
test_data = fread("test.csv")

setkey(songs, song_id)
setkey(members, msno)
setkey(song_extra_info, song_id)

# Left join song data to train
setkey(train_data, song_id)
train_data = merge(train_data, songs, all.x=TRUE)

# Left join song_extra_info to train
setkey(train_data, song_id)
train_data = merge(train_data, song_extra_info, all.x=TRUE)

# Left join members data to train
setkey(train_data, msno)
train_data = merge(train_data, members, all.x=TRUE)

temp = head(train_data, n=1000)

temp[,msno:=NULL]
temp[,song_id:=NULL]

target = temp$target
temp[,target:=NULL]

# splitting registration date into year, month, and day
temp$registration_init_year = floor(temp$registration_init_time/10000)
temp$registration_init_month = floor((temp$registration_init_time - temp$registration_init_year * 10000)/100)
temp$registration_init_day = temp$registration_init_time - (temp$registration_init_year*10000 + temp$registration_init_month * 100)
temp[,registration_init_time:=NULL]

# splitting expiration date into year, month, and day
temp$expiration_year = floor(temp$expiration_date/10000)
temp$expiration_month = floor((temp$expiration_date - temp$expiration_year * 10000)/100)
temp$expiration_day = temp$expiration_date - (temp$expiration_year*10000 + temp$expiration_month * 100)
temp[,expiration_date:=NULL]

# if artist name only has alphanumeric, artist_name_eng = 1, else 0
temp[,artist_name_eng := ifelse(grepl('^[A-Za-z0-9]+$', temp$artist_name), 1, 0)]
sum(temp$artist_name_eng)

# if composer only has alphanumeric, composer_eng = 1, else 0
temp[,composer_eng := ifelse(grepl('^[A-Za-z0-9]+$', temp$composer), 1, 0)]
sum(temp$composer_eng)

# if lyricist only has alphanumeric, lyricist_eng = 1, else 0
temp[,lyricist_eng := ifelse(grepl('^[A-Za-z0-9]+$', temp$lyricist), 1, 0)]
sum(temp$lyricist_eng)

# if song name only has alphanumeric, name_eng = 1, else 0
temp[,name_eng := ifelse(grepl('^[A-Za-z0-9]+$', temp$name), 1, 0)]
sum(temp$name_eng)

temp[,artist_name:=NULL]
temp[,composer:=NULL]
temp[,lyricist:=NULL]
temp[,name:=NULL]
temp[,isrc:=NULL]

#tbd
genre_ids
#artist_name
#composer
#lyricist
#name
#isrc - nulled

#good to be one-hot encoded:
source_system_tab
source_screen_name
source_type
gender

#leave as int:
song_length
language
city
bd
registered_via

head(train_data)
colnames(temp)
str(temp)
unique(train_data[,registered_via])


