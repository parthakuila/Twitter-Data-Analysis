library('twitteR')
library('ROAuth')
library('RCurl')
library('plyr')
library('dplyr')
library('lubridate')
#library('openssl')
#library('httpuv')
library('rtweet')
library('ggplot2')
library('stringr')
library('data.table')


#-------------------------------Authentication--------------------------

consumer_key <-"##########################"

consumer_secret <- "##########################"
access_token<-"##########################"
access_secret <- "##########################"

 setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )
 
cred <- OAuthFactory$new(consumerKey='##########################', consumerSecret='##########################',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")
#cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


#-------------------Get user profile from user name-----------------------


#username = 'mykelkuila'
#username = 'BarackObama'
username = 'ipl'

user <- getUser(username)
#@statusesCount
user$id
tweets_raw <- userTimeline(username, n = 1000, includeRts = FALSE, excludeReplies = TRUE)
dfT = twListToDF(tweets_raw)



#-----------------------When/What time of day does user commonly tweet? Frequency?----------------

ca = dfT$created
h <- data.frame(Dates = ca, Hour = format(ca, format = "%H"))
plot(h$Hour, main = "Distribution of tweets over hours of the day", sub = "Hourly distribution", xlab = "Hour", ylab = "No. of tweets", font=2, cex.lab = 1.5)

table_hour = sort(table(h$Hour), decreasing=TRUE)
percentage_table_hour = table_hour/sum(table_hour)*100
#t = as.numeric(as.character(dat$Day))
#names = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24')
#barplot(t, names.arg = names, main = "Distribution of tweets over hours of the day", sub = "Hourly distribution", xlab = "Hour", ylab = "No. of tweets", font=2)


#-----------------------Frequency?----------------------------------

d <- data.frame(Dates = ca, Day = format(ca, format = "%d"))

plot(d$Day, main = "Distribution of tweets over days of the month", xlab = "Day", ylab = "No. of tweets", font=2, cex.lab = 1.5)
table_day = sort(table(d$Day), decreasing=TRUE)
percentage_table_day = table_day/sum(table_day)*100

#---------------------Where is user tweeting from?----------------------

user$location
loc <- subset(dfT, select = c('latitude','longitude'))
print(paste0("User's location: ", user$location))


  
#-------------------How often does user link to external sites?-------------------


url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

URL <- str_extract(dfT$text, url_pattern)
URL = URL[!is.na(URL)]
l = length(URL)
t = length(dfT$text)
p = (l/t*100)
#print(paste0("Number of times user links to external sites: ", length(URL), ' (', p ,'%',')'))

# how many http links per tweet
#links_per_tweet = sapply(words_list, function(x) length(grep("http", x)))
#table(links_per_tweet)
#prop.table(table(links_per_tweet))


#--------------------What words/emoticons etc does the user commonly use?------------------


tweets = dfT$text  
words_list = strsplit(tweets, " ")
words = unlist(words_list)
mfw = sort(table(words), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot

par(mar=c(5,6,4,1)+.1)
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1.5, font=2, horiz=TRUE)



#---------------------Follower/friend ratio?--------------------


#friends <- user$getFriends()
friends = user$friendsCount
#followers <- user$getFollowers()
followers = user$followersCount
ratio = followers/friends


#------------------- how many @mentions per tweet-----------------------------

ats_per_tweet = sapply(words_list, function(x) grep("@", x))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))

vec = dfT$text
ats = function(vec){
 
ats_pattern = "@[[:alpha:]]+"
have.ats = grep(x = vec, pattern = ats_pattern)
 
ats.matches = gregexpr(pattern = ats_pattern, text = vec[have.ats])
extracted_ats = regmatches(x = vec[have.ats], m = ats.matches)
 
ret = data.frame(table(tolower(unlist(extracted_ats))))
colnames(ret) = c("ats","freq")
ret = ret[order(ret$freq,decreasing = TRUE),]
return(ret)
}
 
df = head(ats(vec),100)
df_ats = transform(df,ats = reorder(ats,freq))
 
plt = ggplot(df_ats, aes(x = ats, y = freq)) + geom_bar(stat="identity", fill = "black")
plt + coord_flip() + labs(title = "Frequency of @ Mentions in the tweets")

#how many @mentions per tweet?
# how many @mentions per tweet
#ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
#table(ats_per_tweet)
#prop.table(table(ats_per_tweet))


#-----------------------What are the user's commonly used hashtags?----------------

vec = dfT$text
hashtags = function(vec){
 
hash_pattern = "#[[:alpha:]]+"
have.hash = grep(x = vec, pattern = hash_pattern)
 
hash.matches = gregexpr(pattern = hash_pattern, text = vec[have.hash])
extracted_hashes = regmatches(x = vec[have.hash], m = hash.matches)
 
ret = data.frame(table(tolower(unlist(extracted_hashes))))
colnames(ret) = c("tag","freq")
ret = ret[order(ret$freq,decreasing = TRUE),]
return(ret)
}
 
dfh = head(hashtags(vec),100)
df_hash = transform(dfh,tag = reorder(tag,freq))
 
plt = ggplot(df_hash, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "black")
plt + coord_flip() + labs(title = "Hashtag frequencies in the tweets")

# how many hashtags per tweet
#hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
#table(hash_per_tweet)
#prop.table(table(hash_per_tweet))


#--------------------------------------------------------------------------------------------------


tweets <- 
  ldply(tweets_raw, function(x) {
    data_frame(id = x$id,
				date = as.Date(x$created),
               day = weekdays(date),
				favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text			   
    )
  })
  
 

#---------------------------------Results-------------------------------------------

sink("Results_BarackObama.txt", append=TRUE)
print(paste0("User name: ", as.character(user$screenName) ))
print(paste0("User ID: ", user$id))
print(paste0("Most popular hours of the day for tweeting: ", head(names(percentage_table_hour),3), '00 hrs: ', head(percentage_table_hour,3), '%')
print(paste0("Most popular day of the month for tweeting: ", head(names(percentage_table_day),3), 'th: ', head(percentage_table_day,3), '%' ))
print(paste0("User's location: ", user$location))
print(paste0("Number of times user links to external sites: ", length(URL), ' (', p ,'%',')'))
print(paste0("Top three most commonly used words: ", names(top20)[1], '(', top20[[1]], '), ', names(top20)[2], '(', top20[[2]], '), ',names(top20)[2], '(', top20[[2]], ')'))
print(paste0("No. of Followers: ", followers))
print(paste0("No. of Friends: ", friends))
print(paste0("Follower/friend ratio: ", ratio))
print(paste0("Top three most commonly used @ Mentions: ", head(df_ats$ats, 3), ': ', head(df_ats$freq, 3))
print(paste0("Top three most commonly used Hashtags: ", head(df_hash$tag, 3), ': ', head(df_hash$freq, 3))
sink()

#-------------------------------------------------------------------------------------
