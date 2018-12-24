#-Get user profile from user name-

#username = 'asmita_poddar'
username = 'BarackObama'

user <- getUser(username)
#@statusesCount
user$id
tweets_raw <- userTimeline(username, n = 1000, includeRts = FALSE, excludeReplies = TRUE)
dfT = twListToDF(tweets_raw)