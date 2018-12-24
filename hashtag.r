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
