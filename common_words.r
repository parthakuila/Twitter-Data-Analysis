tweets = dfT$text  
words_list = strsplit(tweets, " ")
words = unlist(words_list)
mfw = sort(table(words), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot

par(mar=c(5,6,4,1)+.1)
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1.5, font=2, horiz=TRUE)