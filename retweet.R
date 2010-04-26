createRetweetFrame <- function(quake.data, retweeted.ids) {

  retweets <- subset(quake.data, retweeted_status.id %in% retweet.tweets, select = c(id, user.id, user.name, user.screen_name, retweeted_status.id, retweeted_status.user.id, retweeted_status.user.name, retweeted_status.user.screen_name, interval))

  names(retweets) <- c("tweet.id", "retweeter.id", "retweeter.name", "retweeter.screenname", "original.tweet.id", "original.author.id", "original.author.name", "original.author.screenname", "time.interval")

  retweets$original.author.id <- as.factor(retweets$original.author.id)
  retweets$original.tweet.id <- as.factor(retweets$original.tweet.id)

 return(retweets)
}

buildEdgeList <- function(retweets) {
  edges <- data.frame(from=NA, to=NA)
  all.edges <- vector()
  for (retweeter.id in retweets$retweeter.id) {
    tweet.ids <- sort(retweets$original.tweet.id[retweets$retweeter.id == retweeter.id])
    if (length(tweet.ids) > 1) {
      comb.ids <- combn(tweet.ids, 2)
      comb.ids <- data.frame(from = comb.ids[1,], to = comb.ids[2,])
      edge.names <- lapply(1:length(comb.ids$from), function(x) { paste(comb.ids$from[x], comb.ids$to[x], sep=",")})

      all.edges <- append(all.edges, unlist(edge.names))
    
      ## for (col.idx in 1:length(comb.ids[1,])) {
      ##   edge.name <- paste(comb.ids[,col.idx], collapse = ",")
      ## }
    }
  }
  return(all.edges)
}

# author.screennames <- lapply(retweets.group$original.tweet.id, function(x) { retweets$original.author.screenname[retweets$original.tweet.id == x][1] })

# author.names <- lapply(retweets.group$original.tweet.id, function(x) { retweets$original.author.name[retweets$original.tweet.id == x][1] })
# retweets.group$original.author.name <- unlist(author.names)

# agg <- aggregate(retweets, list(retweets$original.tweet.id), length)
# then rename and becomes retweets.counts and after subset, rewteets.group
# retweets.group <- subset(agg, select = c(Group.1, retweeter.id))
# retweets.group <- retweets.group[order(-retweets.group$retweet.counts)]

# Plot it!  retweets ordered by count
# ggplot(retweets.group[1:100,], aes(x=group.id, y=retweet.counts)) + geom_bar(stat="identity")

# Retweets by interval timeline
# ggplot(retweets, aes(x=time.interval)) + geom_histogram() + opts(axis.text.x=theme_text(angle=-90, hjust=0))

# Top 50 retweeted tweets with text annotation specifying author
# ggplot(retweets.group[1:50,], aes(x=group.id, y=retweet.counts, label=original.author.name)) + geom_bar(stat="identity") + geom_text(angle=-90, hjust=1.3) + ylim(0, 900) + labs(x="Popular Tweets", y="# Retweets") + opts(axis.text.x = theme_blank(), axis.ticks = theme_blank())

# In progress method for stacked bar chart of top 5 tweets over time
# ggplot(retweets, aes(x = time.interval, fill = original.tweet.id)) + geom_bar(aes(weight = 
#
