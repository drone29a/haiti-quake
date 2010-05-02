createRetweetFrame <- function(quake.data, retweeted.ids) {

  retweets <- subset(quake.data, retweeted_status.id %in% retweet.tweets, select = c(id, user.id, user.name, user.screen_name, retweeted_status.id, retweeted_status.user.id, retweeted_status.user.name, retweeted_status.user.screen_name, interval))

  names(retweets) <- c("tweet.id", "retweeter.id", "retweeter.name", "retweeter.screenname", "original.tweet.id", "original.author.id", "original.author.name", "original.author.screenname", "time.interval")

  retweets$original.author.id <- as.factor(retweets$original.author.id)
  retweets$original.tweet.id <- as.factor(retweets$original.tweet.id)

 return(retweets)
}

buildEdgeList <- function(retweets) {
  edges <- data.frame(from=NA, to=NA)
  all.edges.vec <- vector()
  for (curr.retweeter.id in sort(unique(retweets$retweeter.id))) {
#    tweet.ids <- sort(retweets$original.tweet.id[retweets$retweeter.id == retweeter.id])
    tweet.ids <- sort(subset(retweets, retweeter.id == curr.retweeter.id, select = original.tweet.id)$original.tweet.id)
    tweet.ids <- array(tweet.ids)
    if (length(tweet.ids) > 1) {
      comb.ids <- combn(tweet.ids, 2)
      comb.ids <- data.frame(from = comb.ids[1,], to = comb.ids[2,])
      edge.names <- lapply(1:length(comb.ids$from), function(x) { paste(comb.ids$from[x], comb.ids$to[x], sep=",")})

      all.edges.vec <- append(all.edges.vec, unlist(edge.names))
    
      ## for (col.idx in 1:length(comb.ids[1,])) {
      ##   edge.name <- paste(comb.ids[,col.idx], collapse = ",")
      ## }
    }
  }
  all.edges.mat <- matrix(unlist(strsplit(all.edges.vec, ",")), ncol=2, byrow=TRUE)

  # There will be some duplicates, that means multiple common retweeters and should be summed
  # for magnitude
  all.edges <- data.frame(from = all.edges.mat[col(all.edges.mat) == 1],
                          to = all.edges.mat[col(all.edges.mat) == 2])

  agg.edges <- aggregate(all.edges, by = list(all.edges$from, all.edges$to),
                         FUN = length, simplify = TRUE)

  agg.edges <- data.frame(from = agg.edges$Group.1,
                          to = agg.edges$Group.2,
                          weight = agg.edges$from)

  agg.edges <- agg.edges[with(agg.edges, order(-weight, from)),]
  
  return(agg.edges)
}

writeToGdfFile <- function(file.path, vertices, edges) {
  gdf.file <- file(file.path, "w")

  cat("nodedef> ", paste(names(vertices), collapse = ","), "\n", sep = "", file = gdf.file)
#  write.table(vertices, sep = ",", col.names = FALSE, row.names = FALSE,
#              quote = TRUE, file = gdf.file)
  for (i in 1:length(vertices[,1])) {
    for (j in 1:length(names(vertices))) {
      if (j == 1) {
        cat(as.character(vertices[i,j]), file = gdf.file)
      }
      else if (j >= length(names(vertices)) - 2) {
        cat("'", as.character(vertices[i,j]), "'", sep = "", file = gdf.file)
      } else {
        cat(as.numeric(vertices[i,j]), file = gdf.file)
      }

      if (j < length(names(vertices))) {
        cat(",", file = gdf.file)
      }
    }
    cat("\n", file = gdf.file)
  }

  cat("edgedef> ", paste(names(edges), collapse = ","), "\n", sep = "", file = gdf.file)
  for (i in 1:length(edges[,1])) {
    for (j in 1:length(names(edges))) {
      if (j == 1 || j == 2) {
        cat(as.character(edges[i,j]), file = gdf.file)
      } else {
        cat(as.numeric(edges[i,j]), file = gdf.file)
      }

      if (j < length(names(edges))) {
        cat(",", file = gdf.file)
      }
    }
    cat("\n", file = gdf.file)
  }

#  write.table(edges, sep = ",", col.names = FALSE, row.names = FALSE,
#              quote = TRUE, file = gdf.file)
  
  close(gdf.file)
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

# Build graph, convert data frame of edges to matrix to get edgelist
# g <- graph.edgelist(as.matrix(edge.list), directed=FALSE)

# duplicates due to non-unique agg.retweets.ids and agg.retweets.ids.sorted
# OR, maybe factor issues?  Think it's resolved now.

# What's up with 7 missing in new retweets.edges compared to edge.list?
# And why does length(unique(edge.list)$from) report much fewer?

# Cleanup $text with:
# gsub("[\n\t]", " ", retweets.verts$text)
# had to run twice, why?

# get $text for vertices
## for (i in l) {
##   text <- retweets.texts$retweeted_status.text[i]
##   tweet.id <- retweets.texts$retweeted_status.id[i]
##   retweets.verts$text[retweets.verts$name == tweet.id] <- text
## }

# dividing up combined edge names, no longer needed with aggregate fun
## for (i in 1:length(from.to)) {
##   from[i] <- from.to[[i]][1]
##   to[i] <- from.to[[i]][2]
## }


# Removed two rows from quake.data
# texts:
# Raw Footage Of Earthquake In Haiti 1/12/2010 http://hotncatty.com/2010/01/12/raw-footage-of-earthquake-in-haiti-1122010/
#
#RT @coldplay: "The people of Haiti will be desperate for help" - message from Chris about the Haiti earthquake http://bit.ly/8bCVea

# rows: 11005, 63174


# new graphs:

# all retweets:
# ggplot(retweets, aes(x=time.interval)) + geom_histogram() + opts(axis.text.x=theme_text(angle=-60, hjust=0, size=8))

# all tweets compared to retweets:
# ggplot(quake.data, aes(interval, fill=tweet.type)) + geom_freqpoly(aes(group = tweet.type, colour = tweet.type)) + opts(axis.text.x=theme_text(angle=-60, hjust=0, size = 6))

# top 50 retweeted tweets:
# ggplot(retweets.group[1:50,], aes(x=group.id, y=retweet.counts, label=original.author.name, fill=retweet.counts)) + geom_bar(stat="identity") + geom_text(angle=70, hjust=-0.1, size = 3) + ylim(0, 900) + labs(x="Popular Tweets", y="# Retweets") + opts(axis.text.x = theme_blank(), axis.ticks = theme_blank())

# top 50 retweeted tweets, the counts and the number of followers
