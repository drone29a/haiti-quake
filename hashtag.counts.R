library(ggplot2)

hashtags <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/hash.tag.group.counts.csv", header=T)
#hashtags <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/hash.tag.counts.csv", header=T)

hashtags <- hashtags[1:50,]
#ht <- data.frame(hash.tag = factor(hashtags$hash.tag, levels = hashtags$hash.tag), count = hashtags$count)
ht <- data.frame(hash.tag = factor(hashtags$hash.tag, levels = hashtags$hash.tag), count = hashtags$mention.count)
#ht <- data.frame(hash.tag = hashtags$hash.tag, count = hashtags$count)

#quartz()

p <- ggplot(ht, aes(hash.tag, count)) + 
	geom_bar() + 
	xlab("Hashtag") + 
	ylab("Number of Occurences") +
	opts(axis.text.x=theme_text(angle=-60, hjust=0, size=8)) +
	opts(title.text="Top 50 Hashtags")

print(p)



ggsave(file = "~/classes/css692/project/code/haiti-quake/figures/hashtag_counts.pdf")

#
#hashtags <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/hashtags_over_short_time.csv", header=T)
#temp <- lapply(hashtags$time.interval, function (x) {paste(unlist(strsplit(as.character(x),":"))[1:2], collapse=":")})
#hashtags$hhmm <- unlist(temp)
#
##ht <- data.frame(hash.tag = hashtags$hash.tag, time.interval = hashtags$time.interval)
## top 10 retweeted tweets in facet grid, add follower horizontal line for each?  could fill with day/night colors to show time?
#p <- ggplot(hashtags, aes(x = hhmm)) + 
#	geom_bar() + 
#	facet_wrap(~ hash.tag, ncol = 2, scales="free_y") + 
#	labs(x="Time (in 15-Minute Intervals)", y="Number of Mentions") + 
#	opts(title.text = "Top 10 Hashtags") +
#	opts(axis.text.x=theme_text(angle=-60, hjust=0, size=4))
#print(p)
#ggsave(file = "~/classes/css692/project/code/haiti-quake/figures/top-20-hashtags.pdf")
#
#p <- ggplot(hashtags, aes(x = hhmm)) + 
#	geom_bar() + 
#	facet_wrap(~ hash.tag, ncol = 2) + 
#	labs(x="Time (in 15-Minute Intervals)", y="Number of Mentions") + 
#	opts(title.text = "Top 10 Hashtags") +
#	opts(axis.text.x=theme_text(angle=-60, hjust=0, size=4))
#print(p)
#ggsave(file = "~/classes/css692/project/code/haiti-quake/figures/top-20-hashtags-same-scale.pdf")
#
# OR,
# ggplot(retweets.top.10, aes(x = time.interval)) + geom_bar() + facet_wrap(~ category3, ncol = 3) + labs(x="Time (in 15-Minute Intervals)", y="Number of Retweets") + opts(title.text = "Top 10 Retweeted Posts") + opts(axis.text.x = theme_blank(), axis.ticks = theme_blank())