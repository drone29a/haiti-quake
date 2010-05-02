library(ggplot2)

hashtags <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/hash.tag.counts.csv", header=T)

ht <- data.frame(Hashtags = factor(hashtags$hash.tag, levels = hashtags$hash.tag), Counts = hashtags$count)

quartz()
ggplot(ht, aes(Hashtags, Counts)) + geom_bar() + xlab("Hashtag") + ylab("Number of Occurances") + opts(axis.text.x=theme_text(angle=-90, hjust=0, size=10))


ggsave(file = "hashtag_counts.pdf")