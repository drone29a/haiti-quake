library(ggplot2)

#quakedata <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/quakedata.csv", header=T)
#hashtags <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/tweet.groups.csv", header=T)

hashtags <- read.csv("/Users/jharrison/classes/css692/project/code/haiti-quake/data/hashtags_over_time_factor_cumul.csv", header=T)

quartz()
top.hashtags <- subset(hashtags, hash.tag %in% c("haiti", "earthquake", "helphaiti", "cnn", "haitiquake"))
ggplot(top.hashtags, aes(x=time.interval)) + geom_freqpoly(aes(group=hash.tag, colour=hash.tag)) + opts(axis.text.x=theme_text(angle=-90, hjust=0, size=10))

ggplot.save()