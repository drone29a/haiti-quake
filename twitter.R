library(rjson)

quake.lines <- readLines("/Users/matt/src/haiti-quake/infochimps_dataset_12681_download_16140/earthquake_fixed")

# quake.frame <- data.frame(lapply(quake.lines, fromJSON))


l <- lapply(quake.lines, fromJSON)
quake.data <- data.frame(do.call(rbind, lapply(lapply(l, unlist), "[", unique(unlist(c(sapply(lapply(l, unlist), names)))))))

write.table(quake.frame, "/Users/matt/src/haiti-quake/quaketable.csv")

trunc.minutes <- function (x, n.minutes) 
{
    if (!inherits(x, "times")) 
        x <- as.chron(x)
    x <- as.numeric(x)
    sec <- round(24 * 3600 * abs(x - floor(x)))
    h <- (sec%/%(n.minutes*60))/(60/n.minutes)
    hour <- as.integer(h)
    minutes <- (h %% hour) * 60
    chron(dates=chron::dates(x), times=times(paste(hour, minutes, "00", sep=":")))
}

parseDates <-  function(x) {
  l <- list(length(x))
  for (i in 1:length(x)) {
    l[[i]] <- strptime(x[[i]], format="%a %b %d %H:%M:%S +0000 %Y", tz="UTC")
  }
  return(l)l
}

# Take a vector of timestamps and assign intervals based on value.
# Period is in seconds.
#
# Returns a vector of the start time of the interval containing
# the timestamp is contained.
interv2 <- function(x, start, period=1800) {
  x.intervals <- list(length(x))
  end = start + period
  for (i in 1:length(x)) {
    if (is.na(x[[i]])) {
      x.intervals[[i]] <- start
    } else {
      while (x[[i]] >= end) {
        start <- end
        end <- start + period
      }
      x.intervals[[i]] <- start
    }
  }

  return(x.intervals)
}

interv <- function(x, start, period, num.intervals) {
  return(cut(x, as.POSIXlt(start)+0:num.intervals*period))
}


quake.agg <- aggregate(ones, list(quake.dates.intervals), sum)

qa <- ggplot(quake.data, aes(x=interval)) + geom_histogram(binwidth = 1, aes(fill = ..count..)) + opts(axis.text.x=theme_text(angle=-90, hjust=0))
