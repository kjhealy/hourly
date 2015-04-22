###--------------------------------------------------
### Hour-by-hour routines of history's most famous people
###--------------------------------------------------


### Setup
library(ggplot2)

cb.palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_minimal())


### "Data"
famous.people <- read.csv("data/famous-people.csv", header=TRUE)
categories <- factor(c("Sleep", "Creative Work", "Day Job/Admin",
                       "Food/Leisure", "Exercise", "Other"),
                     ordered=TRUE)
hrs <- c(1:24)
hrs.lab <- c(paste(c(12, 1:11), "am", sep=""), paste(c(12, 1:11), "pm", sep=""))

### Generate daily routines of famous people
make.my.day <- function(person){
    ind <- sample(seq(from = 1, to = length(categories),
                      by = sample(1:5, size=1, replace=TRUE)), size = 24, replace = TRUE)
    obs <- data.frame(Person=person, Activity=categories[ind], Hour=hrs)
    return(obs)
}

out <- sapply(sample(famous.people$Name, 10, replace=TRUE), make.my.day, simplify=FALSE)
days.df <- do.call(rbind, out)

### Make a plot
## doing the scale this way is morally wrong, but I'm in a hurry
tps <- round(seq(0, 300, length.out=24),0)

p <- ggplot(days.df, aes(x=Person, y=Hour, fill=Activity))
p + geom_bar(stat="identity") + coord_flip() + labs(x="", y="") +
    scale_fill_manual(values = cb.palette) +
        scale_y_continuous(breaks=tps, labels=hrs.lab) +
            theme(legend.position="top",
                  axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
                ggtitle("Hour by Hour Routines of History's Most Creative People")
