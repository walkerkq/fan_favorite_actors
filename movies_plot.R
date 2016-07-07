setwd("/Users/kwalker/git_projects/fan_favorite_actors")
movies <- read.csv("movieInfo.csv", stringsAsFactors=FALSE)

# make numeric
for(h in c(5,10:17)) movies[,h] <- as.numeric(movies[,h])

# keep only movies
movies <- movies[movies$Type=="movie", ]

# keep only movies where the actor is first or second billed
keeps <- NULL
for(x in seq_along(movies$ID)){
     first <- strsplit(movies$Actors[x], ", ")[[1]][1]
     second <- strsplit(movies$Actors[x], ", ")[[1]][2]
     third <- strsplit(movies$Actors[x], ", ")[[1]][3]
     fourth <- strsplit(movies$Actors[x], ", ")[[1]][4]
     if(movies$Actor[x] %in% c(first, second, third, fourth)) {
          keeps <- c(keeps, x)
     }
}
movies2 <- movies[keeps, ]

# keep only movies with 1000+ user reviews
movies2 <- movies2[movies2$tomatoUserReviews > 1000 & movies2$Year < 2017, ]
movies2 <- movies2[!is.na(movies2$ID), ]

# keep rows with both values
movies2 <- movies2[!is.na(movies2$tomatoMeter) & !is.na(movies2$tomatoUserMeter), ]

# keep actors with at least 10 movies
threshold <- data.frame(table(movies2$Actor))
threshold <- threshold[threshold$Freq > 10, ]
movies2 <- movies2[movies2$Actor %in% threshold$Var1, ]

# get difference between user and critic score for each movie
movies2$Difference <- movies2$tomatoUserMeter - movies2$tomatoMeter

scoreMedians <- NULL
for(h in unique(movies2$Actor)){
     subset <- movies2[movies2$Actor==h, ]
     row <- data.frame(Actor=h, Number=nrow(subset), 
                       CriticMedian=median(subset$tomatoMeter), 
                       UserMedian=median(subset$tomatoUserMeter),
                       UserMinusCritic=(median(subset$tomatoUserMeter)-median(subset$tomatoMeter)),
                       DifferenceMean=round(mean(subset$Difference),2), 
                       DifferenceMedian=median(subset$Difference)
                       )
     scoreMedians <- rbind(scoreMedians, row)
}
scoreMedians$CalculatedCritic <- scoreMedians$UserMedian - scoreMedians$DifferenceMedian

######## GET TA PLOTTIN
library(ggplot2)
library(ggrepel)
library(gridExtra)

#####################
# movie scatter plot
fit <- lm(tomatoUserMeter ~ tomatoMeter, movies2) # R-sq is only 0.54, slope is 0.54
resid <- data.frame(Title=movies2$Title, tomatoMeter=movies2$tomatoMeter, Residual=fit$residuals)
resid <- resid[order(-resid$Residual), ]
resid <- resid[!duplicated(resid), ]
ggplot(movies2, aes(tomatoMeter, tomatoUserMeter)) + geom_point(color="#1f77b4", size=4, alpha=.5) + 
     theme_classic(base_size=16) + labs(title="Audience vs. Critic Scores, Movies") +
     xlab("Critic Score") + ylab("Audience Score") + 
     geom_abline(intercept=0, slope=1, linetype=2) + ylim(c(0,100)) +
     annotate("text", x=20, y=91, label="The Boondock Saints", vjust=-1) +
     annotate("text", x=34, y=87, label="I Am Sam", vjust=-1) +
     annotate("text", x=30, y=76, label="Hook", vjust=-1) +
     annotate("text", x=5, y=70, label="Audience Favorites", size=6, hjust=0) +
     annotate("text", x=90, y=99, label="Finding Nemo", vjust=1) +
     annotate("text", x=3, y=16, label="Speed 2", vjust=1) +
     annotate("text", x=63, y=29, label="Edtv", vjust=1) +
     annotate("text", x=79, y=43, label="The Informant!", vjust=1) +
     annotate("text", x=95, y=10, label="Critic Favorites", size=6, hjust=1) 

#####################
# distribution of differences
qqnorm(movies2$Difference) # very very normal distribution
diffMean <- mean(movies2$Difference)
diffSD <- sd(movies2$Difference)
ggplot(movies2, aes(Difference)) + geom_histogram(bins=25, fill="cadetblue") + 
     geom_vline(xintercept=mean(movies2$Difference)) +
     geom_vline(xintercept=diffMean-diffSD, linetype=2) + 
     annotate("text", x=30, y=100, label=paste("Mean: ", round(diffMean), "; ", "\nStd. Dev.: ", round(diffSD), sep=""), hjust=0) +
     geom_vline(xintercept=diffMean+diffSD, linetype=2) +
     theme_classic() + xlab("Audience Score Minus Critic Score") + ylab("Count") + 
     labs(title="Distribution of Differences\nBetween Audience and Critic Scores")

#####################
# most over/underrated
for(b in seq_along(scoreMedians$Actor)){
     if(scoreMedians$DifferenceMedian[b] > 2){
          scoreMedians$Group[b] <- "Audiences Prefer"
     } else if(scoreMedians$DifferenceMedian[b] < -2){
          scoreMedians$Group[b] <- "Critics Prefer"
     } else {
          scoreMedians$Group[b] <- "Agree"
     }
}
scoreMedians <- scoreMedians[order(-scoreMedians$DifferenceMedian), ]
scoreMedians$Actor <- factor(scoreMedians$Actor, levels=scoreMedians$Actor)
over <- ggplot(tail(scoreMedians,5), aes(Actor, DifferenceMedian)) + 
    geom_bar(stat="identity", fill="#b2df8a") +
     coord_flip() + theme_classic(base_size=14) + theme(plot.title=element_text(size=16)) +
     xlab("") + labs(title="Most Preferred by Critics") + 
     ylab("Median Difference Between Audience and Critic Scores") + scale_y_reverse()

scoreMedians <- scoreMedians[order(scoreMedians$DifferenceMedian), ]
scoreMedians$Actor <- factor(scoreMedians$Actor, levels=scoreMedians$Actor)
under <- ggplot(tail(scoreMedians,5), aes(Actor, DifferenceMedian)) + 
    geom_bar(stat="identity", fill="#1f77b4")  +
    coord_flip() + theme_classic(base_size=14) + theme(plot.title=element_text(size=16)) +
    xlab("") + labs(title="Most Preferred by Audiences") + 
    ylab("Median Difference Between Audience and Critic Scores") + scale_y_reverse()
grid.arrange(under, over, ncol=2)


#####################
# scatterplot actors
myColors <- c("#a6cee3", "#1f77b4", "#b2df8a")
ggplot(scoreMedians, aes(CalculatedCritic, UserMedian)) + geom_point(aes(color=Group), size=4) + 
    theme_classic(base_size=16) + scale_color_manual(values=myColors) + scale_fill_manual(values=myColors) +
    geom_abline(intercept=0, slope=1, linetype=2, color="gray50") +
     geom_label_repel(data=subset(scoreMedians, DifferenceMedian > 7 | DifferenceMedian <= -5 | UserMedian > 70 | UserMedian < 50), 
                      aes(label=Actor, fill=Group), size=3.5, color="white", fontface="bold", 
                      label.padding=unit(0.15, "lines"),
                      label.size=0.1,
                      point.padding=unit(0.1, "lines")) +
     xlim(c(15,100)) + ylim(c(40,85)) + 
     ylab("Audience Score") + xlab("Critic Score") + labs(title="Audience vs. Critic Scores, Actors") + 
     theme(legend.position="none")

#####################
# look at individual actors
actor_plot <- function(actor) { 
     movieplot <- movies2[movies2$Actor==actor, ]
     movieplot <- movieplot[ ,c(4,5,13,15,20)]
     movieplot <- movieplot[order(-movieplot$Year),]
     movieplot$Title <- sapply(movieplot$Title, function(x) strsplit(x, ": ")[[1]][1])
     movieplot$Review <- paste(movieplot$Title, " (", movieplot$Year, ")", sep="")
     movieplot$Review <- factor(movieplot$Review, levels=movieplot$Review)

     ggplot(movieplot, aes(color=Review)) + theme_classic(base_size=14) + scale_color_brewer(palette="Paired") +
          geom_segment(aes(x=tomatoMeter, xend=tomatoUserMeter, y=Review, yend=Review), color="#20687d", size=1) +
          geom_point(aes(x=tomatoMeter, y=Review, color="Critic"), size=4, shape=15) +
          geom_point(aes(x=tomatoUserMeter, y=Review, color="Audience"), size=4, shape=15) +
          xlab("Score") + ylab("") + labs(title=paste("Movies Starring", actor, sep=" ")) +
          xlim(c(0,100)) 
     
}
actor_plot("David Spade")
actor_plot("Adam Sandler")
actor_plot("Gerard Butler")
actor_plot("Ryan Reynolds")
actor_plot("Kate Hudson")

actor_plot("Hugo Weaving")
actor_plot("Seth Rogen")
actor_plot("Jack Nicholson")
actor_plot("Clint Eastwood")
actor_plot("Anna Kendrick")


#####################
##### which actors had a lot of small roles not included?
compare <- data.frame(one=table(movies$Actor[movies$Actor %in% unique(movies2$Actor)]), two=table(movies2$Actor))
compare$Diff <- compare$one.Freq - compare$two.Freq
compare <- merge(scoreMedians, compare[,c(1,2,5)], by.x="Actor", by.y="one.Var1")
compare$NonLeading <- round(compare$Diff/compare$one.Freq, 2)
compare$Leading <- 1-compare$NonLeading
compare <- compare[order(compare$NonLeading), ]
compare$Actor <- factor(compare$Actor, levels=compare$Actor)
comparemelt <- melt(compare[,c(1,12,13)], id="Actor")
colnames(comparemelt) <- c("Actor", "Role", "Percent")
ggplot(comparemelt, aes(Actor, Percent)) + geom_bar(stat="identity", aes(fill=Role)) + 
     coord_flip() + theme_classic(base_size=14) + ylab("") + xlab("") + 
     labs(title="Breakdown of Non-Leading vs. Leading Roles") + ylim(c(0,1)) + 
     scale_fill_brewer(palette="Paired")


