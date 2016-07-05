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
     if(movies$Actor[x] %in% c(first,second)) {
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
threshold <- threshold[threshold$Freq > 5, ]
movies2 <- movies2[movies2$Actor %in% threshold$Var1, ]

# get difference between user and critic score for each movie
movies2$Difference <- movies2$tomatoUserMeter - movies2$tomatoMeter

######## GET TA PLOTTIN
library(ggplot2)
library(gridExtra)

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

#####################
# movie scatter plot
fit <- lm(tomatoUserMeter ~ tomatoMeter, movies2) # R-sq is only 0.54, slope is 0.54
resid <- data.frame(Title=movies2$Title, tomatoMeter=movies2$tomatoMeter, Residual=fit$residuals)
resid <- resid[order(-resid$Residual), ]
resid <- resid[!duplicated(resid), ]
ggplot(movies2, aes(tomatoMeter, tomatoUserMeter)) + geom_point(color="cadetblue", size=4, alpha=.5) + 
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
     if(scoreMedians$DifferenceMedian[b] > 3){
          scoreMedians$Group[b] <- "Audiences Prefer"
     } else if(scoreMedians$DifferenceMedian[b] < -3){
          scoreMedians$Group[b] <- "Critics Prefer"
     } else {
          scoreMedians$Group[b] <- "Agree"
     }
}
scoreMedians <- scoreMedians[order(-scoreMedians$DifferenceMedian), ]
scoreMedians$Actor <- factor(scoreMedians$Actor, levels=scoreMedians$Actor)
over <- ggplot(scoreMedians[45:54, ], aes(Actor, DifferenceMedian)) + 
    geom_bar(stat="identity", fill="cadetblue") + 
     coord_flip() + theme_classic() + theme(plot.title=element_text(size=16)) +
     xlab("") + labs(title="Top 10 Most Overrated Actors") + 
     ylab("Median Difference Between Audience and Critic Scores") 

scoreMedians <- scoreMedians[order(scoreMedians$DifferenceMedian), ]
scoreMedians$Actor <- factor(scoreMedians$Actor, levels=scoreMedians$Actor)
under <- ggplot(scoreMedians[45:54, ], aes(Actor, DifferenceMedian)) + 
    geom_bar(stat="identity", fill="cadetblue") + 
    coord_flip() + theme_classic() + theme(plot.title=element_text(size=16)) +
    xlab("") + labs(title=" Top 10 Most Underrated Actors") + 
    ylab("Median Difference Between Audience and Critic Scores") 
grid.arrange(over, under, ncol=2)


#####################
# scatterplot actors
ggplot(scoreMedians, aes(CalculatedCritic, UserMedian)) + geom_point(color="cadetblue", size=4) + 
    theme_classic(base_size=16) +
    geom_abline(intercept=0, slope=1, linetype=2, color="gray50") +
     geom_label_repel(aes(label=Actor), size=3, color="gray20",
                      label.padding=unit(0.1, "lines"),
                      label.size=0.1,
                      point.padding=unit(0.1, "lines")) +
     xlim(c(30,100)) + ylim(c(40,85)) + 
     ylab("Audience Score") + xlab("Critic Score") + labs(title="Audience vs. Critic Scores, Actors") +
     annotate("text", x=35, y=85, label="Audience Favorites", size=5, hjust=0, color="gray20") +
     annotate("text", x=95, y=45, label="Critic Favorites", size=5, hjust=1, color="gray20") 

#####################
# look at individual actors
actor_plot <- function(actor) { 
     movieplot <- movies2[movies2$Actor==actor, ]
     movieplot <- movieplot[ ,c(4,5,13,15,20)]
     movieplot <- movieplot[order(movieplot$tomatoUserMeter),]
     movieplot$Title <- factor(movieplot$Title, levels=movieplot$Title)
     
     ggplot(movieplot, aes(color=Title)) + theme_classic() + scale_color_brewer(palette="Paired") +
          geom_segment(aes(x=tomatoMeter, xend=tomatoUserMeter, y=Title, yend=Title), color="#20687d", size=1) +
          geom_point(aes(x=tomatoMeter, y=Title, color="Critic"), size=4, shape=15) +
          geom_point(aes(x=tomatoUserMeter, y=Title, color="Audience"), size=4, shape=15) +
          xlab("Score") + ylab("") + labs(title=paste("Movies Starring", actor, "\nRanked by Audience Score", sep=" ")) +
          xlim(c(0,100)) 
     
}
actor_plot("Hugo Weaving")
actor_plot("Marion Cotillard")
actor_plot("Helena Bonham Carter")
actor_plot("Daniel Day-Lewis")












