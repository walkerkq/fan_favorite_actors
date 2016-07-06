library(RCurl)
library(XML)

# Careers of Top Actors
# http://www.the-numbers.com/people/records/top-grossing-stars-in-all-roles
moneyActors <- data.frame(Name=c(
     "Samuel L. Jackson", "Harrison Ford", "Morgan Freeman", "Tom Hanks", "Robert Downey, Jr.",
     "Liam Neeson", "Gary Oldman", "Tom Cruise", "Elizabeth Banks", "Hugo Weaving",
     "Eddie Murphy", "Bill Hader", "Stanley Tucci", "Robin Williams", "Johnny Depp",
     "Alan Rickman", "Matt Damon", "Bruce Willis", "Michael Caine", "Scarlett Johansson",
     "Owen Wilson", "Jon Favreau", "Ian McKellen", "Helena Bonham Carter", "Woody Harrelson",
     "Willem Dafoe", "Cameron Diaz", "Don Cheadle", "Donald Sutherland", "Alan Tudyk"), 
     Link=c("nm0000168", "nm0000148", "nm0000151", "nm0000158", "nm0000375",
            "nm0000553", "nm0000198", "nm0000129", "nm0006969", "nm0915989",
            "nm0000552", "nm0352778", "nm0001804", "nm0000245", "nm0000136",
            "nm0000614", "nm0000354", "nm0000246", "nm0000323", "nm0424060",
            "nm0005562", "nm0269463", "nm0005212", "nm0000307", "nm0000437",
            "nm0000353", "nm0000139", "nm0000332", "nm0000661", "nm0876138"),
     Number=c(1:30),
     Source=rep("Money", 30)
     )

# Or based on Oscars?
oscarActors <- data.frame(Name=c(
     "Leonardo DiCaprio", "Eddie Redmayne", "Matthew McConaughey", "Daniel Day-Lewis", "Jean Dujardin",
     "Colin Firth", "Jeff Bridges", "Sean Penn", "Forest Whitaker", "Philip Seymour Hoffman", 
     "Jamie Foxx", "Adrien Brody", "Denzel Washington", "Russell Crowe", "Kevin Spacey",
     "Brie Larson", "Julianne Moore", "Cate Blanchett", "Jennifer Lawrence", "Meryl Streep",
     "Natalie Portman", "Sandra Bullock", "Kate Winslet", "Marion Cotillard", "Helen Mirren",
     "Reese Witherspoon", "Hilary Swank", "Charlize Theron", "Nicole Kidman", "Halle Berry"), 
     Link=c("nm0000138", "nm1519666", "nm0000190", "nm0000358", "nm0241121",
            "nm0000147", "nm0000313", "nm0000576", "nm0001845", "nm0000450", 
            "nm0004937", "nm0004778", "nm0000243", "nm0000128", "nm0000228",
            "nm0488953", "nm0000194", "nm0000949", "nm2225369", "nm0000658",
            "nm0000204", "nm0000113", "nm0000701", "nm0182839", "nm0000545",
            "nm0000702", "nm0005476", "nm0000234", "nm0000173", "nm0000932"),
     Number=rep(1:15, 2),
     Source=rep("Oscars", 30)
)

# or Misc.?
otherActors <- data.frame(Name=c(
     "Will Smith", "George Clooney", "Brad Pitt", "Julia Roberts", "Jack Nicholson", 
     "Ben Stiller", "Will Ferrell", "Seth Rogen", "Christian Bale", "Gerard Butler", 
     "Robert De Niro", "Mark Wahlberg", "Steve Carrell", "Edward Norton", "Josh Brolin", 
     "Aaron Eckhart", "Joseph Gordon-Levitt", "Ryan Gosling", "Steve Martin", "Meg Ryan",
     "Diane Lane", "Richard Gere", "Rachel McAdams", "Dennis Quaid", "Heath Ledger",
     "Al Pacino", "Jim Carrey", "Drew Barrymore", "Gwenyth Paltrow", "John Cusack",
     "Nicolas Cage", "Dwayne Johnson", "Eva Mendes", "Kate Hudson", "Hugh Jackman",
     "Joaquin Phoenix", "Jennifer Garner", "Michelle Pfieffer", "Patrick Swayze", "Adam Sandler",
     "Anne Hathaway", "Angelina Jolie", "Ryan Reynolds", "Jennifer Aniston", "Jackie Chan",
     "Megan Fox", "Shia LaBeouf", "Jack Black", "Keanu Reeves", "Kiera Knightley", 
     "John Goodman", "Bradley Cooper", "Jessica Alba", "Arnold Schwarzenegger", "Jeff Daniels",
     "Chris Evans", "Signourney Weaver", "Ethan Hawke", "Chris Farley", "David Spade",
     "Kristen Stewart", "Vin Diesel", "Anna Kendrick", "Miles Teller", "Vince Vaughn", 
     "John Travlota", "Kevin Costner", "Ray Liotta", "Daniel Craig", "Pierce Brosnan",
     "Mike Myers", "Danny DeVito", "Michael Douglas", "Clint Eastwood", "Sean Bean"
     ), Link=c(
     "nm0000226", "nm0000123", "nm0000093", "nm0000210", "nm0000197", 
     "nm0001774", "nm0002071", "nm0736622", "nm0000288", "nm0124930", 
     "nm0000134", "nm0000242", "nm0136797", "nm0001570", "nm0000982", 
     "nm0001173", "nm0330687", "nm0331516", "nm0000188", "nm0000212",
     "nm0000178", "nm0000152", "nm1046097", "nm0000598", "nm0005132",
     "nm0000199", "nm0000120", "nm0000106", "nm0000569", "nm0000131",
     "nm0000115", "nm0425005", "nm0578949", "nm0005028", "nm0413168",
     "nm0001618", "nm0004950", "nm0000201", "nm0000664", "nm0001191",
     "nm0004266", "nm0001401", "nm0005351", "nm0000098", "nm0000329",
     "nm1083271", "nm0479471", "nm0085312", "nm0000206", "nm0461136", 
     "nm0000422", "nm0177896", "nm0001006", "nm0000216", "nm0001099",
     "nm0262635", "nm0000244", "nm0000160", "nm0000394", "nm0005450",
     "nm0829576", "nm0004874", "nm0447695", "nm1886602", "nm0000681", 
     "nm0000237", "nm0000126", "nm0000501", "nm0185819", "nm0000112",
     "nm0000196", "nm0000362", "nm0000140", "nm0000142", "nm0000293"
     ), Number=c(1:75), Source=rep("Misc", 75))

# BOTH!
actors <- rbind(moneyActors, oscarActors, otherActors)

# get imdb IDs
movieLinks <- data.frame()
for(j in seq_along(actors$Link)){  
    prodnum <- NULL
    URL <- paste("http://www.imdb.com/name/", actors$Link[j], sep="")
    result <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
    movies <- data.frame(Title=c(xpathSApply(result, "//div[@id='filmography']//b//a", xmlValue)), 
                         Link=c(xpathSApply(result, "//div[@id='filmography']//b//a", xmlGetAttr, "href")))
    number <- xpathSApply(result, "//div[@id='filmo-head-actor']", xmlValue)
    if(length(number)==0){number <- xpathSApply(result, "//div[@id='filmo-head-actress']", xmlValue) }
    number <- as.numeric(gsub("[^0-9]+", "", number))
    if(actors$Name[j] %in% c("Tom Hanks", "Jon Favreau", "Drew Barrymore", "Mark Wahlberg")) {
        # check for producer credits 
        prodnum <- xpathSApply(result, "//div[@id='filmo-head-producer']", xmlValue)
        prodnum <- as.numeric(gsub("[^0-9]+", "", prodnum))
    }else if(actors$Name[j] %in% c("Steve Martin")){
        # check for writer credits 
        prodnum <- xpathSApply(result, "//div[@id='filmo-head-writer']", xmlValue)
        prodnum <- as.numeric(gsub("[^0-9]+", "", prodnum))
    }
    if(length(prodnum)>0) {
        movies <- movies[(prodnum+1):(prodnum+number), ]
    } else { movies <- movies[1:number,] }
    movies$Actor <- actors$Name[j]
    movies$Source <- actors$Source[j]
    movieLinks <- rbind(movieLinks, movies)
}
movieLinks$Link <- substring(movieLinks$Link, 8, 16)

# get imdb and RT data
movieInfo <- data.frame()
for(z in seq_along(movieLinks$Link)){
     URL <- paste("http://www.omdbapi.com/?i=", movieLinks$Link[z], "&plot=short&tomatoes=true&r=xml", sep="" )
     result <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
     test <- xpathSApply(result, "//movie", xmlGetAttr, "imdbid")
     if(length(test)>0) {
          row <- data.frame(
               Actor=movieLinks$Actor[z],
               Source=movieLinks$Source[z],
               ID=xpathSApply(result, "//movie", xmlGetAttr, "imdbid"),
               Title=xpathSApply(result, "//movie", xmlGetAttr, "title"),
               Year=xpathSApply(result, "//movie", xmlGetAttr, "year"),
               Rated=xpathSApply(result, "//movie", xmlGetAttr, "rated"),
               Type=xpathSApply(result, "//movie", xmlGetAttr, "type"),
               Actors=xpathSApply(result, "//movie", xmlGetAttr, "actors"),
               Genre=xpathSApply(result, "//movie", xmlGetAttr, "genre"),
               Metascore=xpathSApply(result, "//movie", xmlGetAttr, "metascore"),
               imdbRating=xpathSApply(result, "//movie", xmlGetAttr, "imdbrating"),
               imdbVotes=xpathSApply(result, "//movie", xmlGetAttr, "imdbvotes"),
               tomatoMeter=xpathSApply(result, "//movie", xmlGetAttr, "tomatometer"),
               tomatoRating=xpathSApply(result, "//movie", xmlGetAttr, "tomatorating"),
               tomatoUserMeter=xpathSApply(result, "//movie", xmlGetAttr, "tomatousermeter"),
               tomatoUserRating=xpathSApply(result, "//movie", xmlGetAttr, "tomatouserrating"),
               tomatoUserReviews=xpathSApply(result, "//movie", xmlGetAttr, "tomatouserreviews"),
               BoxOffice=xpathSApply(result, "//movie", xmlGetAttr, "boxoffice"),
               Production=xpathSApply(result, "//movie", xmlGetAttr, "production"))
          movieInfo <- rbind(movieInfo, row)
     }
}
# save the results
#write.csv(movieInfo, "movieInfo.csv", row.names=FALSE)

