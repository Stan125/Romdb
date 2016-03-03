#### Stani Suits : is my show going to get better or worse

# devtools::install_github("hrbrmstr/omdbapi")

# Packages
library(XML)
library(omdbapi)
library(dplyr)
library(ggplot2)

# Scrape IMDB Ratings
frame <- search_by_title("Suits", type = "series")
find_by_title("Suits")
suits <- matrix(ncol = 3)
ergebnis <- c()
for (i in 1:5) {
  for (j in 1:20){
    
    # Daten von OMDB ziehen
    ergebnis <- frame[1, 1] %>% 
      as.character() %>%
      find_by_title(.,
                    season = i,
                    episode = j) 
    
    # Wenn nicht gefunden, next
    if (nrow(ergebnis) == 1) {
      
      # IMDBRating rausziehen
      ergebnis <- ergebnis %>% dplyr::select(imdbRating)
      
      # Dataframe füllen
      suits <- rbind(suits,
                     c(i, j, ergebnis))
    } 
    
    
  }
}

suits_clean <- data.frame(suits[2:nrow(suits), ])
colnames(suits_clean) <- c("Season", "Episode", "IMDBRating")
suits_clean$episode.no <- 1:nrow(suits_clean)
suits_clean$Season <- as.numeric(suits_clean$Season) %>% as.factor()
suits_clean$Episode <- as.numeric(suits_clean$Episode) %>% as.factor()
suits_clean$IMDBRating <- as.numeric(suits_clean$IMDBRating)
suits_clean <- as.data.frame(suits_clean)


# Manual adjustments
suits_clean[70, 3] <- 9.9
suits_clean[68, 3] <- 9.4

# Graph
png("suitsIMDB.png",
    res = 125,
    height = 900,
    width = 1600)
ggplot(data = suits_clean, aes(x = episode.no,
                         y = IMDBRating,
                         col = Season)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle("IMDB Ratings of Suits episodes \n") +
  theme_bw() +
  xlab("Episode No.")
dev.off()


