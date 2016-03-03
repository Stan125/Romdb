########### R x OMDB ##############
###### Stanislaus Stadlmann #######
# The motivation of this Script was
# to obtain all ratings on OMDB
# of a certain TV series.

# Install GitHub R Package
# devtools::install_github("hrbrmstr/omdbapi")

# Packages
library(XML)
library(omdbapi)
library(dplyr)
library(ggplot2)

# Do a search of the series/movie
search_results <- search_by_title("Suits", type = "series")

# Specify here which of the results is the right one
title <- search_results[1, 1]

# Make containers
suits <- matrix(ncol = 3)
ergebnis <- c()

# Starting values
j <- 1
# Pull data
for (i in 1:6) {
  j <- 1
  while (nrow(ergebnis) == 1) {
    
    # Get data from OMDB, suppress error
    ergebnis <- title %>% 
          as.character() %>%
          find_by_title(.,
                        season = i,
                        episode = j)
    
    # IMDBRating rausziehen
    ergebnis <- ergebnis %>% dplyr::select(imdbRating)
    
    # Dataframe füllen
    suits <- rbind(suits,
                   c(i, j, ergebnis))
    
    # Next iteration
    j <- j + 1
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


