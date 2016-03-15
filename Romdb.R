########### R x OMDB ##############
###### Stanislaus Stadlmann #######
# The motivation of this Script was
# to obtain all ratings on OMDB
# of a certain TV series.

# Install GitHub R Package
# devtools::install_github("hrbrmstr/omdbapi")

# Packages
library(httr)
library(dplyr)
library(ggplot2)

# Specify name of Series
title <- "Game of Thrones"

# Make containers
object <- matrix(ncol = 3)
result <- matrix(nrow = 1)

# Starting values
i <- 1
j <- 1
# Pull data
while (!is.null(result)) {
  if (i > 1 & j == 1) {
    break
  }
  # Starting values
  j <- 1
  
  while (!is.null(result)) {
    # Get data from OMDB
    query <- GET("http://www.omdbapi.com/", 
                  query = list(t = title, 
                               Season = i,
                               Episode = j),
                 r = "json")
    
    # Parse content
    query_parsed <- content(query, as = "parsed")
    
    # Store content
    result <- query_parsed$imdbRating
    
    if (!is.null(result) && result == "N/A") {
      result <- NA
    }
    
    # Next season if no more episodes
    if (is.null(result)) {
      result <- matrix(nrow = 1)
      break
    }

    # Fill container
    object <- rbind(object,
                   c(i, j, result))
    
    # Next iteration
    j <- j + 1
  }
  print(paste("Last episode of Season", i, "was episode", j - 1))
  i <- i + 1
}

# Make DF
object <- data.frame(object[2:nrow(object), ])

# Rename columns
colnames(object) <- c("Season", "Episode", "IMDBRating")

# Necessary transformations to new df
object_clean <- data.frame(object)
object_clean$episode.no <- 1:nrow(object_clean)
object_clean$Season <- as.numeric(object$Season) %>% as.factor()
object_clean$Episode <- as.numeric(object$Episode) %>% as.factor()
object_clean$IMDBRating <- as.numeric(as.character(object$IMDBRating))

# Delete Season if only NA's
for (season in unique(object_clean$Season)) {
  if (all(is.na(object_clean[object_clean$Season == season, "IMDBRating"]))) {
    object_clean <- object_clean[object_clean$Season != season, ]
  }
}


# Manual adjustments


# Graph
ggplot(data = object_clean, aes(x = episode.no,
                          y = IMDBRating,
                          col = Season)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle(paste0("IMDB Ratings of ", title, " episodes \n")) +
  theme_bw() +
  xlab("Episode No.")


