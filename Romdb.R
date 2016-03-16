########### R x OMDB ##############
###### Stanislaus Stadlmann #######
# The motivation of this Script was
# to obtain all ratings on OMDB
# of a certain TV series.

# Packages
library(httr)
library(dplyr)
library(ggplot2)

# Specify name of Series
title <- "Game of Thrones"

# Make containers
object <- data.frame(Season = NA,
                     Episode = NA, 
                     imdbRating = NA,
                     episode.no = NA)
result <- matrix(nrow = 1)
ep.no <- 0

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
      result <- 0
      break
    }

    # Make episode no higher
    ep.no <- ep.no + 1
    
    # Fill container
    object[ep.no, ] <- c(i, j, result, ep.no)
    
    # Next iteration
    j <- j + 1
  }
  cat(paste("Last episode of Season", i, "was episode", j - 1, "\n"))
  i <- i + 1
}

# Necessary transformations
object <- sapply(object, FUN = as.numeric)
object <- as.data.frame(object)

# Delete Season if only NA's
for (season in unique(object$Season)) {
  if (all(is.na(object[object$Season == season, "imdbRating"]))) {
    object <- object[object$Season != season, ]
    cat("Season", season, "was deleted, because all Ratings were NA")
  }
}


# Manual adjustments


# Graph
ggplot(data = object, aes(x = episode.no,
                          y = imdbRating,
                          col = Season)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle(paste0("IMDB Ratings of ", title, " episodes \n")) +
  theme_bw() +
  xlab("Episode No.")


