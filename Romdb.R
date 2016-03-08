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
search_results <- search_by_title("The Office", type = "series")

# Specify here which of the results is the right one
title <- search_results[1, 1]

# Make containers
object <- matrix(ncol = 3)
result <- matrix(nrow = 1)

# Starting values
i <- 1
# Pull data
while (nrow(result) == 1) {
  if (i > 1 & j == 1) {
    break
  }
  # Starting values
  j <- 1
  
  while (nrow(result) == 1) {
    # Get data from OMDB
    result <- title %>% 
          as.character() %>%
          find_by_title(.,
                        season = i,
                        episode = j)
    
    # Next season if no more episodes
    if (nrow(result) != 1) {
      result <- matrix(nrow = 1)
      break
    }
    
    # Get IMDBRating
    result <- result %>% dplyr::select(imdbRating)
    
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
object_clean$IMDBRating <- as.numeric(object$IMDBRating)

# Manual adjustments


# Graph
png(paste0(title,"IMDB.png"),
    res = 125,
    height = 900,
    width = 1600)
ggplot(data = object_clean, aes(x = episode.no,
                                y = IMDBRating,
                                col = Season)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle(paste0("IMDB Ratings of ", title, " episodes \n")) +
  theme_bw() +
  xlab("Episode No.")
dev.off()


