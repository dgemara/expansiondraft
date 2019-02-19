library(rvest)
library(tidyverse)

getpoints <- function(code) {
  message(paste("Retrieving data for", code))
  url <- "https://www.hockey-reference.com/teams/"
  data <- url %>%
    paste0(code,
           "/2017.html") %>%
    read_html(url) %>%
    html_nodes(xpath = '//*[@id="skaters"]')  %>%
    html_table() 
  data <- data[[1]]
  names(data) <- data[1,]
  data <- data[-1, ]
  data <- data[-nrow(data), ]
  data$TEAM <- code
  return(data)
}

teams_hockeyref <- c("ANA","ARI","CGY","EDM","LAK","SJS","VAN",
                     "CHI","MIN","WPG","DAL","STL","COL","NSH",
                     "TOR","MTL","OTT","TBL","FLA","DET","BOS","BUF",
                     "CBJ","NYR","NYI","PHI","PIT","WSH","CAR","NJD")

getgoalies <- function(code) {
  message(paste("Retrieving data for", code))
  url <- "https://www.hockey-reference.com/teams/"
  data <- url %>%
    paste0(code,
           "/2017.html") %>%
    read_html(url) %>%
    html_nodes(xpath = '//*[@id="goalies"]')  %>%
    html_table() 
  data <- data[[1]]
  names(data) <- data[1,]
  data <- data[-1, ]
  data <- data[-nrow(data), ]
  data$TEAM <- code
  data$POS <- "G"
  return(data)
}

hockeyref <- lapply(teams_hockeyref, getpoints) %>%
  bind_rows() 

hockeyref_goalies <- lapply(teams_hockeyref, getgoalies) %>%
  bind_rows()


# Game Score --------------------------------------------------------------

gamescore <- read.csv("201617GameScore.csv") %>%
  select(Player, GS)



# CapFriendly -------------------------------------------------------------

getcaphit <- function(code, position) {
  message(paste("Retrieving data for", code))
  if (position == "forwards") {
    i <- 2
  }
  else if (position == "defensemen") {
    i <- 3
  }
  else {
    i <- 4
  }
  #message(paste("Retrieving position: ", position))
  if (code != 27) {
    source <- paste0("//*[@id='sect_",
                     code,
                     "']/div/div/div/div[", i, "]/div/table")
  } else {
    source <-
      paste0('//*[@id="draft_area"]/div[29]/div/div/div/div[', i, ']/div/table')
  }
  url <- "https://www.capfriendly.com/expansion-draft/vegas"
  # url <- "Vegas Expansion Draft - CapFriendly - NHL Salary Caps.html"
  data <- url %>%
    read_html(url) %>%
    html_nodes(xpath = source)  %>%
    html_table()
  data <- data[[1]]
  return(data)
}

forwards <- lapply(c(1:30), getcaphit, position = "forwards") %>%
  bind_rows()

defensemen <- lapply(c(1:30), getcaphit, position = "defensemen") %>%
  bind_rows()

goalies <- lapply(c(1:30), getcaphit, position = "goalies") %>%
  bind_rows()

names(forwards)[1] <- "Player"
names(defensemen)[1] <- "Player"
names(goalies)[1] <- "Player"

forwards$`40/70` <- NULL
defensemen$`40/70` <- NULL

capFriendly <- bind_rows(forwards, defensemen, goalies)

getprotected <- function(code) {
  url <- "https://en.wikipedia.org/wiki/2017_NHL_Expansion_Draft"
  data <- url  %>%
    read_html(url) %>%
    html_nodes(xpath = paste0('//*[@id="mw-content-text"]/div/table[', code , ']'))  %>%
    html_table()
  data <- data[[1]]
  data <- data[,-1]
  data <- gather(data)
  return(data)
}
protected <- lapply(c(2:5), getprotected) %>%
  bind_rows() 

getrookies <- function(code) {
  message(paste("Retrieving data for", code))
  url <- "https://www.capfriendly.com/expansion-draft/vegas"
  # url <- "Vegas Expansion Draft - CapFriendly - NHL Salary Caps.html"
  ## leafs rookies for some reason not in same table format
  if (code != 27) {
    source <- paste0("//*[@id='sect_",
                     code,
                     "']/div/div/div/div[4]/div/table[2]")
  } else {
    source <-
      '//*[@id="draft_area"]/div[29]/div/div/div/div[4]/div/table[2]'
  }
  data <- url %>%
    read_html(url) %>%
    html_nodes(xpath = source)  %>%
    html_table()
  data <- data[[1]]
  return(data)
}

rookies <- lapply(c(1:30), getrookies) %>%
  bind_rows()

write.csv(capFriendly, "data/caphits.csv")
write.csv(hockeyref, "data/pointshares.csv")
write.csv(hockeyref_goalies, "data/goalies.csv")
write.csv(rookies, "data/rookies.csv")
write.csv(protected, "data/protected.csv")
