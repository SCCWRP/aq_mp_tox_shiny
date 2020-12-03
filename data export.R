###Practice with lists

# for help https://bioconnector.github.io/workshops/r-lists.html?fbclid=IwAR0lrzrHGZC0LC5D0z3Q-9OABsiYfIe1IXPcPxxAJyPEus-jCvU-7NQaR10
slamwins <- list(17,14,14,12,11)
class(slamwins)
slamwins
slamwins[[2]][1]
names(slamwins) <- c("Federer", "Sampras", "Nadal", "Djokovic", "Borg")
slamwins
slamwins$Federer
slamwins <- 
  list(
    Federer = 
      list(
        AUS = 4, 
        FR = 1,
        WIM = 7,
        US = 5),
    Sampras = 
      list(
        AUS = 2,
        FR = 0,
        WIM = 7,
        US = 5),
    Nadal = 
      list(
        AUS = 1,
        FR = 9,
        WIM = 2,
        US = 2),
    Djokovic = 
      list(
        AUS = 6,
        FR = 1,
        WIM = 3,
        US = 2),
    Borg = 
      list(
        AUS = 0,
        FR = 6,
        WIM = 5,
        US = 0)
  )
slamwins

#add a list of totals
totals <- c(17, 14, 14, 12, 11)

for (i in 1:length(slamwins)) {
  
  slamwins[[i]]$Total <- totals[i]
  
}

slamwins

#return to previous state using NULL
for (i in 1:length(slamwins)) {
  
  slamwins[[i]]$Total <- NULL
  
}
lapply(slamwins, unlist)
lapply(lapply(slamwins, unlist), sum)
slamwins <- lapply(lapply(slamwins, unlist), function(x) c(x, Total = sum(x)))
slamwins
as.data.frame(slamwins)

datmat <- do.call(rbind, slamwins)
datdf <- as.data.frame(datmat, row.names = FALSE)
datdf$player <- row.names(datmat)
datdf


library(dplyr)
aoc2 <- aoc %>% 
  dplyr::mutate_all(as.factor) %>%
  map(levels)

lapply(aoc2, unlist)
aoc3 <- do.call(rbind, aoc2)
as.data.frame(aoc2)  
  
bind_rows(aoc2)

names = names(aoc2) %>%
  unlist()
aoc
library(tidyverse)
names[1]


df = data.frame(x = aoc2[[1]])
names(df) <- names[1]


for(n in 2:length(aoc2)){
  df2 = data.frame(x = aoc2[[n]])
  names(df2) <- names[n]
  df = cbind(df,df2)
}



          
write_csv(aoc3, "aoc4.csv", col_names = TRUE)
tibble(aoc2)

aoc4<- map_dfr(aoc3)

sapply(aoc2, levels)

for (n in names(aoc))
  if (is.factor(aoc[[n]])) {
    print(n)
    print(levels(aoc[[n]]))
  }
sapply(aoc3,levels)


library(data.table)

head(ldply(aoc2, rbind))
