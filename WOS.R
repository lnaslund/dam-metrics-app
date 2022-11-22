wos <- read.csv("WOS_Citations.csv")

wos_longer <- wos %>% pivot_longer(cols = c("X1a":"X5f"), names_to = "variable", values_to = "value")

test <- c("X4a", "X2b", "X1a")

dois <- NULL
for(i in 1:length(test)){
  temp_doi <- wos_longer %>% filter(variable == test[i], value ==1)
  
  dois <- dois %>% bind_rows(temp_doi)
}

dois %>% distinct(Article.Title) 

str_sub(wos$DOI, 1, 2) != 10

str_count("Rodeles, AA; Galicia, D; Miranda, R", ",")





