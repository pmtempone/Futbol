----#library----

library(rvest)


---#webscraping transfer market argentina----


url_arg <- "http://www.transfermarkt.com/primera-division/startseite/wettbewerb/AR1N"

WS <- read_html(url_arg)

URLs <- WS %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href") %>% as.character()

URLs <- paste0("http://www.transfermarkt.com",URLs)

Catcher1 <- data.frame()

for (i in URLs) {
  
  WS1 <- read_html(i)
  Player <- WS1 %>% html_nodes("#yw1 .spielprofil_tooltip") %>% html_text() %>% as.character()
  P_URL <- WS1 %>% html_nodes("#yw1 .spielprofil_tooltip") %>% html_attr("href") %>% as.character()
  temp <- data.frame(Player,P_URL)
  Catcher1 <- rbind(Catcher1,temp)
  cat("*")
}

no.of.rows <- nrow(Catcher1)
odd_indexes<-seq(1,no.of.rows,2)
Catcher1 <- data.frame(Catcher1[odd_indexes,])

Catcher1$P_URL <- paste0("http://www.transfermarkt.com",Catcher1$P_URL)

Catcher2 <- data.frame()

for (i in Catcher1$P_URL) {
  
  WS2 <- read_html(i)
  MarketValue <- WS2 %>% html_nodes(".dataMarktwert a") %>% html_text() %>% as.character()
  Player <- WS2 %>% html_nodes("h1") %>% html_text() %>% as.character()
  if (length(MarketValue) > 0) {
    temp2 <- data.frame(Player,MarketValue)
    Catcher2 <- rbind(Catcher2,temp2)} else {}
  cat("*")
}

Player
Catcher2
