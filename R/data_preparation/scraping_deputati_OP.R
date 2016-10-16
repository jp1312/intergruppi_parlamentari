
# packages
library(rvest)

# scrape data on Parlamentari from OP
url <- "http://parlamento17.openpolis.it/lista-dei-parlamentari-in-carica/camera/nome/asc"

# deputati <- read_html(url)
# deputati %>%
#  html_nodes("table") %>%
#  .[[1]] %>%
#  html_table()


# Read and parse HTML file
library(RCurl)
library(XML)
html <- getURL(url, followlocation = TRUE)
doc <- htmlParse(html, asText = TRUE)

# I noticed inspecting the source of the url in Chrome that the name of each parlamentare is
# within a tag <a> then I extract all the <a> tags, unlist to flattens the list to
# create a character vector.

plain.text = xpathApply(doc, '//a', xmlValue)
t1 <- unlist(plain.text)

# within tag <a> there is also other stuff so now i have to isolate the name of parlamentare
# I do it manually (to be improved!)

# i floor and cap the data checking first and last parlamentare position
t2 <- t1[35:1899]
t2

# to eliminate voti ribelli i create an index checking when the element does not contain a number 
idx <- is.na(lapply(t2, as.numeric))
t3 <- t2[idx]

# now parlamenatare name is at at all even positions
idx_even <- seq_along(t3) %% 2 == 0

t4 <- t3[idx_even]

head(t4)
length(t4)  # 630, ok
str(t4)

# prova qualcosa di piu pulito con 
# getNodeSet
# in qualche modo estrai il valore dentro il tag "//p:class='politician-id'/
# oppure impara a scrivere una regular expression per specificare che vuoi isolare testo con tutte maiuscole, che inizia e termina con parentesi ecc.





# ------------------ il resto delle info le ho pulite in excel e ora le importo

library(xlsx)
dep2 <- read.xlsx(file = "./www/OP/OP_deputati.xlsx", sheetName = "fin")
str(dep2)
head(dep2)
tail(dep2)

# sistemo indice produttivita
dep2$indice_prod <- as.character(dep2$indice_prod)
idx_na <- dep2$indice_prod=="NA"
sum(idx_na)
dep2$indice_prod[idx_na] <- NA
dep2$indice_prod <- as.numeric(dep2$indice_prod)
str(dep2)
head(dep2)
tail(dep2)


# sistemo gruppo
str(dep2)
idx_gr_na <- dep2$Gruppo=="NA"
sum(idx_gr_na)
dep2[idx_gr_na, "Gruppo"] <- NA
str(dep2)

# controllo circoscrizione
table(dep2$circoscrizione)

# controllo gruppo
table(dep2$Gruppo)
sum(is.na(dep2$Gruppo))
dep2$Gruppo <- droplevels(dep2$Gruppo)  # drop levels without occurrence
table(dep2$Gruppo)


# aggiungo variabile (costante in realta) camera: camera
str(dep2)
dep2$camera <- "camera"
dep2$camera <- as.factor(dep2$camera)




OP_deputati <- dep2
OP_deputati$parlamentare <- t4
OP_deputati <- OP_deputati[,c(10, 1:9)]
str(OP_deputati)

dir()
getwd()

rm(list=c("dep2", "doc", "html", "idx", "idx_even", "idx_na"))
rm(list=c("deputati", "plain.text", "t1", "t2", "t3", "t4", "tt"))
rm(list="url")
rm(list=c("OP_senatori", "idx_gr_na"))
save.image("./data/OP_deputati.RData")
