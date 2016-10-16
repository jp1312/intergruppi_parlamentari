

# scrape data on Parlamentari/senatori from OP
url <- "http://parlamento17.openpolis.it/lista-dei-parlamentari-in-carica/senato/nome/asc"



# ------------------ info pulite in excel e ora le importo

library(xlsx)
sen <- read.xlsx(file = "./www/OP/OP_senatori.xlsx", sheetName = "fin")
str(sen)
head(sen)
tail(sen)

# sistemo indice produttivita
sen$indice_prod <- as.character(sen$indice_prod)
idx_na <- sen$indice_prod=="NA"
sum(idx_na)
sen$indice_prod[idx_na] <- NA
sen$indice_prod <- as.numeric(sen$indice_prod)


# sistemo gruppo
str(sen)
idx_gr_na <- sen$Gruppo=="NA"
sum(idx_gr_na)
sen[idx_gr_na, "Gruppo"] <- NA
str(sen)

# sistemo parlamentare
sen$parlamentare <- as.character(sen$parlamentare)
str(sen)

# controllo circoscrizione
table(sen$circoscrizione)

# controllo gruppo
table(sen$Gruppo)
sum(is.na(sen$Gruppo))
sen$Gruppo <- droplevels(sen$Gruppo)  # drop levels without occurrence
table(sen$Gruppo)


# aggiungo variabile (costante in realta) camera: senato
str(sen)
sen$camera <- "senato"
sen$camera <- as.factor(sen$camera)

OP_senatori <- sen

rm(list=c("OP_deputati", "aderenti_innovazione", "sen", "idx_na", "idx_gr_na"))
getwd()
save.image("./data/OP_senatori.RData")
