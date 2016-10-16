
# creazione tabella aderenti
# chiave: id_aderente + id_intergruppo
# chiave di join con intergruppi: id_intergruppo

rm(list=ls())

## pulizia dati 
getwd()
setwd("~/github/intergruppi_parlamentari/www")
dir()

# alla fine le liste degli aderenti andranno in merge con le basi dati di openpolis dei parlamentari per cui comincio fin da ora a formattare i nomi (chiave di merge) secondo il db openpolis...
dir("../data")
load("../data/OP_deputati.RData")
load("../data/OP_senatori.RData")
parlamentari <- rbind(OP_deputati, OP_senatori)
str(parlamentari)
grep("BRUNO BOSSIO", parlamentari$parlamentare)
parlamentari[103, "parlamentare"]
grep("GADDA", parlamentari$parlamentare)
parlamentari[275, "parlamentare"]
grep("FAUTTILLI", parlamentari$parlamentare)
parlamentari[247, "parlamentare"]
grep("BORGHI", parlamentari$parlamentare)
parlamentari[85, "parlamentare"]
grep("PICCOLI Giovanni", parlamentari$parlamentare)
parlamentari[873, "parlamentare"]
grep("PANIZZA Franco", parlamentari$parlamentare)
grep("Albrecht", parlamentari$parlamentare)
grep("LANIECE", parlamentari$parlamentare)
grep("Roger", parlamentari$parlamentare)
grep("Gian Paolo", parlamentari$parlamentare)


# struttura tabella

# ----- chiave
# id parlamentare
# id_intergruppo

# ----- info trovate
# COGNOME Nome
# gruppo (FI, PD, ecc.)
# camera
# email
# commissione
# twitter

# ---- da merge con OP
# indice produttivita
# gruppo
# voti ribelli
# ecc.

# ---- da merge con dati.camera.it
# anagrafe





# -------------------------------------
# agenda digitale
# -------------------------------------

dir()
library(xlsx)
agenda_dig <- read.xlsx("./agenda_digitale/aderenti_agenda_dig.xlsx", sheetName = "fin")
str(agenda_dig)
agenda_dig$x <- 1:nrow(agenda_dig)

# merge con OP
library(dplyr)
str(agenda_dig)
str(parlamentari)
agenda_dig$parlamentare <- as.character(agenda_dig$parlamentare)
agenda_dig_op <- left_join(agenda_dig, parlamentari)
str(agenda_dig_op)

# controllo failed merge
sum(is.na(agenda_dig_op$camera))  # 5 failed merge

# apporto modifiche per ottenere full merge
idx <- is.na(agenda_dig_op$camera)
agenda_dig_op[idx,"parlamentare"]
grep("LOCATELLI", parlamentari$parlamentare)
parlamentari[354,"parlamentare"]
agenda_dig[agenda_dig$parlamentare == "LOCATELLI Pia Elda", "parlamentare"] <- "LOCATELLI Pia"
grep("DI GIORGI", parlamentari$parlamentare)
parlamentari[739,"parlamentare"]
agenda_dig[agenda_dig$parlamentare == "DIGIORGI Rosamaria", "parlamentare"] <- "DI GIORGI Rosa Maria"
grep("GALAN", parlamentari$parlamentare) # decaduto da parlamentare, lo elimino dalla lista aderenti intergruppo
agenda_dig <- agenda_dig[-10, ]
grep("VARGIU", parlamentari$parlamentare)
parlamentari[606,"parlamentare"]
agenda_dig[agenda_dig$parlamentare == "VARGIU Pier Paolo", "parlamentare"] <- "VARGIU Pierpaolo"
grep("CRIMI Vito", parlamentari$parlamentare)
parlamentari[715,"parlamentare"]
agenda_dig[agenda_dig$parlamentare == "CRIMI Vito", "parlamentare"] <- "CRIMI Vito Claudio"

# ripeto merge
agenda_dig_op <- left_join(agenda_dig, parlamentari)
str(agenda_dig_op)
sum(is.na(agenda_dig_op$camera))  # ok!
str(agenda_dig_op)
agenda_dig_op <- agenda_dig_op[,-2]

# import info dat.camera.it
getwd()
dir("../data")
load("../data/dati.camera.it.RData")
str(df)

# --------------- merge agenda_dig_op con dati.camera.it
str(agenda_dig_op)
str(df)
agenda_dig_op_camera <- left_join(agenda_dig_op, df)

# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
unique(df$genere)
sum(agenda_dig_op_camera$camera == "senato")
sum(is.na(agenda_dig_op_camera[agenda_dig_op_camera$camera == "senato", "genere"]))  # ok, tutti e 7 i senatori, come ci aspettavamo, falliscono il merge

sum(is.na(agenda_dig_op_camera[agenda_dig_op_camera$camera == "camera", "genere"]))  # 5 deputati falliscono il merge

# modifico manualmente i dati di camera.it per farli coincidere con quelli di agenda_dig e quindi anche con quelli di OP
idx <- is.na(agenda_dig_op_camera$genere) & agenda_dig_op_camera$camera == "camera"
sum(idx)
agenda_dig_op_camera[idx, "parlamentare"]
grep("QUINTARELLI", df$parlamentare)
df[4,"parlamentare"]
df[df$parlamentare == "QUINTARELLI Giuseppe stefano", "parlamentare"] <- "QUINTARELLI Stefano"
grep("BRUNO BOSSIO", df$parlamentare)
df[133,"parlamentare"]
df[df$parlamentare == "BRUNO BOSSIO Vincenza", "parlamentare"] <- "BRUNO BOSSIO Enza"
grep("GADDA", df$parlamentare)
df[242,"parlamentare"]
df[df$parlamentare == "GADDA Maria chiara", "parlamentare"] <- "GADDA Maria Chiara"
grep("LOCATELLI", df$parlamentare)
df[371,"parlamentare"]
df[df$parlamentare == "LOCATELLI Pia elda", "parlamentare"] <- "LOCATELLI Pia"
grep("CARROZZA", df$parlamentare)
df[204,"parlamentare"]
df[df$parlamentare == "CARROZZA Maria chiara", "parlamentare"] <- "CARROZZA Maria Chiara"

# ripeto merge
agenda_dig_op_camera <- left_join(agenda_dig_op, df)
# controllo deputatu
sum(is.na(agenda_dig_op_camera[agenda_dig_op_camera$camera == "camera", "genere"]))  # ok!

# aggiungo id_intergruppo
dir()
dir("../data")
load("../data/table_intergruppi.RData")
str(intergruppi)
intergruppi[intergruppi$denominazione_corta=="agenda digitale", "id_intergruppo"]
agenda_dig_op_camera$id_intergruppo <- "0003"
str(agenda_dig_op_camera)
agenda_dig_fin <- agenda_dig_op_camera[,c(1,19,2:10,13:17)]
str(agenda_dig_fin)
any(is.na(agenda_dig_fin$id_intergruppo))














# -------------------------------------
# cannabis
# -------------------------------------

dir("./cannabis")
cannabis <- read.xlsx("./cannabis/aderenti_cannabis.xlsx", sheetName = "fin")
str(cannabis)
cannabis$x <- 1:nrow(cannabis)
str(cannabis)
cannabis <- cannabis[,c(1,5)]

# merge con OP
str(cannabis)
str(parlamentari)
cannabis$parlamentare <- as.character(cannabis$parlamentare)
cannabis_op <- left_join(cannabis, parlamentari)
str(cannabis_op)

# controllo failed merge
sum(is.na(cannabis_op$camera))  # 12 failed merge

# apporto modifiche per ottenere full merge
idx <- is.na(cannabis_op$camera)

# scrivo semplice funzione per automatizzare la modifica del nome quando ho un match con grep 1:1
daModificare <- cannabis_op[idx,"parlamentare"]
cercaMatch <- strsplit(daModificare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], parlamentari$parlamentare)
  if(length(ctrl)==1) {
    cambio <- parlamentari[ctrl,"parlamentare"]
    cannabis[cannabis$parlamentare == daModificare[i], "parlamentare"] <- cambio
    cat("nome ",i, "aggiustato: da ", daModificare[i], " a ", cambio, "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}

if(residuo>0) {
    warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}

# ripeto merge
cannabis_op <- left_join(cannabis, parlamentari)
idx <- is.na(cannabis_op$camera)
cannabis_op[idx,"parlamentare"]
grep("DE ROSA", parlamentari$parlamentare)
parlamentari[213,"parlamentare"]
cannabis[cannabis$parlamentare == "DE ROSA Felice", "parlamentare"] <- "DE ROSA Massimo"
grep("FERRARA Francesco", parlamentari$parlamentare)
parlamentari[252,"parlamentare"]
cannabis[cannabis$parlamentare == "FERRARA Francesco", "parlamentare"] <- "FERRARA Francesco Detto Ciccio"
grep("FORGIA", parlamentari$parlamentare)
parlamentari[343,"parlamentare"]
cannabis[cannabis$parlamentare == "LA FORGIA Francesco", "parlamentare"] <- "LAFORGIA Francesco"
grep("SCHIR", parlamentari$parlamentare)
parlamentari[560,"parlamentare"]
cannabis[cannabis$parlamentare == cannabis_op[which(idx)[5],"parlamentare"], "parlamentare"] <- "SCHIRO' Gea"
grep("BOSSIO", parlamentari$parlamentare)
parlamentari[103,"parlamentare"]
cannabis[cannabis$parlamentare == "BRUNO BOSSIO Vincenza", "parlamentare"] <- "BRUNO BOSSIO Enza"


# ripeto merge
cannabis_op <- left_join(cannabis, parlamentari)
sum(is.na(cannabis_op$camera))  # ok!
str(cannabis_op)
cannabis_op <- cannabis_op[,-2]


# --------------- merge agenda_dig_op con dati.camera.it
str(cannabis_op)
str(df)
cannabis_op_camera <- left_join(cannabis_op, df)

# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
sum(cannabis_op_camera$camera == "senato")
sum(is.na(cannabis_op_camera[cannabis_op_camera$camera == "senato", "genere"]))  # ok, tutti e 24 i senatori, come ci aspettavamo, falliscono il merge

sum(is.na(cannabis_op_camera[cannabis_op_camera$camera == "camera", "genere"]))  # 10 deputati falliscono il merge

# prima correzione con ciclo for
idx <- is.na(cannabis_op_camera$genere) & cannabis_op_camera$camera == "camera"
daUtilizzare <- cannabis_op_camera[idx, "parlamentare"]
cercaMatch <- strsplit(daUtilizzare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], df$parlamentare)
  if(length(ctrl)==1) {
    daModificare <- df[ctrl,"parlamentare"]
    df[df$parlamentare == daModificare, "parlamentare"] <- daUtilizzare[i]
    cat("nome ",i, "aggiustato: da ", daModificare, " a ", daUtilizzare[i], "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}
if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}

# ripeto merge
cannabis_op_camera <- left_join(cannabis_op, df)

# ora modifiche manuali
sum(is.na(cannabis_op_camera[cannabis_op_camera$camera == "camera", "genere"]))  # 1 failed

idx <- is.na(cannabis_op_camera$genere) & cannabis_op_camera$camera == "camera"
cannabis_op_camera[idx, "parlamentare"]
grep("DE ROSA", df$parlamentare)
df[170,"parlamentare"]
df[df$parlamentare == "DE ROSA Massimo felice", "parlamentare"] <- "DE ROSA Massimo"

# ripeto merge
cannabis_op_camera <- left_join(cannabis_op, df)
# controllo deputati
sum(is.na(cannabis_op_camera[cannabis_op_camera$camera == "camera", "genere"]))  # ok!

# aggiungo id_intergruppo
intergruppi[intergruppi$denominazione_corta=="cannabis", "id_intergruppo"]
cannabis_op_camera$id_intergruppo <- "0007"
str(cannabis_op_camera)
cannabis_fin <- cannabis_op_camera[,c(1,19,2:10,13:17)]
str(cannabis_fin)
any(is.na(cannabis_fin$id_intergruppo))
cannabis_fin







# -------------------------------------
# e-cig
# -------------------------------------

dir("./e-cig")
ecig <- read.xlsx("./e-cig/aderenti_ecig.xlsx", sheetName = "fin")
str(ecig)
ecig$x <- 1:nrow(ecig)
str(ecig)
ecig <- ecig[,c(3,5)]

# merge con OP
str(ecig)
str(parlamentari)
ecig$parlamentare <- as.character(ecig$parlamentare)
ecig_op <- left_join(ecig, parlamentari)
str(ecig_op)

# controllo failed merge
sum(is.na(ecig_op$camera))  # 2 failed merge

# apporto modifiche per ottenere full merge
idx <- is.na(ecig_op$camera)

# scrivo semplice funzione per automatizzare la modifica del nome quando ho un match con grep 1:1
daModificare <- ecig_op[idx,"parlamentare"]
cercaMatch <- strsplit(daModificare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], parlamentari$parlamentare)
  if(length(ctrl)==1) {
    cambio <- parlamentari[ctrl,"parlamentare"]
    ecig[ecig$parlamentare == daModificare[i], "parlamentare"] <- cambio
    cat("nome ",i, "aggiustato: da ", daModificare[i], " a ", cambio, "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}

if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
  } else {
    cat("Tutto modificato correttamente!")
}
  

# ripeto merge
ecig_op <- left_join(ecig, parlamentari)
idx <- is.na(ecig_op$camera)
sum(idx)  # ok!
  
str(ecig_op)
ecig_op <- ecig_op[,-2]
  
  
# --------------- merge agenda_dig_op con dati.camera.it
str(ecig_op)
str(df)
ecig_op_camera <- left_join(ecig_op, df)
  
# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
sum(ecig_op_camera$camera == "senato")
sum(is.na(ecig_op_camera[ecig_op_camera$camera == "senato", "genere"]))  # ok, tutti e 5 i senatori, come ci aspettavamo, falliscono il merge
  
sum(is.na(ecig_op_camera[ecig_op_camera$camera == "camera", "genere"]))  # ok!
  
  
# aggiungo id_intergruppo
intergruppi[intergruppi$denominazione_corta=="e-cig", "id_intergruppo"]
ecig_op_camera$id_intergruppo <- "0012"
str(ecig_op_camera)
ecig_fin <- ecig_op_camera[,c(1,19,2:10,13:17)]
str(ecig_fin)
ecig_fin  







# -------------------------------------
# innovazione
# -------------------------------------

dir("./innovazione")
innovazione <- read.xlsx("./innovazione/aderenti_innovazione.xlsx", sheetName = "fin")
str(innovazione)
innovazione$x <- 1:nrow(innovazione)
str(innovazione)
innovazione <- innovazione[,c(3,9)]

# merge con OP
str(innovazione)
str(parlamentari)
innovazione$parlamentare <- as.character(innovazione$parlamentare)
innovazione_op <- left_join(innovazione, parlamentari)
str(innovazione_op)

# controllo failed merge
sum(is.na(innovazione_op$camera))  # 15 failed merge

# apporto modifiche per ottenere full merge
idx <- is.na(innovazione_op$camera)

# scrivo semplice funzione per automatizzare la modifica del nome quando ho un match con grep 1:1
daModificare <- innovazione_op[idx,"parlamentare"]
cercaMatch <- strsplit(daModificare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], parlamentari$parlamentare)
  if(length(ctrl)==1) {
    cambio <- parlamentari[ctrl,"parlamentare"]
    innovazione[innovazione$parlamentare == daModificare[i], "parlamentare"] <- cambio
    cat("nome ",i, "aggiustato: da ", daModificare[i], " a ", cambio, "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}

if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}


# ripeto merge
innovazione_op <- left_join(innovazione, parlamentari)
idx <- is.na(innovazione_op$camera)
innovazione_op[idx,"parlamentare"]
grep("BRUNO BOSSIO", parlamentari$parlamentare)
parlamentari[103,"parlamentare"]
innovazione[innovazione$parlamentare == "BRUNO BOSSIO Vincenza", "parlamentare"] <- "BRUNO BOSSIO Enza"
grep("CRIMI Vito", parlamentari$parlamentare)
parlamentari[715,"parlamentare"]
innovazione[innovazione$parlamentare == "CRIMI Vito", "parlamentare"] <- "CRIMI Vito Claudio"
grep("CURR", parlamentari$parlamentare)
parlamentari[188,"parlamentare"]
innovazione[innovazione$parlamentare == innovazione_op[idx,"parlamentare"][3], "parlamentare"] <- "CURRO' Tommaso"
grep("ALIA Gianpiero", parlamentari$parlamentare)
parlamentari[191,"parlamentare"]
innovazione[innovazione$parlamentare == innovazione_op[idx,"parlamentare"][4], "parlamentare"] <- "D'ALIA Gianpiero"
grep("DI GIORGI", parlamentari$parlamentare)
parlamentari[739,"parlamentare"]
innovazione[innovazione$parlamentare == innovazione_op[idx,"parlamentare"][5], "parlamentare"] <- "DI GIORGI Rosa Maria"
grep("Federico", parlamentari$parlamentare)
parlamentari[195,"parlamentare"]
innovazione[innovazione$parlamentare == innovazione_op[idx,"parlamentare"][6], "parlamentare"] <- "D'INCA' Federico"
grep("GALAN", parlamentari$parlamentare) # decaduto
grep("GALAN", innovazione$parlamentare)
innovazione <- innovazione[-46,]
grep("NACCARATO", parlamentari$parlamentare)
parlamentari[848,"parlamentare"] # in questo caso preferisco modificare OP dove chiaramente c'e un errore nel nome...
parlamentari[848, "parlamentare"] <- "NACCARATO Paolo"
grep("ROSSI Maurizio", parlamentari$parlamentare)
parlamentari[892,"parlamentare"]
innovazione[innovazione$parlamentare == innovazione_op[idx,"parlamentare"][9], "parlamentare"] <- "ROSSI Maurizio Giuseppe"
grep("ROMANO Paolo", parlamentari$parlamentare)
parlamentari[527,"parlamentare"]
innovazione[innovazione$parlamentare == innovazione_op[idx,"parlamentare"][10], "parlamentare"] <- "ROMANO Paolo Nicolo'"


# ripeto merge
innovazione_op <- left_join(innovazione, parlamentari)
sum(is.na(innovazione_op$camera))  # ok!
str(innovazione_op)
innovazione_op <- innovazione_op[,-2]


# --------------- merge agenda_dig_op con dati.camera.it
str(innovazione_op)
str(df)
innovazione_op_camera <- left_join(innovazione_op, df)

# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
sum(innovazione_op_camera$camera == "senato")
sum(is.na(innovazione_op_camera[innovazione_op_camera$camera == "senato", "genere"]))  # ok, tutti e 24 i senatori, come ci aspettavamo, falliscono il merge

sum(is.na(innovazione_op_camera[innovazione_op_camera$camera == "camera", "genere"]))  # 7 deputati falliscono il merge

# prima correzione con ciclo for
idx <- is.na(innovazione_op_camera$genere) & innovazione_op_camera$camera == "camera"
daUtilizzare <- innovazione_op_camera[idx, "parlamentare"]
cercaMatch <- strsplit(daUtilizzare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], df$parlamentare)
  if(length(ctrl)==1) {
    daModificare <- df[ctrl,"parlamentare"]
    df[df$parlamentare == daModificare, "parlamentare"] <- daUtilizzare[i]
    cat("nome ",i, "aggiustato: da ", daModificare, " a ", daUtilizzare[i], "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}
if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}

# ripeto merge
innovazione_op_camera <- left_join(innovazione_op, df)

# ora modifiche manuali
sum(is.na(innovazione_op_camera[innovazione_op_camera$camera == "camera", "genere"]))  # 2 failed

idx <- is.na(innovazione_op_camera$genere) & innovazione_op_camera$camera == "camera"
innovazione_op_camera[idx, "parlamentare"]
grep("BIANCHI", df$parlamentare)
df[167,"parlamentare"]
df[df$parlamentare == "BIANCHI Mariastella", "parlamentare"] <- "BIANCHI Stella"
grep("ROMANO Paolo", df$parlamentare)
df[246,"parlamentare"]
df[df$parlamentare == "ROMANO Paolo nicolo'", "parlamentare"] <- "ROMANO Paolo Nicolo'"


# ripeto merge
innovazione_op_camera <- left_join(innovazione_op, df)
# controllo deputati
sum(is.na(innovazione_op_camera[innovazione_op_camera$camera == "camera", "genere"]))  # ok!

# aggiungo id_intergruppo
intergruppi[intergruppi$denominazione_corta=="innovazione", "id_intergruppo"]
innovazione_op_camera$id_intergruppo <- "0019"
str(innovazione_op_camera)
innovazione_fin <- innovazione_op_camera[,c(1,19,2:10,13:17)]
str(innovazione_fin)

any(is.na(innovazione_fin$id_intergruppo))










# -------------------------------------
# invecchimento
# -------------------------------------

dir("./invecchiamento_attivo")
invecchiamento <- read.xlsx("./invecchiamento_attivo/aderenti_invecchiamento_attivo.xlsx", sheetName = "fin")
str(invecchiamento)
invecchiamento$x <- 1:nrow(invecchiamento)
str(invecchiamento)
invecchiamento <- invecchiamento[,c(3,5)]

# merge con OP
str(invecchiamento)
str(parlamentari)
invecchiamento$parlamentare <- as.character(invecchiamento$parlamentare)
invecchiamento_op <- left_join(invecchiamento, parlamentari)
str(invecchiamento_op)

# controllo failed merge
sum(is.na(invecchiamento_op$camera))  # 4 failed merge

# apporto modifiche per ottenere full merge
idx <- is.na(invecchiamento_op$camera)

# scrivo semplice funzione per automatizzare la modifica del nome quando ho un match con grep 1:1
daModificare <- invecchiamento_op[idx,"parlamentare"]
cercaMatch <- strsplit(daModificare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], parlamentari$parlamentare)
  if(length(ctrl)==1) {
    cambio <- parlamentari[ctrl,"parlamentare"]
    invecchiamento[invecchiamento$parlamentare == daModificare[i], "parlamentare"] <- cambio
    cat("nome ",i, "aggiustato: da ", daModificare[i], " a ", cambio, "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}

if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}


# ripeto merge
invecchiamento_op <- left_join(invecchiamento, parlamentari)
idx <- is.na(invecchiamento_op$camera)
invecchiamento_op[idx,"parlamentare"]
grep("Silvio", parlamentari$parlamentare)
parlamentari[791,"parlamentare"]
invecchiamento[invecchiamento$parlamentare == "LAI Silvio", "parlamentare"] <- "LAI Bachisio Silvio"
grep("NISSOLI", parlamentari$parlamentare)
parlamentari[443,"parlamentare"]
invecchiamento[invecchiamento$parlamentare == "FITZGERALD NISSOLI Fucsia", "parlamentare"] <- "NISSOLI Angela Rosaria Detta Fucsia"


# ripeto merge
invecchiamento_op <- left_join(invecchiamento, parlamentari)
sum(is.na(invecchiamento_op$camera))  # ok!
str(invecchiamento_op)
invecchiamento_op <- invecchiamento_op[,-2]


# --------------- merge invecchiamento_op con dati.camera.it
str(invecchiamento_op)
str(df)
invecchiamento_op_camera <- left_join(invecchiamento_op, df)

# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
sum(invecchiamento_op_camera$camera == "senato")
sum(is.na(invecchiamento_op_camera[invecchiamento_op_camera$camera == "senato", "genere"]))  # ok, tutti e 10 i senatori, come ci aspettavamo, falliscono il merge

sum(is.na(invecchiamento_op_camera[invecchiamento_op_camera$camera == "camera", "genere"]))  # 5 deputati falliscono il merge

# prima correzione con ciclo for
idx <- is.na(invecchiamento_op_camera$genere) & invecchiamento_op_camera$camera == "camera"
daUtilizzare <- invecchiamento_op_camera[idx, "parlamentare"]
cercaMatch <- strsplit(daUtilizzare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], df$parlamentare)
  if(length(ctrl)==1) {
    daModificare <- df[ctrl,"parlamentare"]
    df[df$parlamentare == daModificare, "parlamentare"] <- daUtilizzare[i]
    cat("nome ",i, "aggiustato: da ", daModificare, " a ", daUtilizzare[i], "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}
if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}

# ripeto merge
invecchiamento_op_camera <- left_join(invecchiamento_op, df)

# ora modifiche manuali
sum(is.na(invecchiamento_op_camera[invecchiamento_op_camera$camera == "camera", "genere"]))  # 1 failed

idx <- is.na(invecchiamento_op_camera$genere) & invecchiamento_op_camera$camera == "camera"
invecchiamento_op_camera[idx, "parlamentare"]
grep("IANNUZZI", df$parlamentare)
df[277,"parlamentare"]
df[df$parlamentare == "IANNUZZI Barbato", "parlamentare"] <- "IANNUZZI Tino"

# ripeto merge
invecchiamento_op_camera <- left_join(invecchiamento_op, df)
# controllo deputati
sum(is.na(invecchiamento_op_camera[invecchiamento_op_camera$camera == "camera", "genere"]))  # ok!

# aggiungo id_intergruppo
intergruppi[intergruppi$denominazione_corta=="invecchiamento attivo", "id_intergruppo"]
invecchiamento_op_camera$id_intergruppo <- "0020"
str(invecchiamento_op_camera)
invecchiamento_fin <- invecchiamento_op_camera[,c(1,19,2:10,13:17)]
str(invecchiamento_fin)
any(is.na(invecchiamento_fin$id_intergruppo))













# -------------------------------------
# mobilita ciclistica
# -------------------------------------

dir("./mobilita_ciclistica")
bicicletta <- read.xlsx("./mobilita_ciclistica/aderenti_bicicletta.xlsx", sheetName = "fin")
str(bicicletta)
bicicletta$x <- 1:nrow(bicicletta)
str(bicicletta)
bicicletta <- bicicletta[,c(3,4)]

# merge con OP
str(bicicletta)
str(parlamentari)
bicicletta$parlamentare <- as.character(bicicletta$parlamentare)
bicicletta_op <- left_join(bicicletta, parlamentari)
str(bicicletta_op)

# controllo failed merge
sum(is.na(bicicletta_op$camera))  # 11 failed merge

# apporto modifiche per ottenere full merge
idx <- is.na(bicicletta_op$camera)

# scrivo semplice funzione per automatizzare la modifica del nome quando ho un match con grep 1:1
daModificare <- bicicletta_op[idx,"parlamentare"]
cercaMatch <- strsplit(daModificare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], parlamentari$parlamentare)
  if(length(ctrl)==1) {
    cambio <- parlamentari[ctrl,"parlamentare"]
    bicicletta[bicicletta$parlamentare == daModificare[i], "parlamentare"] <- cambio
    cat("nome ",i, "aggiustato: da ", daModificare[i], " a ", cambio, "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}

if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}


# ripeto merge
bicicletta_op <- left_join(bicicletta, parlamentari)
idx <- is.na(bicicletta_op$camera)
bicicletta_op[idx,"parlamentare"] 
grep("CASELLATI", parlamentari$parlamentare)  ## la casellati elisabetta è decaduta per incompatibilita (eletta nel CSM)
grep("RIENZO", parlamentari$parlamentare)
parlamentari[193,"parlamentare"]
bicicletta[bicicletta$parlamentare == "DA RIENZO Vincenzo", "parlamentare"] <- "D'ARIENZO Vincenzo"
grep("DE CARO", parlamentari$parlamentare)  ## decaduto (sindaco di bari)
grep("MOGHERINI", parlamentari$parlamentare) # decaduta
grep("ROSSI Maurizio", parlamentari$parlamentare)
parlamentari[892,"parlamentare"]
bicicletta[bicicletta$parlamentare == "ROSSI Maurizio", "parlamentare"] <- "ROSSI Maurizio Giuseppe"


# ripeto merge
bicicletta_op <- left_join(bicicletta, parlamentari)
sum(is.na(bicicletta_op$camera))  # 3 failed merge per decadenza...ok!
str(bicicletta_op)
bicicletta_op <- bicicletta_op[,-2]


# --------------- merge bicicletta_op con dati.camera.it
str(bicicletta_op)
str(df)
bicicletta_op_camera <- left_join(bicicletta_op, df)

# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
sum(bicicletta_op_camera$camera == "senato", na.rm = TRUE)
sum(is.na(bicicletta_op_camera[bicicletta_op_camera$camera == "senato", "genere"]))  # ok, tutti e 14 (+3 decaduti) i senatori, come ci aspettavamo, falliscono il merge

sum(is.na(bicicletta_op_camera[bicicletta_op_camera$camera == "camera", "genere"]))  # 4 deputati falliscono il merge

# prima correzione con ciclo for
idx <- is.na(bicicletta_op_camera$genere) & bicicletta_op_camera$camera == "camera"
daUtilizzare <- bicicletta_op_camera[idx, "parlamentare"]
cercaMatch <- strsplit(daUtilizzare, split = " ")
residuo <- 0

for(i in 1:length(cercaMatch)) {
  ctrl <- grep(cercaMatch[[i]][1], df$parlamentare)
  if(length(ctrl)==1) {
    daModificare <- df[ctrl,"parlamentare"]
    df[df$parlamentare == daModificare, "parlamentare"] <- daUtilizzare[i]
    cat("nome ",i, "aggiustato: da ", daModificare, " a ", daUtilizzare[i], "\n")
  } else {
    residuo <- residuo + 1
    cat("nome ",i, "da ricontrollare", "\n")
  }
}
if(residuo>0) {
  warning(residuo, " nomi da ricontrollare")
} else {
  cat("Tutto modificato correttamente!")
}

# ripeto merge
bicicletta_op_camera <- left_join(bicicletta_op, df)

# ora modifiche manuali
sum(is.na(bicicletta_op_camera[bicicletta_op_camera$camera == "camera", "genere"]))  # 3 decaduti

# aggiungo id_intergruppo
intergruppi$denominazione_corta
intergruppi[intergruppi$denominazione_corta=="mobilità ciclabile", "id_intergruppo"]
bicicletta_op_camera$id_intergruppo <- "0022"
str(bicicletta_op_camera)
bicicletta_fin <- bicicletta_op_camera[,c(1,19,2:10,13:17)]
str(bicicletta_fin)
any(is.na(bicicletta_fin$id_intergruppo))
bicicletta_fin









# -------------------------------------
# montagna
# -------------------------------------

dir("./montagna")
montagna <- read.xlsx("./montagna/alcuni_aderenti_montagna.xlsx", sheetName = "fin")
str(montagna)
montagna$x <- 1:nrow(montagna)
str(montagna)
montagna <- montagna[,c(3,4)]

# merge con OP
str(montagna)
str(parlamentari)
montagna$parlamentare <- as.character(montagna$parlamentare)
montagna_op <- left_join(montagna, parlamentari)
str(montagna_op)

# controllo failed merge
sum(is.na(montagna_op$camera))  # ok!!!
montagna_op <- montagna_op[,-2]


# --------------- merge montagna_op con dati.camera.it
str(montagna_op)
str(df)
montagna_op_camera <- left_join(montagna_op, df)

# controllo 1. da camera.it ho info solo sui deputati quindi per i senatori il merge so gia che non c'è
sum(montagna_op_camera$camera == "senato", na.rm = TRUE)
sum(is.na(montagna_op_camera[montagna_op_camera$camera == "senato", "genere"]))  # ok, tutti e 3 i senatori, come ci aspettavamo, falliscono il merge
sum(is.na(montagna_op_camera[montagna_op_camera$camera == "camera", "genere"]))  # ok!
montagna_op_camera

# aggiungo id_intergruppo
intergruppi$denominazione_corta
intergruppi[intergruppi$denominazione_corta=="montagna", "id_intergruppo"]
montagna_op_camera$id_intergruppo <- "0023"
str(montagna_op_camera)
montagna_fin <- montagna_op_camera[,c(1,19,2:10,13:17)]
str(montagna_fin)
montagna_fin



## ------------------------ table finale aderenti

aderenti <- rbind(agenda_dig_fin,
                  bicicletta_fin,
                  cannabis_fin,
                  ecig_fin,
                  innovazione_fin,
                  invecchiamento_fin,
                  montagna_fin
)

str(aderenti)
head(aderenti)
any(is.na(aderenti$id_intergruppo))


## Save data
tmp.env1 <- new.env()
assign("aderenti", aderenti, pos=tmp.env1)
save(list=ls(all.names=TRUE, pos=tmp.env1), envir=tmp.env1, file="aderenti.RData")
rm(tmp.env1)

tmp.env2 <- new.env()
assign("dati.camera.it_clean", df, pos=tmp.env2)
save(list=ls(all.names=TRUE, pos=tmp.env2), envir=tmp.env2, file="dati.camera.it_clean.RData")
rm(tmp.env2)

tmp.env3 <- new.env()
assign("OP_parlamentari_clean", parlamentari, pos=tmp.env3)
save(list=ls(all.names=TRUE, pos=tmp.env3), envir=tmp.env3, file="parlamentari_clean.RData")
rm(tmp.env3)


                  





