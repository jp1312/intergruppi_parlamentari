


# --- lista aderenti copiata e pulita su excel
library(xlsx)
aderenti_innovazione <- read.xlsx("./www/innovazione/aderenti.xlsx", sheetName = "fin")
str(aderenti_innovazione)

# sistemo parlamentare e email e twitter
aderenti_innovazione$Nome <- as.character(aderenti_innovazione$Nome)
aderenti_innovazione$Cognome <- as.character(aderenti_innovazione$Cognome)
aderenti_innovazione$parlamentare <- as.character(aderenti_innovazione$parlamentare)
aderenti_innovazione$Email <- as.character(aderenti_innovazione$Email)
aderenti_innovazione$Twitter <- as.character(aderenti_innovazione$Twitter)
str(aderenti_innovazione)

# controllo Nome
sum(aderenti_innovazione$Nome %in% c("NA", "controlla", ""))

# controllo Cognome
sum(aderenti_innovazione$Cognome %in% c("NA", "controlla", ""))

# controllo parlamenatre
sum(aderenti_innovazione$parlamentare %in% c("NA", "controlla", ""))

# controllo email
sum(aderenti_innovazione$Email %in% c("NA", "controlla", ""))

# controllo Gruppo
sum(aderenti_innovazione$Email %in% c("NA", "controlla", ""))

# controllo camera
sum(aderenti_innovazione$Camera %in% c("NA", "controlla", ""))

# controllo twitter
sum(aderenti_innovazione$Twitter %in% c("NA", "controlla", ""))
idx_na <- aderenti_innovazione$Twitter == "NA"
sum(idx_na)
aderenti_innovazione[idx_na, "Twitter"] <- NA

str(aderenti_innovazione)





# ---------------------- append deputati e senatori

getwd()
dir("./data")
load("./data/OP_deputati.RData")
load("./data/OP_senatori.RData")

# qualche controllo di coerenza
all ( names(OP_senatori) == names(OP_deputati) )
OP_parlamento <- rbind(OP_deputati, OP_senatori)
str(OP_parlamento)

# controlla circoscrizione
table(OP_parlamento$circoscrizione)
idx_err <- OP_parlamento$circoscrizione == "Campiania 2"
OP_parlamento[idx_err, "circoscrizione"] <- "Campania 2"

# controlla gruppo
table(OP_parlamento$Gruppo)



# ---------------------- merge Innovazione aderenti con OP_parlamento by name

library(dplyr)
?left_join
aderenti_innovazione_add <- left_join(x = aderenti_innovazione, y = OP_parlamento, by = "parlamentare") 
str(aderenti_innovazione_add)

sum(is.na(aderenti_innovazione_add$camera))
sum(is.na(aderenti_innovazione_add$Gruppo.y))
sum(is.na(aderenti_innovazione_add$followers_OP))
sum(is.na(aderenti_innovazione_add$circoscrizione))
sum(is.na(aderenti_innovazione_add$missioni_perc))  # chiaro che in 15 casi il merge non e andato a buon fine, controllo

idx_fail_merge <- is.na(aderenti_innovazione_add$missioni_perc)
head(aderenti_innovazione_add[idx_fail_merge,])  # sembrerebbe dovuto a formattazioni diverse etc.

# uno ad uno
# [1]
grep(pattern = "BOSSIO", OP_parlamento$parlamentare)
OP_parlamento[103, "parlamentare"]  # enza invece di vincenza ...

aderenti_innovazione[which(idx_fail_merge)[1],"parlamentare"] <- "BRUNO BOSSIO Enza"

# [2]
aderenti_innovazione[which(idx_fail_merge)[2],"parlamentare"]
grep(pattern = "CAPARINI", OP_parlamento$parlamentare)
OP_parlamento[117, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[2],"parlamentare"] <- "CAPARINI Davide"


# [3]
aderenti_innovazione[which(idx_fail_merge)[3],"parlamentare"]
grep(pattern = "CRIMI Vito", OP_parlamento$parlamentare)
OP_parlamento[715, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[3],"parlamentare"] <- "CRIMI Vito Claudio"


# [4]
aderenti_innovazione[which(idx_fail_merge)[4],"parlamentare"]
grep(pattern = "CURR", OP_parlamento$parlamentare)
OP_parlamento[188, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[4],"parlamentare"] <- "CURRO' Tommaso"


# [5]
aderenti_innovazione[which(idx_fail_merge)[5],"parlamentare"]
grep(pattern = "ALIA Gianpiero", OP_parlamento$parlamentare)
OP_parlamento[191, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[5],"parlamentare"] <- "D'ALIA Gianpiero"


# [6]
aderenti_innovazione[which(idx_fail_merge)[6],"parlamentare"]
grep(pattern = "GIORGI R", OP_parlamento$parlamentare)
OP_parlamento[739, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[6],"parlamentare"] <- "DI GIORGI Rosa Maria"


# [7]
aderenti_innovazione[which(idx_fail_merge)[7],"parlamentare"]
grep(pattern = "Federico", OP_parlamento$parlamentare)
OP_parlamento[195, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[7],"parlamentare"] <- "D'INCA' Federico"


# [8] ---> dove sta Galan ??
aderenti_innovazione[which(idx_fail_merge)[8],"parlamentare"]
grep(pattern = "Giancarlo", OP_parlamento$parlamentare)
OP_parlamento[909, "parlamentare"]

aderenti_innovazione[which(idx_fail_merge)[7],"parlamentare"] <- ""




