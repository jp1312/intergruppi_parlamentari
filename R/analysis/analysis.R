## Analysis intergruppi parlamentari

rm(list=ls())

## Load data
getwd()
setwd("~/github/intergruppi_parlamentari")
dir("./data")
load("./data/aderenti.RData")
load("./data/table_intergruppi.RData")
load("./data/parlamentari_clean.RData")
load("./data/dati.camera.it.RData")
str(df)
  
# source code
source("./R/utils.R")


## packages
library(ggplot2)
library(data.table)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(tm)
library(SnowballC)
library(wordcloud)
library(extrafont)
library(extrafontdb)
library(grid)
library(gridExtra)
library(lattice)
library(plotrix)


# ------ Clean and merge datasets
sum_aderenti <- data.frame(table(aderenti$parlamentare))
any(is.na(aderenti$parlamentare))
sum_aderenti
names(sum_aderenti) <- c("parlamentare", "ngruppi")
sum_aderenti$parlamentare <- as.character(sum_aderenti$parlamentare)
parlam_inter <- left_join(OP_parlamentari_clean, sum_aderenti)
str(OP_parlamentari_clean)
deputati <- subset(parlam_inter, camera == "camera")
senatori <- subset(parlam_inter, camera == "senato")
any(senatori$parlamentare=="<NA>")
senatori$parlamentare

# set fonts
# this theme uses IMpact font. i need to translate it in windows first
windowsFonts(Impact=windowsFont("Impact"))
windowsFonts(Times=windowsFont("TT Times New Roman"))



# ------------------------------------------------------------
## overview intergruppi rilevati
# ------------------------------------------------------------


# --- Barplot 33 intergruppi by numero aderenti

d <- intergruppi
d[,.(categoria,denominazione_completa)]
sum(!is.na(d[,.(associazione_collegata)]))

names(d)
d[,c("categoria","totale_aderenti_parlamentari")]
d$denominazione_corta
d <- d[!is.na(d$totale_aderenti_parlamentari), ]
d$denominazione_corta <- factor(d$denominazione_corta, levels = d$denominazione_corta[order(d$totale_aderenti_parlamentari)])
d$totale_aderenti_parlamentari

p1 <- ggplot(d, aes(x = denominazione_corta, y = totale_aderenti_parlamentari)) +
  geom_bar(stat = "identity", color = "black", fill = "darkseagreen3") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Più o meno popolari") +
  scale_y_continuous(breaks=c(30, 80, 150, 300)) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) 

p1



# -------------- montagna

unique(aderenti$id_intergruppo)
intergruppi <- data.table(intergruppi)
intergruppi[, denominazione_corta]
intergruppi[denominazione_corta=="finanza sostenibile", .(denominazione_corta, totale_aderenti_parlamentari, attivita)]
intergruppi[denominazione_corta=="finanza sostenibile", id_intergruppo]
intergruppi[denominazione_corta=="innovazione", id_intergruppo]
intergruppi[, .(denominazione_corta,id_intergruppo)]

innovazione_all <- subset(aderenti, id_intergruppo=="0019")
aderenti$id_intergruppo
table(innovazione_all$Gruppo)
innovazione_all

intergruppi[id_intergruppo=="0023", denominazione_corta]
intergruppi[id_intergruppo=="0023", denominazione_completa]
intergruppi[id_intergruppo=="0023", totale_aderenti_parlamentari]

montagna_all <- subset(aderenti, id_intergruppo=="0023")
montagna_all





# ------------------- density for each gruppo 



















# ------------------------------------------------------------------------------------
# --------------- relazione tra appartenenza agli intergruppi e vita parlamentare
# ------------------------------------------------------------------------------------


## i parlamentari appartenenti a piu gruppi...

# deputati
nrow(deputati)
sum(!is.na(senatori[,"ngruppi"]))  # 64 deputati appartiene a un intergruppo
sum(!is.na(deputati[,"ngruppi"]))  # 197 deputati appartiene a un intergruppo
sum(!is.na(deputati[,"ngruppi"]))/nrow(deputati)  # quasi uno su tre!
64/320
197/630
mean(subset(deputati, ngruppi>0)$ngruppi)  # chi aderisce ad un intergruppo in media ne aderisce a piu di uno... 1,3 per la precisione
all(is.na(deputati[is.na(deputati[,"ngruppi"]),"ngruppi"]))
deputati[is.na(deputati[,"ngruppi"]),"ngruppi"] <- 0
summary(deputati[,"ngruppi"])
subset(deputati, ngruppi==4)  # medaglia d'oro a Pierpaolo Vargiu appartenente a 4 intergruppi
aderenti_inter <- left_join(aderenti, intergruppi)
subset(aderenti_inter, parlamentare == "VARGIU Pierpaolo") 
# agenda digitale
# cannabis
# innovazione
# invecchiamento attivo
# è un deputato di Cagliari, classe '57, eletto circoscrizione sardegna e appartenente al gruppo SCpI (Scelta Civica per l'Italia)
median(deputati$voti_ribelli)  # 52
subset(aderenti_inter, parlamentare == "VARGIU Pierpaolo")$voti_ribelli  # 288 (circa 4 volte superiore alla mediana)
median(deputati$indice_prod, na.rm = TRUE)  # 147.1
mean(deputati$indice_prod, na.rm = TRUE)  # 192.2
subset(aderenti_inter, parlamentare == "VARGIU Pierpaolo")$indice_prod  # 517.2 (circa due volte superiore alla media)
517.2/147.2-1
(517.2/192.2-1)*100
(517.2/147.2-1)*100
192+192
192*3
147*4
dep_perf_gruppo <- deputati[,.(indice_perf=mean(indice_prod, na.rm = TRUE)), by=Gruppo]
setorder(dep_perf_gruppo, indice_perf)
dep_perf_gruppo

str(senatori)
senatori <- data.table(senatori)
sen_perf_gruppo <- senatori[,.(indice_perf=mean(indice_prod, na.rm = TRUE)), by=Gruppo]
setorder(sen_perf_gruppo, indice_perf)
sen_perf_gruppo



str(deputati)
# ma per rispodenre alla domanda se veramente i parlamentari appartenenti a intergruppi sono piu ribelli e/o piu produttivi
# dobbiamo guardare alla loro interezza ... con un istogramma




# ------------------------------- relazione tra aderenza intergruppi e voti ribelli
# ------------------------------- relazione tra aderenza intergruppi e produttivita

# --- Camera
# costruzione dataset deputati che non appartengono a nessun intergruppo
dep <- data.frame(deputati)        # Full data set
dep$ngruppi <- as.factor(dep$ngruppi)
sum(is.na(dep$ngruppi)) # 433 deputati non aderiscono a intergruppi
dep$aderisce <- ifelse(is.na(dep$ngruppi),0,1)
table(dep$aderisce)

# in forma tabellare vedo meglio le cifre..
dep.dt <- data.table(dep)
dep.dt[,.(median(indice_prod, na.rm=T)), by = aderisce]
dep.dt[,.(mean(indice_prod, na.rm=T)), by = aderisce]
dep.dt[,.(sd(indice_prod, na.rm=T)), by = aderisce]


# confronto grafico
g1 <-ggplot(dep, aes(x = indice_prod, group = as.factor(aderisce), fill = as.factor(aderisce))) +
  geom_density(alpha = .4) +
  xlab("Indice di produttività Open polis") +
  ylab("") +
  ggtitle("Aderenti a intergruppi più produttivi?") +
  guides(fill=guide_legend("Aderisce?")) +
  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white")
  ) 

subtitle <- "Densità indice di produttività deputati"
ggplot_with_subtitle(g1, subtitle,
                       bottom_margin=20, lineheight=0.9) ## salvato manualmente da RStudio




# --- Senato
# costruzione dataset deputati che non appartengono a nessun intergruppo
sen <- data.frame(senatori)        # Full data set
sen$ngruppi <- as.factor(sen$ngruppi)
sum(is.na(sen$ngruppi)) # 256 deputati non aderiscono a intergruppi
sen$aderisce <- ifelse(is.na(sen$ngruppi),0,1)
table(sen$aderisce)

# in forma tabellare vedo meglio le cifre..
sen.dt <- data.table(sen)
sen.dt[,.(median(indice_prod, na.rm=T)), by = aderisce]
sen.dt[,.(mean(indice_prod, na.rm=T)), by = aderisce]
sen.dt[,.(sd(indice_prod, na.rm=T)), by = aderisce]


# confronto grafico
g2 <-ggplot(sen, aes(x = indice_prod, group = as.factor(aderisce), fill = as.factor(aderisce))) +
  geom_density(alpha = .4) +
  xlab("Indice di produttività Open polis") +
  ylab("") +
  ggtitle("Senatori aderenti a intergruppi più produttivi?") +
  guides(fill=guide_legend("Aderisce?")) +
  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white")
  ) 

subtitle <- "Densità indice di produttività senatori"
ggplot_with_subtitle(g2, subtitle,
                     bottom_margin=20, lineheight=0.9) ## salvato manualmente da RStudio



# ----- faceted by camera e senato
dep.small <- dep[,c("indice_prod", "voti_ribelli", "aderisce")]
dep.small$camera <- rep("Camera", nrow(dep.small))
sen.small <- sen[,c("indice_prod", "voti_ribelli", "aderisce")]
sen.small$camera <- rep("Senato", nrow(sen.small))
parlam.small <- rbind(dep.small, sen.small)

# confronto grafico
g3 <-ggplot(parlam.small, aes(x = indice_prod, group = as.factor(aderisce), fill = as.factor(aderisce))) +
  geom_density(alpha = .4) +
  facet_grid(~camera) +
  xlab("Indice di produttività Open polis") +
  ylab("") +
  ggtitle("Parlamentari aderenti a intergruppi più produttivi?") +
  guides(fill=guide_legend("Aderisce?")) +
  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Times", size = 14)
        
  ) 

subtitle <- "Densità indice di produttività parlamentari"
ggplot_with_subtitle(g3, subtitle,
                     bottom_margin=20, lineheight=0.9) ## salvato manualmente da RStudio


# ----- Per gruppi parlamentari -----
unique(aderenti$id_intergruppo)

# 0003 - agenda digitale
intergruppi[id_intergruppo=="0003",denominazione_corta]
dig <- aderenti[aderenti$id_intergruppo=="0003","parlamentare"]
dig <- data.frame(parlamentare = dig, aderisce = rep(1, length(dig)))
dig
dig$parlamentare <- as.character(dig$parlamentare)
dig_plus <- left_join(OP_parlamentari_clean, dig)
dig_dep <- subset(dig_plus, camera == "camera")
dig_sen <- subset(dig_plus, camera == "senato")
dig_dep$intergruppo <- rep("Agenda digitale", nrow(dig_dep))
dig_dep[is.na(dig_dep$aderisce),"aderisce"] <- 0
head(dig_dep)
dig_sen$intergruppo <- rep("Agenda digitale", nrow(dig_sen))
dig_sen[is.na(dig_sen$aderisce),"aderisce"] <- 0
head(dig_sen)


# 0022 - mobilita ciclabile
intergruppi[id_intergruppo=="0022",denominazione_corta]
bik <- aderenti[aderenti$id_intergruppo=="0022","parlamentare"]
bik <- data.frame(parlamentare = bik, aderisce = rep(1, length(bik)))
bik
bik$parlamentare <- as.character(bik$parlamentare)
bik_plus <- left_join(OP_parlamentari_clean, bik)
bik_dep <- subset(bik_plus, camera == "camera")
bik_sen <- subset(bik_plus, camera == "senato")
bik_dep$intergruppo <- rep("Mobilità ciclabile", nrow(bik_dep))
bik_dep[is.na(bik_dep$aderisce),"aderisce"] <- 0
head(bik_dep)
bik_sen$intergruppo <- rep("Mobilità ciclabile", nrow(bik_sen))
bik_sen[is.na(bik_sen$aderisce),"aderisce"] <- 0
head(bik_sen)



# 0007 - cannabis
intergruppi[id_intergruppo=="0007",denominazione_corta]
can <- aderenti[aderenti$id_intergruppo=="0007","parlamentare"]
can <- data.frame(parlamentare = can, aderisce = rep(1, length(can)))
can
can$parlamentare <- as.character(can$parlamentare)
can_plus <- left_join(OP_parlamentari_clean, can)
can_dep <- subset(can_plus, camera == "camera")
can_sen <- subset(can_plus, camera == "senato")
can_dep$intergruppo <- rep("Cannabis legale", nrow(can_dep))
can_dep[is.na(can_dep$aderisce),"aderisce"] <- 0
head(can_dep)
can_sen$intergruppo <- rep("Cannabis legale", nrow(can_sen))
can_sen[is.na(can_sen$aderisce),"aderisce"] <- 0
head(can_sen)




# 0012 - ecig
intergruppi[id_intergruppo=="0012",denominazione_corta]
ecig <- aderenti[aderenti$id_intergruppo=="0012","parlamentare"]
ecig <- data.frame(parlamentare = ecig, aderisce = rep(1, length(ecig)))
ecig
ecig$parlamentare <- as.character(ecig$parlamentare)
ecig_plus <- left_join(OP_parlamentari_clean, ecig)
ecig_dep <- subset(ecig_plus, camera == "camera")
ecig_sen <- subset(ecig_plus, camera == "senato")
ecig_dep$intergruppo <- rep("Sigarette elettroniche", nrow(ecig_dep))
ecig_dep[is.na(ecig_dep$aderisce),"aderisce"] <- 0
head(ecig_dep)
ecig_sen$intergruppo <- rep("Sigarette elettroniche", nrow(ecig_sen))
ecig_sen[is.na(ecig_sen$aderisce),"aderisce"] <- 0
head(ecig_sen)



# 0019 - innovazione
intergruppi[id_intergruppo=="0019",denominazione_corta]
inn <- aderenti[aderenti$id_intergruppo=="0019","parlamentare"]
inn <- data.frame(parlamentare = inn, aderisce = rep(1, length(inn)))
inn
inn$parlamentare <- as.character(inn$parlamentare)
inn_plus <- left_join(OP_parlamentari_clean, inn)
inn_dep <- subset(inn_plus, camera == "camera")
inn_sen <- subset(inn_plus, camera == "senato")
inn_dep$intergruppo <- rep("Innovazione", nrow(inn_dep))
inn_dep[is.na(inn_dep$aderisce),"aderisce"] <- 0
head(inn_dep)
inn_sen$intergruppo <- rep("Innovazione", nrow(inn_sen))
inn_sen[is.na(inn_sen$aderisce),"aderisce"] <- 0
head(inn_sen)



# 0020 - invecchimento attivo
intergruppi[id_intergruppo=="0020",denominazione_corta]
inv <- aderenti[aderenti$id_intergruppo=="0020","parlamentare"]
inv <- data.frame(parlamentare = inv, aderisce = rep(1, length(inv)))
inv
inv$parlamentare <- as.character(inv$parlamentare)
inv_plus <- left_join(OP_parlamentari_clean, inv)
inv_dep <- subset(inv_plus, camera == "camera")
inv_sen <- subset(inv_plus, camera == "senato")
inv_dep$intergruppo <- rep("Invecchiamento attivo", nrow(inv_dep))
inv_dep[is.na(inv_dep$aderisce),"aderisce"] <- 0
head(inv_dep)
inv_sen$intergruppo <- rep("Invecchiamento attivo", nrow(inv_sen))
inv_sen[is.na(inv_sen$aderisce),"aderisce"] <- 0
head(inv_sen)
# --- ma gli aderenti sono davvero piu anziani delle media
check_inv <- aderenti[aderenti$id_intergruppo=="0020",]
names(check_inv)
2016-mean(as.numeric(substr(x = check_inv$dataNascita, start = 1, stop = 4)),na.rm=T)  # 54.5
2016-mean(as.numeric(substr(x = df$dataNascita, start = 1, stop = 4)),na.rm=T)  # 49.1



# ----- append
head(inv_dep)
dep.g <- rbind(
  dig_dep,
  bik_dep,
  can_dep,
  ecig_dep,
  inn_dep,
  inv_dep
  )

sen.g <- rbind(
  dig_sen,
  bik_sen,
  can_sen,
  ecig_sen,
  inn_sen,
  inv_sen
)



# confronto grafico - dep
gg1 <-ggplot(dep.g, aes(x = indice_prod, group = as.factor(aderisce), fill = as.factor(aderisce))) +
  geom_density(alpha = .4) +
  facet_wrap(~intergruppo, ncol=2) +
  xlab("Indice di produttività Open polis") +
  ylab("") +
  ggtitle("Più produttivi, indipendentemente dall'intergruppo") +
  guides(fill=guide_legend("Aderisce?")) +
  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Times", size = 14)
        
  ) 
gg1
subtitle <- "Densità indice di produttività deputati"
ggplot_with_subtitle(gg1, subtitle,
                     bottom_margin=20, lineheight=0.9) ## salvato manualmente da RStudio



# confronto grafico - sen
gg2 <-ggplot(sen.g, aes(x = indice_prod, group = as.factor(aderisce), fill = as.factor(aderisce))) +
  geom_density(alpha = .4) +
  facet_wrap(~intergruppo, ncol=2) +
  xlab("Indice di produttività Open polis") +
  ylab("") +
  ggtitle("Idem al Senato") +
  guides(fill=guide_legend("Aderisce?")) +
  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Times", size = 14)
        
  ) 
gg2
subtitle <- "Densità indice di produttività senatori"
ggplot_with_subtitle(gg2, subtitle,
                     bottom_margin=20, lineheight=0.9) ## salvato manualmente da RStudio





# solo PD
dep.g.pd <- subset(dep.g, Gruppo=="(PD)")
str(dep.g.pd)
dep.g.pd <- data.table(dep.g.pd)
dep.g.pd[,mean(indice_prod, na.rm=TRUE), by = .(aderisce)]

# confronto grafico - dep
ggpd <-ggplot(dep.g.pd, aes(x = indice_prod, group = as.factor(aderisce), fill = as.factor(aderisce))) +
  geom_density(alpha = .4) +
  facet_wrap(~intergruppo, ncol=2) +
  xlab("Indice di produttività Open polis") +
  ylab("") +
  ggtitle("Più produttivi, indipendentemente dall'intergruppo") +
  guides(fill=guide_legend("Aderisce?")) +
  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Times", size = 14)
        
  ) 
ggpd
subtitle <- "Densità indice di produttività deputati"
ggplot_with_subtitle(gg1, subtitle,
                     bottom_margin=20, lineheight=0.9) ## salvato manualmente da RStudio






# ------------------ focus cannabis

str(aderenti)
table(aderenti[aderenti$id_intergruppo=="0007", "Gruppo"])
nrow(deputati[deputati$Gruppo=="(SI-SEL)",])
25/38
sum(isTRUE(intergruppi$sito_web))
isT
intergruppi[intergruppi$sito_web==TRUE,]







# --------------- coloured matrix to visualize position of each group

# imposto matrice
table(OP_parlamentari_clean$Gruppo)

spettro <- c(
  levels(OP_parlamentari_clean$Gruppo)[10],
levels(OP_parlamentari_clean$Gruppo)[6],
levels(OP_parlamentari_clean$Gruppo)[8],
levels(OP_parlamentari_clean$Gruppo)[12],
levels(OP_parlamentari_clean$Gruppo)[2],
levels(OP_parlamentari_clean$Gruppo)[7],
levels(OP_parlamentari_clean$Gruppo)[9],
levels(OP_parlamentari_clean$Gruppo)[1],
levels(OP_parlamentari_clean$Gruppo)[11],
levels(OP_parlamentari_clean$Gruppo)[13],
levels(OP_parlamentari_clean$Gruppo)[4],
levels(OP_parlamentari_clean$Gruppo)[14],
levels(OP_parlamentari_clean$Gruppo)[5],
levels(OP_parlamentari_clean$Gruppo)[3])


spettro_rename <- c(
  "SI-SEL",
  "M5S",
  "PD",
  "Autonomie",
  "DS-CD",
  "Misto",
  "SCpI",
  "NCD-UDC",
  "ALA",
  "CoR",
  "FI-PdL",
  "GAL",
  "Lega",
  "FdI")

lookup= data.frame( 
  Gruppo=spettro, 
  newname=spettro_rename,
  idx_spettro=seq_along(spettro_rename),
  stringsAsFactors = TRUE)

str(lookup)
class(OP_parlamentari_clean$Gruppo)
str(OP_parlamentari_clean$Gruppo)
levels(lookup$Gruppo)
levels(OP_parlamentari_clean$Gruppo)
pp <- left_join(OP_parlamentari_clean, lookup)
table(pp[,c("Gruppo", "newname")])
table(pp[,c("Gruppo", "idx_spettro")])



# -- mobilita ciclabile

intergruppi[,.(id_intergruppo, denominazione_corta)]
bike <- with(subset(aderenti, id_intergruppo=="0022"), parlamentare)
bike <- data.frame(parlamentare = bike)
bike$x <- rep(0,nrow(bike))
str(bike)
bike$parlamentare <- as.character(bike$parlamentare)
bike <- left_join(pp, bike)
t <- table(bike[!is.na(bike$x),"idx_spettro"])
t
idx_bike <- as.character(1:14) %in% dimnames(t)[[1]]
bike_i <- rep(NA,14)
bike_i[idx_bike] <- as.vector(t)
bike_i[!idx_bike] <- 0
bike_i


# -- agenda digitale
unique(aderenti$id_intergruppo)
intergruppi[id_intergruppo=="0003", denominazione_corta]
digitale <- with(subset(aderenti, id_intergruppo=="0003"), parlamentare)
digitale <- data.frame(parlamentare = digitale)
digitale$x <- rep(0,nrow(digitale))
str(digitale)
digitale$parlamentare <- as.character(digitale$parlamentare)
digitale <- left_join(pp, digitale)
t <- table(digitale[!is.na(digitale$x),"idx_spettro"])
t
idx_digitale <- as.character(1:14) %in% dimnames(t)[[1]]
digitale_i <- rep(NA,14)
digitale_i[idx_digitale] <- as.vector(t)
digitale_i[!idx_digitale] <- 0
digitale_i
table(aderenti[aderenti$id_intergruppo=="0003", "Gruppo"]) # check



# -- cannabis
unique(aderenti$id_intergruppo)
intergruppi[id_intergruppo=="0007", denominazione_corta]
cannabis <- with(subset(aderenti, id_intergruppo=="0007"), parlamentare)
cannabis <- data.frame(parlamentare = cannabis)
cannabis$x <- rep(0,nrow(cannabis))
str(cannabis)
cannabis$parlamentare <- as.character(cannabis$parlamentare)
cannabis <- left_join(pp, cannabis)
t <- table(cannabis[!is.na(cannabis$x),"idx_spettro"])
t
idx_cannabis <- as.character(1:14) %in% dimnames(t)[[1]]
cannabis_i <- rep(NA,14)
cannabis_i[idx_cannabis] <- as.vector(t)
cannabis_i[!idx_cannabis] <- 0
cannabis_i
table(aderenti[aderenti$id_intergruppo=="0007", "Gruppo"]) # check





# -- ecig
unique(aderenti$id_intergruppo)
intergruppi[id_intergruppo=="0012", denominazione_corta]
ecig <- with(subset(aderenti, id_intergruppo=="0012"), parlamentare)
ecig <- data.frame(parlamentare = ecig)
ecig$x <- rep(0,nrow(ecig))
str(ecig)
ecig$parlamentare <- as.character(ecig$parlamentare)
ecig <- left_join(pp, ecig)
t <- table(ecig[!is.na(ecig$x),"idx_spettro"])
t
idx_ecig <- as.character(1:14) %in% dimnames(t)[[1]]
ecig_i <- rep(NA,14)
ecig_i[idx_ecig] <- as.vector(t)
ecig_i[!idx_ecig] <- 0
ecig_i
table(aderenti[aderenti$id_intergruppo=="0012", "Gruppo"]) # check


# -- innovazione
unique(aderenti$id_intergruppo)
intergruppi[id_intergruppo=="0019", denominazione_corta]
innovazione <- with(subset(aderenti, id_intergruppo=="0019"), parlamentare)
innovazione <- data.frame(parlamentare = innovazione)
innovazione$x <- rep(0,nrow(innovazione))
str(innovazione)
innovazione$parlamentare <- as.character(innovazione$parlamentare)
innovazione <- left_join(pp, innovazione)
t <- table(innovazione[!is.na(innovazione$x),"idx_spettro"])
t
idx_innovazione <- as.character(1:14) %in% dimnames(t)[[1]]
innovazione_i <- rep(NA,14)
innovazione_i[idx_innovazione] <- as.vector(t)
innovazione_i[!idx_innovazione] <- 0
innovazione_i
table(aderenti[aderenti$id_intergruppo=="0019", "Gruppo"]) # check



# -- invecchiamento
unique(aderenti$id_intergruppo)
intergruppi[id_intergruppo=="0020", denominazione_corta]
invecchiamento <- with(subset(aderenti, id_intergruppo=="0020"), parlamentare)
invecchiamento <- data.frame(parlamentare = invecchiamento)
invecchiamento$x <- rep(0,nrow(invecchiamento))
str(invecchiamento)
invecchiamento$parlamentare <- as.character(invecchiamento$parlamentare)
invecchiamento <- left_join(pp, invecchiamento)
t <- table(invecchiamento[!is.na(invecchiamento$x),"idx_spettro"])
t
idx_invecchiamento <- as.character(1:14) %in% dimnames(t)[[1]]
invecchiamento_i <- rep(NA,14)
invecchiamento_i[idx_invecchiamento] <- as.vector(t)
invecchiamento_i[!idx_invecchiamento] <- 0
invecchiamento_i
table(aderenti[aderenti$id_intergruppo=="0020", "Gruppo"]) # check


# -- montagna
unique(aderenti$id_intergruppo)
intergruppi[id_intergruppo=="0023", denominazione_corta]
intergruppi[id_intergruppo=="0023", denominazione_completa]

montagna <- with(subset(aderenti, id_intergruppo=="0023"), parlamentare)
montagna <- data.frame(parlamentare = montagna)
montagna$x <- rep(0,nrow(montagna))
str(montagna)
montagna$parlamentare <- as.character(montagna$parlamentare)
montagna <- left_join(pp, montagna)
t <- table(montagna[!is.na(montagna$x),"idx_spettro"])
t
idx_montagna <- as.character(1:14) %in% dimnames(t)[[1]]
montagna_i <- rep(NA,14)
montagna_i[idx_montagna] <- as.vector(t)
montagna_i[!idx_montagna] <- 0
montagna_i
table(aderenti[aderenti$id_intergruppo=="0023", "Gruppo"]) # check

head(aderenti[aderenti$id_intergruppo=="0012",])
dd <- rbind(
  bike_i,
  digitale_i,
  cannabis_i,
  ecig_i,
  innovazione_i,
  invecchiamento_i,
  montagna_i)

dd
dimnames(dd)
dimnames(dd)[[1]] <- c("Mobilità ciclabile", "Agenda digitale", "Cannabis", "e-cig", "Innovazione", "Invecchiamento attivo", "Sviluppo montagna*")
dimnames(dd)[[2]] <- spettro_rename
dd
dd_norm <- dd/rowSums(dd)
dd_norm
rowSums(dd_norm)  # ok!
colnames(dd)

windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Symbol")
)
par(mar=c(8, 8, 4.1, 2.1), bg = "white", family = "Times")  # default is c(5, 4, 4, 2)
color2D.matplot(dd_norm, c(0,1), c(0,0), c(0,0), show.legend=TRUE,
                xlab="",ylab="", font.main = 2, main="",
                font.main = 18,
                axes = FALSE, do.hex=FALSE, border=NA)

color2D.matplot(dd_norm, extremes = c("white", "turquoise"), show.legend=TRUE,
                xlab="",ylab="", font.main = 2, main="",
                font.main = 18,
                yrev = FALSE,
                axes = FALSE, do.hex=FALSE, border=NA)

?color2D.matplot
mtext("Grillini e pieddini alleati?",adj = 0, padj = -2, family = "Times", cex = 1.5)
mtext("Frequenza relativa provenienza aderenti per ogni intergruppo",adj = 0, padj = -0.7, family = "Times", cex = 1)

axis(1,at=1:14,labels=colnames(dd_norm), cex.axis = 0.8, las = 2)
axis(2,at=1:7,labels=rownames(dd_norm), cex.axis = 0.8, las = 2)
par(mar=c(5, 4, 4, 2)) # reset default



# strapotere PD?
colMeans(dd_norm)
table(deputati$Gruppo)/630
table(senatori$Gruppo)/320


head(deputati)
sum(table(deputati$Gruppo))
301/630 # 48% camera
table(senatori$Gruppo)
112/320 # 35% al senato 
dd_norm

