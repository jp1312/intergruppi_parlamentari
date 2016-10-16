intergruppi
intergruppi[interdenominazione_corta
            intergruppi$conta <- 1
            hist(intergruppi$totale_aderenti_parlamentari)
            sum(!is.na(intergruppi$totale_aderenti_parlamentari))
            intergruppi[!is.na(intergruppi$sottotitolo),"sottotitolo"]
            intergruppi[!is.na(intergruppi$attivita),"attivita"]
            unique(intergruppi$fonte)
            
            colors()
            intergruppi <- data.table(intergruppi)
            ov <- intergruppi[,.('N. intergruppi' = length(id_intergruppo)), by = categoria]
            setorder(ov, -'N. intergruppi')
            ov$categoria <- c("Società", "Esteri", "Tecnologia", "Territorio", "Finanza", "Business", "Religione", "Cultura")
            ov
            unique(intergruppi$categoria) %in% ov$Categoria
            
            
            
# wordclouds with denominazione_completa, sottotitolo, attivita
            
names(intergruppi)

            interg_words <- with(intergruppi, paste(denominazione_completa, sottotitolo, categoria, attivita, sep = " "))
            intergCorpus <- Corpus(VectorSource(interg_words)) # create a corpus
            intergCorpus <- tm_map(intergCorpus, PlainTextDocument) # convert corpus into plain text
            intergCorpus <- tm_map(intergCorpus, removePunctuation) # remove punctuation
            intergCorpus <- tm_map(intergCorpus, removeWords, c("intergruppo", "intergruppi", "parlamentar", "parlamentari", "deputati", stopwords('italian')))
            # intergCorpus <- tm_map(intergCorpus, stemDocument)
            wordcloud(intergCorpus, max.words = 100, random.order = FALSE)
            


str(intergruppi)
intergruppi$denominazione_corta
intergruppi$denominazione_completa
names(intergruppi)
with(subset(intergruppi, denominazione_corta=="sussidiarieta"), totale_aderenti_parlamentari)
with(subset(intergruppi, denominazione_corta=="e-cig"), totale_aderenti_parlamentari)
with(subset(intergruppi, denominazione_corta=="e-cig"), attivita)
with(subset(intergruppi, denominazione_corta=="e-cig"), riferimento)



library(grid)
getwd()
png("./output/Infographics1.png", width = 20, height = 20, units = "in", res = 500)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 2)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INTERGRUPPI", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Times", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text("PARLAMENTARI", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Times", col = "#E7A922", cex = 6.4))
#grid.text("By J. Primavera", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "Times", col = "#552683", cex = 0.8))
#grid.text("ANALYSIS WITH PROGRAMMING", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
#grid.text("alstatr.blogspot.com", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
print(p3_kobe, vp = vplayout(2, 1:2))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
#grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Times", col = "#CA8B01", cex = 13, alpha = 0.3))
# grid.text("Intergruppi parlamentari", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Times", col = "#552683", cex = 1.2))
grid.text("Rilevati tramite ricerca sul web", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Impact", col = "white", cex = 1.2))
intergruppi$denominazione_completa
grid.text(paste(intergruppi$denominazione_completa,
                sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Times", col = "#552683", cex = 0.8))
#grid.text(paste(
#  "http://alstatr.blogspot.com",
#  "http://alstatr.blogspot.com",
#  "Analysis with Programming",
#  "Al-Ahmadgaid B. Asaad",
#  "Annually",
#  "National",
#  "2011-2013", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
dev.off()





# Generate Infographic in PDF format

library(grid)
getwd()
png("./output/Infographics1.png", width = 20, height = 20, units = "in", res = 500)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 1)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INTERGRUPPI", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Times", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text("PARLAMENTARI", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Times", col = "#E7A922", cex = 6.4))
intergruppi$denominazione_completa
grid.text("Rilevati tramite ricerca sul web", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Impact", col = "white", cex = 1.2))
grid.text(paste(
  "2.0",                                          
  "Per l'acqua bene comune",                      
  "Per l'agenda digitale",                        
  "Per l'agenda urbana",                          
  "Amicizia Italia-Armenia",                      
  "Amici della birmania",                         
  "Cannabis legale",                              
  "Contro i cambiamenti climatici",               
  "Per legge contro diffusione gioco d'azzardo",  
  "Per la cooperazione allo sviluppo",            
  "Per le donne, i diritti e le pari opportunità",
  "Sigarette elettroniche",                       
  "Eutanasia e il testamento biologico",        
  "Parlamentari evangelici",                  
  "Federalista per la costituzione europea",      
  "Finanza sostenibile",                          
  "Giovani parlamentari",                         
  "Per il Giubileo",                              
  "Per l'innovazione tecnologica",                
  "Per l'invecchiamento attivo",                  
  "Amicizia Italia - Kurdistan iracheno",      
  "Per la mobilità ciclabile",                    
  "Per lo sviluppo della montagna",               
  "Per la musica",                                
  "Per la pace",                                  
  "Per la tutela delle piccole imprese balneari", 
  "Amicizia col popolo rom",                      
  "Amici del popolo Sarahwi",                     
  "Sui problemi sociali dell'Ictus",              
  "Per la sussidiarieta",                         
  "Per il Tibet",                                 
  "Amici del tiro, della caccia e della pesca",   
  "Per la Via Francigena",
  sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Times", col = "#552683", cex = 4))
dev.off()





x<-matrix(rnorm(1024)+sin(seq(0,2*pi,length=1024)),nrow=32)
install.packages("plotrix")
library(plotrix)
color2D.matplot(x,c(1,0),c(0,0),c(0,1),show.legend=TRUE,
                xlab="Columns",ylab="Rows",main="2D matrix plot")

# generate colors that show negative values in red and positive in green
cellcol<-matrix(rep("#000000",1024),nrow=32)
cellcol[x<0]<-color.scale(x[x<0],c(1,0.8),c(0,0.8),0)
cellcol[x>0]<-color.scale(x[x>0],0,c(0.8,1),c(0.8,0))

# now do hexagons without borders
color2D.matplot(x,cellcolors=cellcol,xlab="Columns",ylab="Rows",
                do.hex=TRUE,main="2D matrix plot (hexagons)",border=NA)

# for this one, we have to do the color legend separately
# because of the two part color scaling

legval<-seq(min(x),max(x),length.out=6)
legcol<-rep("#000000",6)
legcol[legval<0]<-color.scale(legval[legval<0],c(1,0.8),c(0,0.8),0)
legcol[legval>0]<-color.scale(legval[legval>0],0,c(0.8,1),c(0.8,0))
color.legend(0,-5,6,-4,round(c(min(x),0,max(x)),1),rect.col=legcol)
# do a color only association plot
xt<-table(sample(1:10,100,TRUE),sample(1:10,100,TRUE))
observed<-xt[,rev(1:dim(xt)[2])]
expected<-outer(rowSums(observed),colSums(observed),"*")/sum(xt)
deviates<-(observed-expected)/sqrt(expected)
cellcol<-matrix(rep("#000000",100),nrow=10)
cellcol[deviates<0]<-
  color.scale(deviates[deviates<0],c(1,0.8),c(0,0.5),0)
cellcol[deviates>0]<-
  color.scale(deviates[deviates>0],0,c(0.7,0.8),c(0.5,0))
color2D.matplot(x=round(deviates,2),cellcolors=cellcol,
                show.values=TRUE,main="Association plot")





dd <- data.frame(
  bike_i,
  digitale_i,
  cannabis_i,
  ecig_i,
  innovazione_i,
  invecchiamento_i,
  montagna_i)

dd
dd <- t(dd)
dd <- as.data.frame(dd)
dd
str(dd)
names(dd)
row.names(dd)
names(dd) <- spettro_rename
row.names(dd) <- c("Mobilità ciclabile", "Agenda digitale", "Cannabis", "e-cig", "Innovazione", "Invecchiamento attivo", "Sviluppo montagna")
dd
dd_norm <- dd/rowSums(dd)
dd_norm
rowSums(dd_norm)  # ok!


?color2D.matplot
dd_norm
color2D.matplot(dd_norm, c(0,1), c(0,0), c(1,0), show.legend=TRUE,
                xlab="",ylab="",main="Intergruppi VS Spettro politica italiana")
color2D.matplot(dd_norm, show.legend=TRUE,
                xlab="",ylab="",main="Intergruppi VS Spettro politica italiana")

dd_norm
color2D.matplot4




# --- Barplot 33 intergruppi by category

d <- intergruppi
d[,c("categoria","denominazione_corta")]
levels(d$categoria)
levels(d$categoria) <- c("Business", "Cultura", "Finanza", "Esteri", "Religione", "Sociale", "Tecnologia", "Territorio")
d[,c("categoria","denominazione_corta")]
d$categoria <- factor(d$categoria, levels=c("Cultura", "Religione", "Finanza", "Business", "Territorio", "Tecnologia", "Esteri", "Sociale"))
table(d$categoria)
names(d)

p1 <- ggplot(d, aes(x = categoria)) +
  geom_bar(stat = "count", color = "black", fill = "#552683") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Ce n'è per tutti") +
  theme(plot.title = element_text(hjust = 0, vjust=5)) +
  scale_y_continuous(breaks=c(2, 3, 7, 13))
p1
p1_kobe <- p1 + kobe_theme() 
p1_kobe
subtitle <- "Numero di intergruppi per categoria, sui 33 individuati tramite ricerca sul web"
ggplot_with_subtitle(p1_kobe, subtitle,
                     bottom_margin=20, lineheight=0.9, col = "#552683") ## salvato manualmente da RStudio






# --- Pie deputati aderenti per gruppo faceted by camera e senato
# -- deputati first

str(parlam_inter)
d2cam <- parlam_inter[parlam_inter$camera=="camera",]
str(d2cam)
sum(is.na(d2cam$Gruppo))  # elimino 8 deputati per i quali non ho info sul gruppo
d2cam <- d2cam[!is.na(d2cam$Gruppo),]
denom_cam <- table(d2cam$Gruppo)  # fattore di normalizzazione...
denom_cam
denom_cam <- denom_cam[denom_cam>0]
numer_cam <- table(d2cam[!is.na(d2cam$ngruppi), "Gruppo"])
numer_cam
idx_cam <- names(numer_cam) %in% names(denom_cam)
numer_cam <- numer_cam[idx_cam]
length(numer_cam)
length(denom_cam)
gruppi_cam <- names(denom_cam)
gruppi_cam
gruppi_cam <- c("NCD-UDC", "DS-CD", "FdI", "FI-PdL", "Lega", "M5S", "Misto", "PD", "SCpI", "SI-SEL")
denom_cam <- as.numeric(denom_cam)
numer_cam <- as.numeric(numer_cam)
perc_aderenza_cam <- numer_cam/denom_cam
perc_aderenza_cam
cam_perc_ader <- data.frame(Gruppo = gruppi_cam, 'perc_aderenza' = perc_aderenza_cam)
cam_perc_ader
cam_perc_ader$camera <- rep("Camera", nrow(cam_perc_ader))

# -- now senatori

d2sen <- parlam_inter[parlam_inter$camera=="senato",]
str(d2sen)
sum(is.na(d2sen$Gruppo))  # elimino 8 senatori per i quali non ho info sul gruppo
d2sen <- d2sen[!is.na(d2sen$Gruppo),]
denom_sen <- table(d2sen$Gruppo)  # fattore di normalizzazione...
denom_sen
denom_sen <- denom_sen[denom_sen>0]
numer_sen <- table(d2sen[!is.na(d2sen$ngruppi), "Gruppo"])
numer_sen
idx_sen <- names(numer_sen) %in% names(denom_sen)
numer_sen <- numer_sen[idx_sen]
length(numer_sen)
length(denom_sen)
gruppi_sen <- names(denom_sen)
gruppi_sen
gruppi_sen <- c("NCD-UDC", "FI-PdL", "Lega", "M5S", "Misto", "PD", "AL-A", "Autonomie", "CoR", "GAL")
denom_sen <- as.numeric(denom_sen)
numer_sen <- as.numeric(numer_sen)
perc_aderenza_sen <- numer_sen/denom_sen
perc_aderenza_sen
sen_perc_ader <- data.frame(Gruppo = gruppi_sen, 'perc_aderenza' = perc_aderenza_sen)
sen_perc_ader
sen_perc_ader$camera <- rep("Senato", nrow(sen_perc_ader))


# Unire camera e senato
parlam_perc_ader <- rbind(cam_perc_ader, sen_perc_ader)
head(parlam_perc_ader)

p3 <- ggplot(parlam_perc_ader, aes(x = Gruppo, y = perc_aderenza)) +
  geom_bar(stat = "identity", color = "black", fill = "#552683") +
  coord_polar() +
  facet_grid(~camera) +
  xlab("") +
  ylab("") +
  ggtitle("Una questione di Sinistra?") +
  theme(plot.title = element_text(hjust = 0, vjust=5))

p3
p3_kobe <- p3 + kobe_theme2() 
p3_kobe
subtitle <- "Propensione dei parlamentari ad aderire a intergruppi, per Gruppi parlamentari e camera di appartenenza"
ggplot_with_subtitle(p3_kobe, subtitle,
                     bottom_margin=20, lineheight=0.9, col = "#552683") ## salvato manualmente da RStudio




# a che intergruppi appartengono quelli di SEL???
# piu di 8 deputati su 10 appartengono a un'intergruppo!
str(aderenti)
table(aderenti[aderenti$Gruppo=="(SI-SEL)", "id_intergruppo"])
intergruppi[intergruppi$id_intergruppo=="0007","denominazione_corta"]
# 25 di SEL aderiscono all'intergruppo cannabis!

# controlla what's going on in aderenti table...
table(aderenti$id_intergruppo)
aderenti$id_intergruppo

# create infographics joining p1, p2 and p3 in some nice background






# ------------------ altre info overview (quanti pubblicano info etc.)

intergruppi <- data.table(intergruppi)
intergruppi$denominazione_corta
intergruppi$attivita
names(intergruppi)
setkey(intergruppi, totale_aderenti_parlamentari)
intergruppi[,.(denominazione_corta, totale_aderenti_parlamentari, categoria)]

str(intergruppi)
intergruppi[denominazione_corta=="cannabis"]

# quanti pubblicano informazioni ?
# quanti lo fanno ma e stato difficile reperirle, scraping etc. ?
# alcuni gruppi sopravvivono alle legislature di altri non se ne ha piu notizia..
# di cosa si occupano? come possiamo categorizzarli?
# quali sono gli intergruppi piu misti? grafico
# quando rappresentano veramente un punto di contatto con la societa civile? quando sono legati as associazioni civili, come quella luca coscioni. no quando sono legati a pochi business . in quel caso sono lobby
# volendo abbozzare una genealogia degli intergruppi si potrebbe partire dai due estremi: e-cig e sussidiarieta ed analizzare poi cio che avviene nel mezzo





# --- Worldcloud denominazione corta

names(intergruppi)
interg_words <- intergruppi$denominazione_corta
intergCorpus <- Corpus(VectorSource(interg_words)) # create a corpus
intergCorpus <- tm_map(intergCorpus, PlainTextDocument) # convert corpus into plain text
intergCorpus <- tm_map(intergCorpus, removePunctuation) # remove punctuation
intergCorpus <- tm_map(intergCorpus, removeWords, c("gioco", "piccole", "attivo", "agenda", "testamento", "biologico", "via", "dazzardo", "azzardo", "contro", "pari", "opportuunita", "cambiamenti", "climatici", "bene", "popolo", "tiro", "sociali", "opportunita", stopwords('italian')))
# intergCorpus <- tm_map(intergCorpus, stemDocument)
par(bg = "#E2E2E3")
wordcloud(intergCorpus, max.words = 30, random.order = TRUE, col = "#552683")






# ------------------------------- relazione tra aderenza intergruppi e voti ribelli

str(deputati)
sum(is.na(deputati$voti_ribelli))  # ok, tutti i deputati hanno info su voti ribelli...
d <- data.frame(deputati)        # Full data set
d$ngruppi <- as.factor(d$ngruppi)
levels(d$ngruppi)
table(d$ngruppi)
d_bg <- d[, -11]  # Background Data - full without ngruppi info

# piu/meno ribelli ?
ggplot(d, aes(x = voti_ribelli, fill = ngruppi)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = .5) +
  geom_histogram(colour = "black") +
  facet_wrap(~ ngruppi) +
  guides(fill = FALSE) +  # to remove the legend
  theme_bw() +             # for clean look overall
  xlim(c(0,500))

str(deputati)
deputati <- data.table(deputati)
deputati$ngruppif <- factor(deputati$ngruppi, ordered = TRUE)
levels(deputati$ngruppif)
head(deputati$ngruppif)
deputati[,.(median(voti_ribelli, na.rm=T)), by = ngruppif]
deputati$ngruppif2 <- with(deputati, ifelse(ngruppi==0, 0, 1) )
deputati[,.(median(voti_ribelli, na.rm=T)), by = ngruppif2]
deputati[,.(mean(voti_ribelli, na.rm=T)), by = ngruppif2]
deputati[,.(sd(voti_ribelli, na.rm=T)), by = ngruppif2]
# l'ipotesi non e confermata!
# i deputati che appartengono a degli intergruppi non sono piu ribelli...
# d'altronde la variabilita e enorme tra coloro che non aderiscono a gruppi

