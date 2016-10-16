


# -------------------------------- connecting to dati.camera.it

# Not run
# install.packages("SPARQL")

library(SPARQL) # SPARQL querying package

# Step 1 - Set up preliminaries and define query
# Define the data.gov endpoint
endpoint <- "http://dati.camera.it/sparql"

# create query statement
query <-
  "SELECT DISTINCT ?persona ?cognome ?nome ?info
?dataNascita ?luogoNascita ?genere 
?collegio ?nomeGruppo ?sigla  COUNT(DISTINCT ?madatoCamera) as ?numeroMandati ?aggiornamento 
WHERE {
?persona ocd:rif_mandatoCamera ?mandato; a foaf:Person.

## deputato
?d a ocd:deputato; ocd:aderisce ?aderisce;
ocd:rif_leg <http://dati.camera.it/ocd/legislatura.rdf/repubblica_17>;
ocd:rif_mandatoCamera ?mandato.
OPTIONAL{?d dc:description ?info}

##anagrafica
?d foaf:surname ?cognome; foaf:gender ?genere;foaf:firstName ?nome.
OPTIONAL{
?persona <http://purl.org/vocab/bio/0.1/Birth> ?nascita.
?nascita <http://purl.org/vocab/bio/0.1/date> ?dataNascita; 
rdfs:label ?nato; ocd:rif_luogo ?luogoNascitaUri. 
?luogoNascitaUri dc:title ?luogoNascita. 
}


##aggiornamento del sistema
OPTIONAL{?d <http://lod.xdams.org/ontologies/ods/modified> ?aggiornamento.}

## mandato
?mandato ocd:rif_elezione ?elezione.  
MINUS{?mandato ocd:endDate ?fineMandato.}


## totale mandati
?persona ocd:rif_mandatoCamera ?madatoCamera.


## elezione
?elezione dc:coverage ?collegio.

## adesione a gruppo
?aderisce ocd:rif_gruppoParlamentare ?gruppo.
?gruppo <http://purl.org/dc/terms/alternative> ?sigla; 
dc:title ?nomeGruppo.
MINUS{?aderisce ocd:endDate ?fineAdesione}

}  "

# Step 2 - Use SPARQL package to submit query and save results to a data frame
qd <- SPARQL(endpoint,query)
str(qd)
df <- qd$results
str(df)

# elimino colonne non interessanti o già incluse nel DB di OP
df <- df[,-c(1, 8, 9, 10, 12)]
str(df)

# --- formatto nome come in OP (COGNOME Nome)
df$Nome <- paste(toupper(substring(df$nome, 1, 1)), tolower(substring(df$nome, 2)), sep = "")
df$parlamentare <- paste(df$cognome, df$Nome, sep = " ")
head(df$parlamentare)



rm(list=c("endpoint", "qd", "query"))
save.image("dati.camera.it.RData")


# ------------ Merge con OP 
# --- molto lavoro manuale visto che ci sono 950 parlamentari e molte incongruenze sui nomi...
# --- decido di fare il merge direttamente con gli aderenti agli intergruppi per ridurre il numero di parlamentari da controllare manualemente...

# --- nel caso volessi tornare al merge completo Camera.it + OP torno qui dove avevo cominciato (ora commento tutto)
# library(dplyr)
# df_merge <- left_join(x = OP_deputati, y = df)
# 
# # controlla merge 
# str(df)
# sum(is.na(df_merge$genere))
# sum(is.na(df_merge$numeroMandati))
# sum(is.na(df_merge$collegio))  # 87 failed merge
# 
# # -- ne controllo alcuni
# idx_fail_merge <- is.na(df_merge$collegio)
# 
# # [1]
# df[which(idx_fail_merge)[1],"parlamentare"]
# grep(pattern = "MARZANO", OP_deputati$parlamentare)
# OP_deputati[395, "parlamentare"]
# OP_deputati[395,]
# df[which(idx_fail_merge)[1],]   # e lei, quindi su dati_camera.it il nome e errato!
# 
# 
# # [2]
# df[which(idx_fail_merge)[2],"parlamentare"]
# grep(pattern = "BOLOGNESI", OP_deputati$parlamentare)
# OP_deputati[75, "parlamentare"]
# OP_deputati[395,]
# identical(OP_deputati[75, "parlamentare"], OP_deputati[395,])
# nchar(OP_deputati[75, "parlamentare"])
# nchar(OP_deputati[395,"parlamentare"])
# for(i in 1:15) {
#   
# }
# 
# df[which(idx_fail_merge)[1],]   # e lei, quindi su dati_camera.it il nome e errato!




