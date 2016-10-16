
# Creazione tabella intergruppi
# chiave: id_intergruppo

# Note:

# To be improved: manca un campo "legislatura" per organizzare gli intergruppi (o le diverse visioni dello stesso ricostituitosi più volte) per legislatura
# Le informazioni trovate sono molto frammentate ed è difficile ricostruire tale visione temporale

# a partire dal 19esimo intergruppo rilevato mi sono limitato a registrarne il nome per mancanza di tempo
# tutte le altre informazioni sono quindi mancanti (NA)



# nome completo dell'intergruppo
denominazione_completa <- 
  c("per l'innovazione tecnologica",
    "per lo sviluppo della montagna",
    "sigarette elettroniche",
    "per la Via Francigena",
    "giovani parlamentari",
    "per l'invecchiamento attivo",
    "cannabis legale",
    "finanza sostenibile",
    "per la sussidiarieta",
    "per l'agenda digitale",
    "per la cooperazione allo sviluppo",
    "per l'acqua bene comune",
    "per le donne, i diritti e le pari opportunità",
    "per il Giubileo",
    "amici del tiro, della caccia e della pesca",
    "per la musica",
    "amici del popolo Sarahwi",
    "federalista per la costituzione europea",
    "per il Tibet",
    "per l'agenda urbana",
    "amicizia col popolo rom",
    "amici della birmania",
    "amicizia Italia-Armenia",
    "per legge contro diffusione gioco d'azzardo", 
    "contro i cambiamenti climatici",
    "l'eutanasia e il testamento biologico",
    "per la pace",
    "2.0",
    "dei parlamentari evangelici",
    "di amicizia Italia - Kurdistan iracheno",
    "sui problemi sociali dell'Ictus",
    "per la tutela delle piccole imprese balneari",
    "per la mobilità ciclabile"
  )

cost <- "Intergruppo parlamentare"
denominazione_completa <- paste(cost, denominazione_completa, sep = " ")

# totale numero intergruppi recensiti
n <- length(denominazione_completa)

# nome corto intergruppo
denominazione_corta <- 
  c("innovazione",
    "montagna",
    "e-cig",
    "via francigena",
    "giovani",
    "invecchiamento attivo",
    "cannabis",
    "finanza sostenibile",
    "sussidiarieta",
    "agenda digitale",
    "cooperazione allo sviluppo",
    "acqua bene comune",
    "donne, diritti, pari opportunita",
    "giubileo",
    "tiro, caccia e pesca",
    "musica",
    "popolo sarahwi",
    "federalista europeo",
    "tibet",
    "agenda urbana",
    "popolo rom",
    "birmania",
    "armenia",
    "contro gioco d'azzardo", 
    "contro cambiamenti climatici",
    "eutanasia e testamento biologico",
    "pace",
    "2.0",
    "evangelici",
    "kurdistan iracheno",
    "problemi sociali ictus",
    "piccole imprese balneari",
    "mobilità ciclabile"
  )


# legislatura: campo da aggiungere ("XI", "XVII", ecc.)
legislatura = rep(NA,n)  

# a parziale complemento del campo legislatura inserisco l'anno prima costituzione (informazione da revisare)
# di fatto eliminando la dimensione della legislatura e trasformando la tabella in una sorta di ultima visione (ad oggi) degli intergruppi rilevati

anno_prima_costituzione <- c(
  2013,
  2013,
  NA,
  NA,
  2016,
  NA,
  NA,
  2003,
  NA,
  2013,
  NA,
  2015,
  1997,
  2008,
  2014,
  2013,
  1986,
  2002,
  rep(NA, n-18)
)


# principale associazione collegata (se esistente, altrimenti NA)
associazione_collegata <- c(
  NA,
  "UNCEM",
  "Associazione europea delle vie francigene",
  NA,
  "Young European Legislators Network",
  "HappyAgeing",
  "Associazione Luca Coscioni",
  "forum per la finanza sostenibile",
  "fondazione sussidiarieta",
  NA,
  NA,
  "Forum Italiano dei Movimenti per l'Acqua",
  NA,
  NA,
  "ANPAM",
  NA,
  NA,
  "MFE",
  rep(NA, n-18)
)


# fonte principale delle informazioni registrate
fonte = c(
  "sito ufficiale",
  "associazione collegata",
  "sito ufficiale",
  "associazione collegata",
  "sito personale parlamentare",
  "associazione collegata",
  "sito ufficiale",
  "associazione collegata",
  "associazione collegata",
  "stampa",
  "stampa",
  "associazione collegata",
  "sito istituzionale",
  "stampa",
  "associazione collegata",
  "sito personale parlamentare",
  "stampa",
  "stampa",
  rep(NA, n-18)
)


  
# numero totale parlamentari aderenti (informazione da revisare, da considerarsi come stima)  
# numeric
totale_aderenti_parlamentari <- 
  c(94, # innovazione
    127, # montagna
    11, # ecig
    37, # via francigena
    NA, # giovani 
    36,
    112, # cannabis
    46, # finanza sostenibile
    300, # sussidiarietà
    23, # agenda digitale
    80,
    80,
    NA,
    150,
    170,
    30,
    70,
    NA,
    rep(NA, n-20),
    32, # imprese balneari
    NA
  )

         
# sottotitolo del gruppo
sottotitolo <- c(
  "Deputati e Senatori impegnati per favorire lo sviluppo tecnologico dell'Italia",
  NA,
  "Sigarette elettroniche, liberi di crescere tra fisco e regolamentazione",
  "Deputati e senatori a sostegno dell'itinerario",
  "Legalizzazione della cannabis e dei suoi derivati",
  "Uno strumento innovativo per promuovere la responsabilia sociale",
  "parlamentari uniti dalla volontà di creare un'occasione di confronto e dialogo su un tema sempre più attuale come quello della sussidiarieta",
  NA,
  "strumento di lavoro comune tra deputati di diversi gruppi che vogliono impegnarsi, insieme al mondo delle ONG, per far diventare la cooperazione internazionale parte integrante della politica estera del nostro Paese",
  rep(NA, n-9 )
)


# macro-categoria
categoria <- c(
  "tecnologia",
  "territorio",
  "business",
  "territorio",
  "sociale",
  "sociale",
  "sociale",
  "economia e finanza",
  "sociale",
  "tecnologia",
  "economia e finanza",
  "sociale",
  "sociale",
  "religione",
  "territorio",
  "cultura",
  "esteri",
  "esteri",
  "esteri",
  "sociale",
  "sociale",
  "esteri",
  "esteri",
  "sociale",
  "sociale",
  "sociale",
  "esteri",
  "tecnologia",
  "religione",
  "esteri",
  "sociale",
  "business",
  "sociale"
)



# mission, attivita, propositi
attivita <- c(
  "affrontare con incisività il nostro ritardo, eliminare i digital divide, sviluppare la cultura digitale con l'obiettivo di conquistare la leadership nello sviluppo ed applicazione delle potenzialità di Internet e delle tecnologie",
  "sostegno dei territori montani e delle popolazioni in essi residenti",
  "normare, rilanciare e sviluppare un settore che può fare impresa senza nuocere alla salute",
  "promuovere e valorizzare il cammino della via Francigena",
  "lavorare insieme sui temi che più da vicino toccano la nostra generazione",
  "rappresentare i bisogni e le esigenze dei cittadini più anziani",
  "lavorare a una proposta comune, credibile e concreta da presentare alle Camere",
  "promuovere modifiche normative finalizzate a ricondurre la finanza al servizio della collettivita",
  NA,
  "individuare le priorità digitali del paese e sostenere il lavoro dei parlamentari",
  "agenda globale, e promozione della necessità di varare una serie di provvedimenti che rispondano alle nuove esigenze internazionali del Paese: dalla riforma della legge sulla cooperazione ad un piano di riallineamento delle risorse pubbliche, per invertire la rotta rispetto ai tagli drastici degli ultimi anni e rispettare pienamente gli impegni internazionali già assunti",
  "avviare un percorso legislativo per la ripubblicizzazione del servizio idrico a partire dall'aggiornamento e la riproposizione della Legge di Iniziativa Popolare presentata nel 2007 dal Forum. Altri obiettivi a breve termine quelli di contrastare la tariffa truffa elaborata dall'Aeeg in completo contrasto con i risultati referendari e quello di tutelare il diritto all'acqua dei cittadini contrastando la pratica degli stacchi all'erogazione",
  "diventare una sede di confronto bipartisan su iniziative legislative già all'esame del Parlamento e di proporne di nuove, ma anche di promuovere i temi che riguardano la vita delle donne attraverso incontri, dibattiti ed eventi culturali",
  "preparazione e divulgazione di tre mozioni di giustizia sociale internazionale, da inviare ai circa 100 intergruppi di altri paesi dei cinque continenti. tre le tematiche: riduzione del debito estero dei paesi in via di sviluppo; liberta religiosa e dignita della persona; etica e globalizzazione",
  "creare un momento di aggregazione, riflessione, approfondimento e proposta, per raggiungere l'obiettivo di promuovere in tutti i contesti istituzionali e legislativi la crescita e lo sviluppo delle attività di caccia, tiro e pesca",
  NA,
  "promuovere azione diplomatica che favorisca la ripresa dei negoziati, sotto l'egida delle Nazioni Unite, tra Regno del Marocco e Fronte Polisario, al fine di giungere nel piu' breve tempo possibile a una soluzione conforme alle risoluzioni dell'Onu e che rispetti il diritto all'autodeterminazione del popolo sahrawi",
  NA,
  rep(NA, n-20),
  "cercare di dare delle risposte concrete alle 30.000 imprese del settore, che da anni si trovano in uno stato di incertezza normativa, che ha generato confusione negli operatori e ha bloccato di fatto gli investimenti nel settore",
  NA
)


# sono pubblicati gli aderenti ?  
elenco_aderenti = c(
  TRUE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  rep(NA, n-19),
  TRUE,
  NA
)

  
# gli aderenti provengono da gruppi diversi (informazione spesso dichiarata al momento della nascita del gruppo ma verificabile solo incrociando la lista degli aderenti con i relativi gruppi di appartenenza)
aderenti_misti = c(
  NA,
  TRUE,
  NA,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  NA,
  TRUE,
  TRUE,
  NA,
  rep(NA, n-17)
)


# il gruppo ha un sito web?
sito_web <- c(
  TRUE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  FALSE
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  NA,
  NA,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  rep(NA, n-17)
)

               

# il gruppo ha una sua pagina facebook?
facebook <- c(
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  NA,
  NA,
  FALSE,
  NA,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  rep(NA, n-18)
)

  
# il gruppo ha un account twitter?
twitter <- c(
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  NA,
  NA,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  rep(NA, n-17)
)

  
# un parlamentare di riferimento (per intergruppi più antichi la persona indicata potrebbe non essere più parlamentare..)  

riferimento <- c(
  NA,
  "BORGHI Enrico",
  NA,
  "TERROSI Alessandra",
  "ASCANI Anna",
  "ROMANO Lucio",
  NA,
  "ZANIN Giorgio",
  "LUPI Maurizio",
  "COPPOLA Paolo",
  "MOGHERINI Federica",
  NA,
  "BOLDRINI Laura",
  "FUMAGALLI CARULLI Ombretta",
  "ROSSI Luciano",
  NA,
  NA,
  NA,
  rep(NA, n-18),
  "GRANAIOLA Manuela",
  NA
)

  
                 

## Creazione tabella intergruppi

intergruppi <- data.frame(
  denominazione_completa,
  denominazione_corta,
  legislatura,
  anno_prima_costituzione,
  associazione_collegata,
  fonte,
  totale_aderenti_parlamentari,
  sottotitolo,
  categoria,
  attivita,
  elenco_aderenti,
  aderenti_misti,
  sito_web,
  facebook,
  twitter,
  riferimento
  )


# riordino alfabeticamente 
intergruppi <- intergruppi[order(intergruppi$denominazione_corta),]

# id intergruupo (chiave)
id_intergruppo = sapply(1:n, function(i) paste0(ifelse(i<10, "000", ifelse(i<100, "00", "0")), i) )
intergruppi$id_intergruppo <- id_intergruppo
str(intergruppi)
intergruppi <- intergruppi[,c(17,1:16)]


tmp.env <- new.env()
assign("intergruppi", intergruppi, pos=tmp.env)
save(list=ls(all.names=TRUE, pos=tmp.env), envir=tmp.env, file="table_intergruppi_v2.RData")
rm(tmp.env)
