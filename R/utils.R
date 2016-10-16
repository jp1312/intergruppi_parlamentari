

# Automatically adjust names in liste aderenti intergruppi to match those in OP

changeNameListaAderenti <- function(daModificare, intergruppo) {
  
  cercaMatch <- strsplit(daModificare, split = " ")
  residuo <- 0
  
  for(i in 1:length(cercaMatch)) {
    ctrl <- grep(cercaMatch[[i]][1], parlamentari$parlamentare)
    if(length(ctrl)==1) {
      cambio <- parlamentari[ctrl,"parlamentare"]
      intergruppo[intergruppo$parlamentare == daModificare[i], "parlamentare"] <- cambio
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
  
}



changeNameCameraIT <- function(daUtilizzare, datiCamera) {
  
  cercaMatch <- strsplit(daModificare, split = " ")
  residuo <- 0
  
  for(i in 1:length(cercaMatch)) {
    ctrl <- grep(cercaMatch[[i]][1], datiCamera$parlamentare)
    if(length(ctrl)==1) {
      daModificare <- datiCamera[ctrl,"parlamentare"]
      datiCamera[datiCamera$parlamentare == daModificare, "parlamentare"] <- daUtilizzare[i]
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
  
}




# from RPub

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(), 
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size= unit(0.2, "cm"),
           legend.margin = unit(0, "cm"),
           legend.title = element_text(face="italic"),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}




theme_pub <- function (base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, 
             base_family = base_family) %+replace% 
    
    theme(# Set text size
      plot.title = element_text(size = 18),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, 
                                  angle = 90),
      
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      
      strip.text.x = element_text(size = 15),
      strip.text.y = element_text(size = 15,
                                  angle = -90),
      
      # Legend text
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      
      # Configure lines and axes
      axis.ticks.x = element_line(colour = "black"), 
      axis.ticks.y = element_line(colour = "black"), 
      
      # Plot background
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey83", 
                                      size = 0.2), 
      panel.grid.minor = element_line(colour = "grey88", 
                                      size = 0.5), 
      
      # Facet labels        
      legend.key = element_rect(colour = "grey80"), 
      strip.background = element_rect(fill = "grey80", 
                                      colour = "grey50", 
                                      size = 0.2))
}





library(grid)
library(gtable)

ggplot_with_subtitle <- function(gg, 
                                 label="", 
                                 fontfamily=NULL,
                                 fontsize=10,
                                 hjust=0, vjust=0, 
                                 bottom_margin=5.5,
                                 newpage=is.null(vp),
                                 vp=NULL,
                                 col = "black",
                                 ...) {
  
  if (is.null(fontfamily)) {
    gpr <- gpar(fontsize=fontsize, col = col, ...)
  } else {
    gpr <- gpar(fontfamily=fontfamily, fontsize=fontsize, col = col, ...)
  }
  
  subtitle <- textGrob(label, x=unit(hjust, "npc"), y=unit(hjust, "npc"), 
                       hjust=hjust, vjust=vjust,
                       gp=gpr)
  data <- ggplot_build(gg)
  
  gt <- ggplot_gtable(data)
  gt <- gtable_add_rows(gt, grobHeight(subtitle), 2)
  gt <- gtable_add_grob(gt, subtitle, 3, 4, 3, 4, 8, "off", "subtitle")
  gt <- gtable_add_rows(gt, grid::unit(bottom_margin, "pt"), 3)
  
  if (newpage) grid.newpage()
  
  if (is.null(vp)) {
    grid.draw(gt)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gt)
    upViewport()
  }
  
 invisible(data)
  
}




theme_pub2 <- function() {
  theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(), 
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size= unit(0.2, "cm"),
        legend.margin = unit(0, "cm"),
        legend.title = element_text(face="italic"),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold")
  )
  
}




# this theme uses IMpact font. i need to translate it in windows first
windowsFonts(Impact=windowsFont("Impact"))

windowsFonts(Times=windowsFont("TT Times New Roman"))

# Configure Theme
kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Times", size = 15),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Times"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 20, family = "Times"), # original size = 13
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Times", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}



kobe_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Times", colour = "#552683", size = 10),
    legend.background = element_rect(fill = "#E2E2E3"),
    legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    legend.text = element_text(family = "Times", colour = "#E7A922", size = 10),
    plot.background = element_rect(fill = "white", colour = "white"), # #E2E2E3
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white", size = 15),
    axis.text = element_text(colour = "#E7A922", family = "Times", size = 12),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Times"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Times"),
    panel.grid.major.y = element_line(colour = "#E7A922"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Times", colour = "white", size = 15),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}



vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)