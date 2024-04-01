library(ggplot2)
library(ggsignif)
library(ggbreak)
library(readr)
library(ggpattern)

sap1 <- read_csv("~/Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/3. Melanie/PICTURES/Sap1/Sap1_MEAN_SEM_copy.csv")

#Cambia la clase de la columna a factor y reordena los grupos (en este caso niveles); esto es para poner el orden en el que queremos que se grafiquen los grupos. 
sap1$Tratamiento <- factor(sap1$Tratamiento, levels=c("ATCA", "DCA53", "DCP1", "ATCA vs DCP1", "ATCA vs DCT6", "DCA53 vs DCP1", "DCA53 vs DCT6"))

#Realiza la grafica, sin las barras de error ni la estadistica. 
graph_sap1 <- ggplot(sap1, aes(x=Tratamiento, y=MEAN, fill=Strains)) + geom_bar(position=position_dodge(), stat="identity", colour="black", linewidth=.3) + xlab(NULL) + ylab("Quantity(copies/μL)") + scale_fill_hue(name=NULL, breaks=c("ATCA", "DCA53", "DCP1", "DCT6"), labels=c(expression(italic("C. albicans")~plain("(ATCA)")), expression(italic("C. albicans")~plain("(DCA53)")), expression(italic("C. parapsilosis")~plain("(DCP1)")), expression(italic("C. tropicalis")~plain("(DCT6)")))) + theme_test() + theme(legend.text = element_text(size = 12), legend.title = element_blank(), legend.key.size = unit(1, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 12)) 

#Realiza la grafica con los patrones, y la leyenda combinada
graph_sap1_pattern<-ggplot(sap1, aes(x=Tratamiento, y=MEAN, pattern=Strains)) + 
  geom_col_pattern(
    position=position_dodge2(preserve = "single"), 
    colour="black", 
    pattern_fill='black',
    pattern_spacing = 0.01,
    fill=ifelse(sap1$Strains=="DCP1", "grey34", "white")) + 
  xlab(" ") + 
  ylab("Quantity(copies/μL)") +
  scale_pattern_manual(
    name=NULL, 
    values =c("ATCA"='stripe', "DCA53"='none', "DCP1"='none'),
    labels=c(
      expression(italic("C. albicans")~plain("(ATCC)")), 
      expression(italic("C. albicans")~plain("(DCA53)")), 
      expression(italic("C. parapsilosis")~plain("(DCP1)")))
  ) + 
  scale_pattern_angle_manual(values = c("ATCA" = 45, "DCA53"=0, "DCP1" = 0)) +
  theme_test() + theme(legend.text = element_text(size = 12), legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(), legend.key.size = unit(0.8, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 10))+
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern = c("ATCA"='stripe', "DCA53"='none', "DCP1"='none'),
        fill = c("ATCA"='white', "DCA53"='white', "DCP1"='grey34')
      )
    ))
graph_sap1_pattern

#Comando para poner las barras de error (se esta graficando el SEM)
errorbar <- geom_errorbar(aes(ymin=MEAN-SEM, ymax=MEAN+SEM), width=.2, size=.5, position=position_dodge(.9))

#Establece el rango del axis y y corta el axis y 
y_range <- scale_y_continuous(limits = c(0, 14500))

#Para cambiar el nombre de los grupos del grafico de barras. 
group_labels <- scale_x_discrete(labels = c("ATCC", "DCA53", "DCP1", "A", "B", "C", "D"))

#Add significance. First install the library ggsignif
#significance between the single biofilm and competition
atca_vs_dcp1_dcp1<- geom_text(aes(x = 4.225, y = 370, label = "****"), size = 5, parse = FALSE, vjust = -1)
dca53_vs_dcp1_dcp1<- geom_text(aes(x = 6.225, y = 55, label = "****"), size = 5, parse = FALSE, vjust = -1)

competition_signif<- geom_signif(xmin=c(3.675, 5.675), xmax = c(4.325,6.325), y_position =c(420, 120), annotation=c("ns", "ns"), textsize = 5, tip_length = 0)

single_signif<- geom_signif(xmin=c(1, 2, 1), xmax= c(2, 3, 3), y_position = c(9000, 10500, 12000), annotation=c("ns", "****", "****"), textsize = 5, tip_length=0.004)


ggsave("Sap1_corr_ATCC.png", plot = graph_sap1_pattern + errorbar + scale_y_break(c(450, 7500), scales =0.4) + y_range + dca53_vs_dcp1_dcp1 + atca_vs_dcp1_dcp1 + group_labels, dpi = 300)


#si lo quieren en escala de grices: 
graph_sap1_grey <- ggplot(sap1, aes(x=Tratamiento, y=MEAN, fill=Strains)) + geom_bar(position=position_dodge(), stat="identity", colour="black", linewidth=.3) + xlab(NULL) + ylab("Quantity(copies/μL)") + scale_fill_grey(start= 0.96, end=0.2, name=NULL, breaks=c("ATCA", "DCA53", "DCP1", "DCT6"), labels=c(expression(italic("C. albicans")~plain("(ATCA)")), expression(italic("C. albicans")~plain("(DCA53)")), expression(italic("C. parapsilosis")~plain("(DCP1)")), expression(italic("C. tropicalis")~plain("(DCT6)")))) + theme_test() + theme(legend.text = element_text(size = 12), legend.title = element_blank(), legend.key.size = unit(1, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 12))

ggsave("Sap1_corr_ATCC_nodct6.png", graph_sap1_pattern + errorbar + group_labels + y_range + atca_vs_dcp1_dcp1 + dca53_vs_dcp1_dcp1 + scale_y_break(c(450, 7500), scales =0.5)+
       competition_signif + single_signif, width=8.5, height=8, dpi=300)


