library(ggplot2)
library(ggsignif)
library(ggbreak)
library(readr)
library(ggpattern)

als3 <- read_csv("~/Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/3. Melanie/PICTURES/Als3/Als3_MEAN_SEM.csv")

#Cambia la clase de la columna a factor y reordena los grupos (en este caso niveles); esto es para poner el orden en el que queremos que se grafiquen los grupos. 
als3$Tratamiento <- factor(als3$Tratamiento, levels=c("ATCA", "DCA53", "DCP1", "DCT6", "ATCA vs DCP1", "ATCA vs DCT6", "DCA53 vs DCP1", "DCA53 vs DCT6", "DCP1 vs DCT6"))

#Realiza la grafica, sin las barras de error ni la estadistica. 
graph_als3 <- ggplot(als3, aes(x=Tratamiento, y=MEAN, fill=Strains)) + geom_bar(position=position_dodge2(preserve = "single"), stat="identity", colour="black", linewidth=.3) + xlab(" ") + ylab("Quantity(copies/μL)") + scale_fill_hue(name=NULL, breaks=c("ATCA", "DCA53", "DCP1", "DCT6"), labels=c(expression(italic("C. albicans")~plain("(ATCA)")), expression(italic("C. albicans")~plain("(DCA53)")), expression(italic("C. parapsilosis")~plain("(DCP1)")), expression(italic("C. tropicalis")~plain("(DCT6)")))) + theme_test() + theme(legend.text = element_text(size = 8), legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 6))

#Realiza la grafica con los patrones, y la leyenda combinada
graph_als3_pattern<-ggplot(als3, aes(x=Tratamiento, y=MEAN, pattern=Strains)) + 
  geom_col_pattern(
    position=position_dodge2(preserve = "single"), 
    colour="black", 
    pattern_fill='black',
    pattern_spacing = 0.01,
    fill=ifelse(als3$Strains=="DCP1", "grey34", "white")) + 
  xlab(" ") + 
  ylab("Quantity(copies/μL)") +
  scale_pattern_manual(
    name=NULL, 
    values =c("ATCA"='stripe', "DCA53"='none', "DCP1"='none', "DCT6"='circle'),
    labels=c(
      expression(italic("C. albicans")~plain("(ATCC)")), 
      expression(italic("C. albicans")~plain("(DCA53)")), 
      expression(italic("C. parapsilosis")~plain("(DCP1)")), 
      expression(italic("C. tropicalis")~plain("(DCT6)")))
  ) + 
  scale_pattern_angle_manual(values = c("ATCA" = 45, "DCA53"=0, "DCP1" = 0, "DCT6"= 45)) +
  theme_test() + theme(legend.text = element_text(size = 12), legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(), legend.key.size = unit(0.8, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 10))+
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern = c("ATCA"='stripe', "DCA53"='none', "DCP1"='none', "DCT6"='circle'),
        fill = c("ATCA"='white', "DCA53"='white', "DCP1"='grey34', "DCT6"='white')
      )
    ))
  
#Comando para poner las barras de error (se esta graficando el SEM)
errorbar <- geom_errorbar(aes(ymin=MEAN-SEM, ymax=MEAN+SEM), width=.1, linewidth=.4, position=position_dodge(.9))

graph_als3_pattern+errorbar

#Establece el rango del axis y. 
y_range <- scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50))

#Para cambiar el nombre de los grupos del grafico de barras. 
group_labels <- scale_x_discrete(labels = c("ATCC", "DCA53", "DCP1", "DCT6", "A", "B", "C", "D", "E"))

#Add significance. First install the library ggsignif
#significance between the single biofilm and competition
atca_vs_dcp1_dcp1<- geom_text(aes(x = 5.225, y = 131, label = "*"), size = 4, parse = FALSE, vjust = -1)
atca_vs_dct6_atca<- geom_text(aes(x = 5.775, y = 265, label = "**"), size = 4, parse = FALSE, vjust = -1)
atca_vs_dct6_dct6<- geom_text(aes(x = 6.225, y = 246, label = "**"), size = 4, parse = FALSE, vjust = -1)
dca53_vs_dct6_dct6<- geom_text(aes(x = 8.225, y = 53, label = "*"), size = 4, parse = FALSE, vjust = -1)
dcp1_vs_dct6_dcp1<- geom_text(aes(x = 8.775, y = 106, label = "**"), size = 4, parse = FALSE, vjust = -1)
dcp1_vs_dct6_dct6<- geom_text(aes(x = 9.225, y = 126, label = "**"), size = 4, parse = FALSE, vjust = -1)


#significance between the two candida strains in competition
competition_signif<- geom_signif(xmin=c(4.675, 5.675, 6.675, 7.675, 8.675), xmax = c(5.325, 6.325, 7.325, 8.325, 9.325), y_position =c(150, 281, 154, 70, 145), annotation=c("*", "ns", "ns", "*", "ns"), textsize = 5, tip_length = 0)

#significance between the single biofilms
single_signif<- geom_signif(xmin=c(1, 3, 1, 2, 2, 1), xmax= c(2, 4, 3, 4, 3, 4), y_position = c(245, 245, 270, 295, 320, 345), annotation=c("ns", "***", "***", "ns", "**", "ns"), textsize = 5, tip_length=0.004)

ggsave("Als3_corr.png", plot = graph_als3_pattern + errorbar + group_labels + y_range + atca_vs_dcp1_dcp1 + atca_vs_dct6_atca + atca_vs_dct6_dct6 + dca53_vs_dct6_dct6 + dcp1_vs_dct6_dcp1 + dcp1_vs_dct6_dct6 + competition_signif, dpi=300)

graph_als3_pattern + errorbar + group_labels + y_range + atca_vs_dcp1_dcp1 + atca_vs_dct6_atca + atca_vs_dct6_dct6 + dca53_vs_dct6_dct6 + dcp1_vs_dct6_dcp1 + dcp1_vs_dct6_dct6 + competition_signif + single_signif

ggsave("Als3_corr_ATCC.png", plot=graph_als3_pattern + errorbar + group_labels + y_range + atca_vs_dcp1_dcp1 + atca_vs_dct6_atca + atca_vs_dct6_dct6 + dca53_vs_dct6_dct6 + dcp1_vs_dct6_dcp1 + dcp1_vs_dct6_dct6 + competition_signif + single_signif,
       width=8.5, height=8, dpi=300
       )

