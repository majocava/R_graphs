install.packages("viridis")
install.packages("colorspace")
library(viridis)
library(colorspace)
data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/Cpelliculosa_update/New_annotations_masurca/PHIB/PHIB_GPI.csv")


data$`Mechanism-based classification` <- factor(data$`Mechanism-based classification`, levels=c("Metabolic flexibility/nutritional factors", "Morphology/hyphae-associated genes", "Stress response genes", "Calcium-calcineurin pathways", "Vacuole biogenesis and function", "Transcriptional regulatory networks", "Mitogen-activated protein kinase (MAPK) pathways", "Biofilm formation", "Signal transduction regulator", "Ribosomal proteins", "DNA maintenance and repair genes", "Ras/CAMP/PKA pathways"))

palette <- divergingx_hcl(12, "Spectral")

group_labels <- scale_x_discrete(labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"))

ggsave("Virulence_classification.pdf", ggplot(data, aes(y = `Number of genes`, x = `Mechanism-based classification`, fill=`Mechanism-based classification`)) + geom_bar(stat = "identity") +
  group_labels +
  scale_fill_manual(values= palette, name=NULL, 
                 labels=c(expression(plain("A: Metabolic flexibility/nutritional factors")), expression(plain("B: Morphology/hyphae-associated genes")), 
                          expression(plain("C: Stress response genes")), expression(plain("D: Calcium-calcineurin pathways")),
                          expression(plain("E: Transcriptional regulatory networks")), expression(plain("F: Vacuole biogenesis and function")),
                          expression(plain("G: Mitogen-activated protein kinase (MAPK) pathways")), expression(plain("H: Biofilm formation")),
                          expression(plain("I: DNA maintenance and repair genes")), expression(plain("J: Ribosomal proteins")),
                          expression(plain("K: Signal transduction regulator")), expression(plain("L: Ras/CAMP/PKA pathways")))) +
  theme_test() + theme(legend.text = element_text(size = 9), legend.box = "horizontal", legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 9), axis.title.x = element_text(size = 9)),
  width=8.5, height=8, dpi=300)
