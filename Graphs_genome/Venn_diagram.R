install.packages("VennDiagram")
library(sets)
library(ggplot2)
library(VennDiagram)

data <- read.csv("./data-new.csv")

data <- read_csv2("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/Cpelliculosa_update/New_annotations_masurca/PHIB/data-new.csv")

print(data$GENES)

reference_set <- subset(data, GCA_001661255.1 == 1)$GENES
chrom_set <- subset(data, GCA_019321675.1 == 1)$GENES
masurca_set <- subset(data, CpEC_UEES == 1)$GENES


venndiagram <- list(GCA_001661255.1=reference_set , CpEC_UEES=masurca_set , GCA_019321675.1=chrom_set)

colors <- list(reference_set = "red",
               chrom_set = "blue",
               masurca_set = "green")

palette <- diverging_hcl(3, "Red-Green")

ggVennDiagram(venndiagram, label_alpha = 0, fill= palette) + theme(plot.margin = margin(0, 0, 0, 0))


venn <- ggvenn(venndiagram, text_size = 5, fill_color = brewer.pal(name="Set2",n=3), auto_scale = TRUE)
ggsave("Virulence_shared.pdf", venn1, width = 8, height = 8, dpi = 300)
