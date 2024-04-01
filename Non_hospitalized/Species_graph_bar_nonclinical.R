library (ggplot2)
library(ggbreak)
library(readr)

data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Non_clinical_bar_graph.csv")

data$Species <- factor(data$Species, levels=c("C. albicans", "C. tropicalis", "C. parapsilosis", "C. glabrata", "C. krusei", 
                                              "C. kefyr", "C. haemulonii", "C. pelliculosa"))

cbp2 <- c("#990000", "#D55E00", "#0072B2", "#F0E442", "#009E73",
          "#56B4E9", "#E69F00", "#000000")


ggsave("Species_bar_graph_non_clinical.pdf", plot = ggplot(data, aes(y = `Number`, x = `Species`, fill = `Species`)) + geom_bar(stat="identity") + xlab(NULL) + ylab("Percentage of clinical isolates/non-hospitalized individuals") + 
  scale_fill_manual(values= cbp2, name=NULL, labels=c(expression(italic("C. albicans")), expression(italic("C. tropicalis")), expression(italic("C. parapsilosis")), expression(italic("C. glabrata")), 
                                                      expression(italic("C. krusei")), expression(italic("C. kefyr")), expression(italic("C. haemulonii")), expression(italic("C. pelliculosa")))) + theme_test() + 
  theme(legend.text = element_text(size = 7.5), legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 8), 
        axis.text.x = element_blank()), width=10, height= 5, dpi=300)

