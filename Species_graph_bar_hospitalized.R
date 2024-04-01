library (ggplot2)
library(ggbreak)
library(readr)

data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Hospitalized/Hospitalized_bar_species.csv")

data$Species <- factor(data$Species, levels=c("C. tropicalis", "C. albicans", "C. parapsilosis", "C. glabrata", "C. lusitaniae", 
                                              "C. dubliniensis", "C. krusei", "C. orthopsilosis", "C. rugosa"))

cbp2 <- c("#D55E00", "#990000", "#0072B2", "#F0E442", "#CC79A7", 
          "#660099", "#009E73", "#FFCC99", "#999999")


ggsave("Species_bar_graph_hospitalized.pdf", plot = ggplot(data, aes(y = `Number`, x = `Species`, fill = `Species`)) + geom_bar(stat="identity") + xlab(NULL) + ylab("Percentage of clinical isolates/hospitalized individuals") + 
  scale_fill_manual(values= cbp2, name=NULL, labels=c(expression(italic("C. tropicalis")), expression(italic("C. albicans")), expression(italic("C. parapsilosis")), expression(italic("C. glabrata")), 
                                                      expression(italic("C. lusitaniae")), expression(italic("C. dubliniensis")), expression(italic("C. krusei")), expression(italic("C. orthopsilosis")),
                                                      expression(italic("C. rugosa")))) + theme_test() + 
  theme(legend.text = element_text(size = 7.5), legend.title = element_blank(), legend.key.size = unit(0.5, "cm"), legend.text.align = 0, axis.title.y = element_text(size = 8), 
        axis.text.x = element_blank()), width=10, height= 5, dpi=300)

