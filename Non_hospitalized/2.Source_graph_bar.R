library (ggplot2)
library(ggbreak)
library(readr)

data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Source_non_clinical_bar_graph.csv")

data$Species <- factor(data$Species, levels=c("C. pelliculosa", "C. haemulonii", "C. kefyr", "C. krusei", "C. glabrata", "C. parapsilosis", 
                                              "C. tropicalis", "C. albicans"))

data$Source <- factor(data$Source, levels = c("Sputum", "BAL/TA", "Vaginal Swab", "Injury", "Urine"))

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#990000")

ggplot(data, aes(y = `Source`, x = -`Number`, fill = `Species`)) + geom_bar(stat="identity") + scale_fill_manual(values = cbp2) +
  scale_y_discrete(name="", position = "right") + scale_x_continuous(breaks = seq(0, -20, by = -4),  # y axis values (before coord_flip) 
                                                                     labels = seq(0,  20, by =  4)) +
  ylab(NULL) + xlab("Total clinical isolates/non-hospitalized individuals") + theme(legend.position = "none")

ggsave("Source_graph_non_clinical.pdf", plot = ggplot(data, aes(y = `Source`, x = -`Number`, fill = `Species`)) + geom_bar(stat="identity") + scale_fill_manual(values = cbp2) +
         scale_y_discrete(name="", position = "right") + scale_x_continuous(breaks = seq(0, -20, by = -4),  # y axis values (before coord_flip) 
                                                                            labels = seq(0,  20, by =  4)) +
         ylab(NULL) + xlab("Total clinical isolates/non-hospitalized individuals") + theme_test() + theme(axis.text.y = element_blank(), axis.title.x = element_text(size = 8), legend.position = "none"), 
       width=5, height= 3, dpi=300)


ggplot(data[order(data$Number, decreasing = T),],
       aes(y = `Source`, x = `Number`, fill= `Species`)) + geom_bar(stat = "identity")
