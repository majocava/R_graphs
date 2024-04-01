library (ggplot2)
library(ggbreak)
library(readr)

data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Hospitalized/hospitalized_candida_isolates.csv")

data$Species <- factor(data$Species, levels=c("C. rugosa", "C. orthopsilosis", "C. krusei", "C. dubliniensis",
                                              "C. lusitaniae", "C. glabrata", "C. parapsilosis", "C. albicans", "C. tropicalis"))

data$Source <- factor(data$Source, levels = c("CSF", "Peritoneal Wash", "Sputum", "Injury", "Blood", "Urine", "BAL/TA"))

cbp2 <- c("#999999", "#FFCC99", "#009E73", "#660099", "#CC79A7", "#F0E442",
          "#0072B2", "#990000", "#D55E00")
  
ggplot(data, aes(y = `Source`, x = -`Number`, fill = `Species`)) + geom_bar(stat="identity") + scale_fill_manual(values = cbp2) +
  scale_y_discrete(name="", position = "right") + scale_x_continuous(breaks = seq(0, -40, by = -5),  # y axis values (before coord_flip) 
                                                                     labels = seq(0,  40, by =  5)) +
  ylab(NULL) + xlab("Total clinical isolates/hospitalized individuals") + theme(legend.position = "none")

ggsave("Source_graph_hospitalized.pdf", plot = ggplot(data, aes(y = `Source`, x = -`Number`, fill = `Species`)) + geom_bar(stat="identity") + scale_fill_manual(values = cbp2) +
         scale_y_discrete(name="", position = "right") + scale_x_continuous(breaks = seq(0, -40, by = -5),  # y axis values (before coord_flip) 
                                                                            labels = seq(0,  40, by =  5)) +
         ylab(NULL) + xlab("Total clinical isolates/hospitalized individuals") + theme_test() + theme(axis.text.y = element_blank(), axis.title.x = element_text(size = 8), legend.position = "none"), 
       width=5, height= 3, dpi=300)


ggplot(data[order(data$Number, decreasing = T),],
       aes(y = `Source`, x = `Number`, fill= `Species`)) + geom_bar(stat = "identity")
