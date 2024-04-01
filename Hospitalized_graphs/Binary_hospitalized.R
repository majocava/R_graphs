library (ggplot2)
library(ggbreak)
library(readr)


data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Hospitalized/Hospitalized_binary_species.csv")

data$Species <- factor(data$Species, levels=c("C. tropicalis", "C. albicans", "C. parapsilosis", "C. glabrata", "C. lusitaniae", 
                                              "C. dubliniensis", "C. krusei", "C. orthopsilosis", "C. rugosa"))
data$Source <- factor(data$Source, levels = c("CSF", "Peritoneal Wash", "Sputum", "Injury", "Blood", "Urine", "BAL/TA"))


graph <- ggplot(data, aes(x=`Species`, y=`Source`, alpha = factor(`Presence`))) +
  geom_point(size = 7, stroke = NA) +
  scale_shape_manual(values = c(1, 16))

graph + geom_segment(aes(x = "C. albicans", y = "BAL/TA", xend = "C. albicans", yend = "CSF"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. glabrata", y = "Blood", xend = "C. glabrata", yend = "BAL/TA"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. lusitaniae", y = "Blood", xend = "C. lusitaniae", yend = "Urine"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. parapsilosis", y = "BAL/TA", xend = "C. parapsilosis", yend = "Injury"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. tropicalis", y = "BAL/TA", xend = "C. tropicalis", yend = "Peritoneal Wash"), color = "black", linewidth = 1.5) +
  theme(axis.title.x = element_blank(), # Remove X axis title
        axis.title.y = element_blank(), # Remove Y axis title
        legend.position = "none") 

ggsave("Binary_plot_hospitalized.pdf", plot = graph + geom_segment(aes(x = "C. albicans", y = "BAL/TA", xend = "C. albicans", yend = "CSF"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. glabrata", y = "Blood", xend = "C. glabrata", yend = "BAL/TA"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. lusitaniae", y = "Blood", xend = "C. lusitaniae", yend = "Urine"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. parapsilosis", y = "BAL/TA", xend = "C. parapsilosis", yend = "Injury"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. tropicalis", y = "BAL/TA", xend = "C. tropicalis", yend = "Peritoneal Wash"), color = "black", linewidth = 1.5) +
         theme(axis.text.x = element_text(face = "italic"),
               axis.title.x = element_blank(), # Remove X axis title
               axis.title.y = element_blank(), # Remove Y axis title
               legend.position = "none"), 
       width=10, height= 3, dpi=300)
