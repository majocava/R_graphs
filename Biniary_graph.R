library (ggplot2)
library(ggbreak)
library(readr)

data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Non_hospitalized/Non_clinical_biniary.csv")

data$Species <- factor(data$Species, levels=c("C. albicans", "C. tropicalis", "C. parapsilosis", "C. glabrata", "C. krusei", 
                                              "C. haemulonii", "C. kefyr", "C. pelliculosa"))
data$Source <- factor(data$Source, levels = c("Sputum", "BAL/TA", "Vaginal Swab", "Injury", "Urine"))


graph <- ggplot(data, aes(x=`Species`, y=`Source`, alpha = factor(`Presence`))) +
  geom_point(size = 7, stroke = NA) +
  scale_shape_manual(values = c(1, 16))

graph + geom_segment(aes(x = "C. albicans", y = "Urine", xend = "C. albicans", yend = "Sputum"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. glabrata", y = "Urine", xend = "C. glabrata", yend = "Vaginal Swab"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. krusei", y = "Injury", xend = "C. krusei", yend = "Urine"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. parapsilosis", y = "Injury", xend = "C. parapsilosis", yend = "Urine"), color = "black", linewidth = 1.5) +
  geom_segment(aes(x = "C. tropicalis", y = "BAL/TA", xend = "C. tropicalis", yend = "Urine"), color = "black", linewidth = 1.5) +
  theme(axis.title.x = element_blank(), # Remove X axis title
        axis.title.y = element_blank(), # Remove Y axis title
        legend.position = "none") 

ggsave("Biniary_plot_non_clinical.png", plot = graph + geom_segment(aes(x = "C. albicans", y = "Urine", xend = "C. albicans", yend = "Sputum"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. glabrata", y = "Urine", xend = "C. glabrata", yend = "Vaginal Swab"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. krusei", y = "Injury", xend = "C. krusei", yend = "Urine"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. parapsilosis", y = "Injury", xend = "C. parapsilosis", yend = "Urine"), color = "black", linewidth = 1.5) +
         geom_segment(aes(x = "C. tropicalis", y = "BAL/TA", xend = "C. tropicalis", yend = "Urine"), color = "black", linewidth = 1.5) +
         theme(axis.text.x = element_text(face = "italic"),
               axis.title.x = element_blank(), # Remove X axis title
               axis.title.y = element_blank(), # Remove Y axis title
               legend.position = "none"), 
       width=10, height= 3, dpi=300)

