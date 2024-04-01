library (ggplot2)
library(ggbreak)
library(readr)
library(dplyr)

data <- read_csv("Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/5. Juan/Departments/Grouped_department.csv")

data$Departments <- factor(data$Departments, levels = c("Critical Care", "Medicine", "Surgery", "Observational Medicine", "Emergency", "Women's Health", "Urology", "Pediatrics", "Neurology", "Oncology"))

data$Species <- factor(data$Species, levels=c("C. rugosa", "C. pelliculosa", "C. kefyr", "C. haemulonii", "C. dubliniensis",
                                              "C. lusitaniae", "C. krusei", "C. glabrata", "C. parapsilosis", 
                                              "C. tropicalis", "C. albicans"))

cbp2 <- c("#999999", "#000000", "#E69F00", "#56B4E9", "#660099", "#CC79A7", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#990000")




ggsave("Grouped_department.pdf", plot = ggplot(data, aes(y = `Percentage_total`, x = `Departments`, fill = `Species`)) +
  geom_bar(stat="identity") + guides(fill = guide_legend(reverse = TRUE)) +  
  scale_fill_manual(values= cbp2, name=NULL, labels=c(expression(italic("C. rugosa")), expression(italic("C. pelliculosa")), expression(italic("C. kefyr")), expression(italic("C. haemulonii")), 
                                                      expression(italic("C. dubliniensis")), expression(italic("C. lusitaniae")), expression(italic("C. krusei")), 
                                                      expression(italic("C. glabrata")), expression(italic("C. parapsilosis")), expression(italic("C. tropicalis")), 
                                                      expression(italic("C. albicans")))) +
  scale_y_continuous(breaks = seq(0, 40, by = 5),
                     labels = seq(0, 40, by = 5)) +
  ylab("Percentage of clinical isolates") + xlab("Medical departments") + theme_test() + theme(axis.title.y = element_text(size = 9), 
                                                                                               axis.title.x = element_text(size = 9)),
  width=13, height= 5, dpi=300)
